// -----------------------------------------------------------------------------
// weatherReader: reads daily or hourly weather CSV files and synthesizes
//                hourly weather time series when needed.
//
// Key ideas:
// - Robust header parsing: supports multiple aliases for common column names.
// - Flexible date handling: either a single Date/DateTime column or separate Y/M/D (and Hour).
// - Radiation handling:
//     * If measured daily/hourly radiation is provided, use it.
//     * Otherwise, estimate daily GSR (global solar radiation) from latitude and
//       diurnal temperature range (Hargreaves-like) and distribute it hourly
//       using the clear-sky fraction (extraterrestrial radiation).
// - Humidity handling:
//     * If hourly/daily RH not provided, approximate via dew point or a
//       smooth daily RH curve bounded by RHmin/RHmax.
//
// Assumptions & units (unless stated otherwise):
//   Temperature: °C
//   Precipitation: mm d⁻¹ (daily) or mm h⁻¹ (hourly)
//   Radiation (Rad, GSR): MJ m⁻² d⁻¹ (daily) or MJ m⁻² h⁻¹ (hourly)
//   Latitude: decimal degrees (positive North)
//   Relative humidity: %
// -----------------------------------------------------------------------------

using System.Globalization;

public class weatherReader
{
    // Reads daily weather input from CSV and returns synthesized hourly weather data.
    // Header is parsed once to detect column indices by alias; data rows are then read and converted.
    //
    // Expected columns (aliases are supported; see arrays below):
    //   Either: Year, Month, Day             OR: Date/Datetime/Timestamp
    //   Radiation (optional) or Latitude (required if radiation is missing)
    //   Tmax, Tmin
    //   Precipitation (optional)
    //   RHx, RHn (optional: daily RH max/min, used for a smooth hourly RH curve)
    //
    // Rows falling outside [startYear, endYear] are skipped.
    public Dictionary<DateTime, inputsHourly> readDaily(string file, int startYear, int endYear)
    {
        var gridWeathersHourly = new Dictionary<DateTime, inputsHourly>();

        using (var sr = new StreamReader(new BufferedStream(new FileStream(file, FileMode.Open))))
        {
            // --- Header parsing ------------------------------------------------
            // Determine delimiter (tab or comma), normalize header tokens, build name→index map.
            string headerLine = sr.ReadLine();
            char delimiter = headerLine.Contains("\t") ? '\t' : ',';
            string[] headers = headerLine.Split(delimiter);

            var colIndex = headers
                .Select((h, i) => new { Name = CleanColumnName(RemoveQuotes(h)), Index = i })
                .ToDictionary(x => x.Name, x => x.Index, StringComparer.OrdinalIgnoreCase);

            // Column aliases: accept multiple synonymous names commonly seen in datasets.
            string[] dateAliases = { "date", "datetime", "timestamp" };
            string[] yearAliases = { "year" };
            string[] monthAliases = { "month" };
            string[] dayAliases = { "day" };
            string[] radAliases = { "rad", "radiation", "solar", "solarrad", "srad" };
            string[] latAliases = { "lat", "latitude", "site_lat", "phi" };
            string[] tmaxAliases = { "tmax", "t2mmax", "maxtemp", "tx" };
            string[] tminAliases = { "tmin", "t2mmin", "mintemp", "tn" };
            string[] precAliases = { "prec", "precip", "rain", "rainfall", "precipitation", "p" };
            string[] rhxAliases = { "rhmax", "humiditymax", "relativehumiditymax", "hummax", "rhx" };
            string[] rhnAliases = { "rhmin", "humiditymin", "relativehumiditymin", "hummin", "rhn" };

            // Resolve date/time columns: either separate Y/M/D or a single Date column.
            int dateIdx = GetColumnIndex(colIndex, dateAliases, optional: true);
            int yearIdx = GetColumnIndex(colIndex, yearAliases, optional: true);
            int monthIdx = GetColumnIndex(colIndex, monthAliases, optional: true);
            int dayIdx = GetColumnIndex(colIndex, dayAliases, optional: true);

            bool hasYMD = (yearIdx >= 0 && monthIdx >= 0 && dayIdx >= 0);
            bool hasDate = (dateIdx >= 0);

            if (!hasYMD && !hasDate)
                throw new Exception("❌ Missing date columns: provide either (year, month, day) or a single 'date'/'datetime'.");

            // Required/optional meteorological columns
            int radIdx = GetColumnIndex(colIndex, radAliases, optional: true);
            int latIdx = GetColumnIndex(colIndex, latAliases, optional: true);
            int tmaxIdx = GetColumnIndex(colIndex, tmaxAliases);
            int tminIdx = GetColumnIndex(colIndex, tminAliases);
            int precIdx = GetColumnIndex(colIndex, precAliases, optional: true);
            int rhxIdx = GetColumnIndex(colIndex, rhxAliases, optional: true);
            int rhnIdx = GetColumnIndex(colIndex, rhnAliases, optional: true);

            // File-level guard: we need either Radiation (Rad) or Latitude (to estimate Rad).
            if (radIdx < 0 && latIdx < 0)
                throw new Exception("☠: Weather file must contain either a radiation column or a latitude column.");

            // --- Row parsing ---------------------------------------------------
            while (!sr.EndOfStream)
            {
                string raw = sr.ReadLine();
                if (string.IsNullOrWhiteSpace(raw)) continue;

                // Note: this split does not handle quoted commas; ensure the input is simple CSV/TSV.
                string[] line = raw.Split(delimiter);
                if (line.Length == 0) continue;

                // --- Date -------------------------------------------------------
                DateTime date;
                if (hasYMD)
                {
                    int y = int.Parse(RemoveQuotes(line[yearIdx]));
                    int m = int.Parse(RemoveQuotes(line[monthIdx]));
                    int d = int.Parse(RemoveQuotes(line[dayIdx]));
                    date = new DateTime(y, m, d);
                }
                else
                {
                    // Parse with invariant culture to reduce locale dependence; keep only the date part.
                    date = DateTime.Parse(RemoveQuotes(line[dateIdx]), CultureInfo.InvariantCulture).Date;
                }
                // Filter by year window
                if (date.Year < startYear || date.Year > endYear) continue;

                // --- Temperatures ----------------------------------------------
                float tmax = ParseFloat(line, tmaxIdx);
                float tmin = ParseFloat(line, tminIdx);

                // --- Try measured daily radiation ------------------------------
                float rad = (radIdx >= 0 ? ParseFloat(line, radIdx) : float.NaN);
                bool radMissing = (radIdx < 0) || float.IsNaN(rad) || float.IsInfinity(rad) || rad <= 0f;

                // --- Estimate daily radiation if missing -----------------------
                if (radMissing)
                {
                    // Need latitude to compute extraterrestrial radiation and day length
                    float latDeg = (latIdx >= 0 ? ParseFloat(line, latIdx) : float.NaN);
                    if (float.IsNaN(latDeg) || float.IsInfinity(latDeg))
                        throw new Exception($"☠: Row {date:yyyy-MM-dd} has neither usable radiation nor latitude.");

                    // Estimate daily GSR via Hargreaves-like relationship, then distribute hourly
                    var hourlyStub = new inputsHourly { date = date, latitude = latDeg };
                    var rd = dayLength(hourlyStub, tmax, tmin);
                    rad = rd.gsr;

                    // If estimation failed or produced non-positive values, skip this day
                    if (float.IsNaN(rad) || float.IsInfinity(rad) || rad <= 0f) continue;
                }

                // --- Build daily record ----------------------------------------
                var id = new inputsDaily
                {
                    date = date,
                    Rad = rad,
                    Tmax = tmax,
                    Tmin = tmin,
                    DewPoint = 0f,              // computed next
                    Precipitation = (precIdx >= 0 ? ParseFloat(line, precIdx) : 0f)
                };
                id.DewPoint = dewPoint(id.Tmax, id.Tmin); // empirical dew point estimate

                // Optional daily RH extrema to shape the hourly RH curve
                if (rhxIdx >= 0) id.RHx = ParseFloat(line, rhxIdx);
                if (rhnIdx >= 0) id.RHn = ParseFloat(line, rhnIdx);

                // --- Daily → Hourly synthesis (cosine curves + simple splits) ---
                gridWeathersHourly.AddRange(estimateHourly(id, date));
            }
        }

        return gridWeathersHourly;
    }

    // Reads HOURLY weather CSV. If hourly radiation is missing (or zero) for a day
    // but latitude is available, it estimates daily GSR and distributes it to hours
    // using clear-sky fractions (extraterrestrial radiation), then fills per-hour.
    public Dictionary<DateTime, inputsHourly> readHourly(string file, int startYear, int endYear, string site)
    {
        // Collect all hourly rows, grouped by calendar date; later we can post-process per-day.
        var gridWeathersHourly = new Dictionary<DateTime, inputsHourly>();
        var dailyRecords = new Dictionary<DateTime, List<inputsHourly>>();

        using (var sr = new StreamReader(new BufferedStream(new FileStream(file, FileMode.Open))))
        {
            // --- Header parsing & delimiter -----------------------------------
            string headerLine = sr.ReadLine();
            char delimiter = headerLine.Contains("\t") ? '\t' : ',';
            string[] headers = headerLine.Split(delimiter);

            // Normalize header names (lowercase, strip spaces/underscores) and map to indices.
            var normCols = headers
                .Select((h, i) => new { Name = Clean(RemoveQuotes(h)), Index = i })
                .GroupBy(x => x.Name, StringComparer.OrdinalIgnoreCase)
                .ToDictionary(g => g.Key, g => g.First().Index, StringComparer.OrdinalIgnoreCase);

            // Aliases for expected columns
            string[] yearA = { "year" };
            string[] monthA = { "month", "mo" };
            string[] dayA = { "day", "dd", "dy" };
            string[] hourA = { "hour", "hr", "h" };
            string[] dateA = { "date", "datetime", "timestamp" };

            string[] tmaxA = { "tmax", "t2mmax", "maxtemp" };
            string[] tminA = { "tmin", "t2mmin", "mintemp" };
            string[] tempA = { "temp", "temperature", "t2m" };
            string[] precA = { "prec", "precip", "precipitation", "prectotcorr", "rain", "rainfall" };
            string[] rhA = { "rh", "humidity", "relhumidity", "relativehumidity" };
            string[] radA = { "rad", "radiation", "solar", "solarrad" };
            string[] latA = { "latitude", "lat" };

            // Either Y/M/D/H or Date/H must be present
            int yearIdx = GetIdx(normCols, yearA, optional: true);
            int monthIdx = GetIdx(normCols, monthA, optional: true);
            int dayIdx = GetIdx(normCols, dayA, optional: true);
            int dateIdx = GetIdx(normCols, dateA, optional: true);
            int hourIdx = GetIdx(normCols, hourA, optional: true);

            bool hasYMDH = (yearIdx >= 0 && monthIdx >= 0 && dayIdx >= 0 && hourIdx >= 0);
            bool hasDateH = (dateIdx >= 0 && hourIdx >= 0);

            if (!hasYMDH && !hasDateH)
                throw new Exception("❌ Missing date-time columns: need (year,month,day,hour) or (date,hour).");

            // Meteorological columns (all optional here; we’ll compute/approximate where possible)
            int tmaxIdx = GetIdx(normCols, tmaxA, optional: true);
            int tminIdx = GetIdx(normCols, tminA, optional: true);
            int tempIdx = GetIdx(normCols, tempA, optional: true);
            int precIdx = GetIdx(normCols, precA, optional: true);
            int rhIdx = GetIdx(normCols, rhA, optional: true);
            int radIdx = GetIdx(normCols, radA, optional: true);
            int latIdx = GetIdx(normCols, latA, optional: true);

            // File-level guard: we need either hourly Rad or a Latitude to estimate it
            if (radIdx < 0 && latIdx < 0)
                throw new Exception("☠: Weather file must contain either a radiation column or a latitude column.");

            // --- Row parsing ---------------------------------------------------
            while (!sr.EndOfStream)
            {
                string[] line = sr.ReadLine().Split(delimiter);
                if (line.Length == 0) continue;

                // Timestamp: derive DateTime from columns
                DateTime ts;
                if (hasYMDH)
                {
                    int y = int.Parse(RemoveQuotes(line[yearIdx]));
                    int m = int.Parse(RemoveQuotes(line[monthIdx]));
                    int d = int.Parse(RemoveQuotes(line[dayIdx]));
                    int h = int.Parse(RemoveQuotes(line[hourIdx]));
                    ts = new DateTime(y, m, d, h, 0, 0, DateTimeKind.Unspecified);
                }
                else
                {
                    var d = DateTime.Parse(RemoveQuotes(line[dateIdx]), CultureInfo.InvariantCulture);
                    int h = int.Parse(RemoveQuotes(line[hourIdx]));
                    ts = d.Date.AddHours(h);
                }

                // Year filter
                if (ts.Year < startYear || ts.Year > endYear) continue;

                var gw = new inputsHourly { date = ts };

                // Temperature: prefer direct hourly temperature; otherwise fallback to daily (Tmax/Tmin) mean.
                if (tempIdx >= 0 && TryParseFloat(RemoveQuotes(line[tempIdx]), out float t))
                {
                    gw.airTemperature = t;
                }
                else if (tmaxIdx >= 0 && tminIdx >= 0 &&
                         TryParseFloat(RemoveQuotes(line[tmaxIdx]), out float tmax) &&
                         TryParseFloat(RemoveQuotes(line[tminIdx]), out float tmin))
                {
                    gw.airTemperature = 0.5f * (tmax + tmin);
                }

                // Precipitation (if available)
                if (precIdx >= 0 && TryParseFloat(RemoveQuotes(line[precIdx]), out float p))
                    gw.precipitation = p;

                // Relative humidity:
                // - If provided, clamp to [0,100]
                // - If missing, approximate via dew-point method using T as both Tmax and Tmin (neutral fallback)
                if (rhIdx >= 0 && TryParseFloat(RemoveQuotes(line[rhIdx]), out float rh))
                {
                    gw.relativeHumidity = Math.Clamp(rh, 0f, 100f);
                }
                else
                {
                    float T = gw.airTemperature;
                    float dp = dewPoint(T, T);
                    float es = 0.61121f * (float)Math.Exp((17.502f * T) / (240.97f + T));
                    float ea = 0.61121f * (float)Math.Exp((17.502f * dp) / (240.97f + dp));
                    gw.relativeHumidity = Math.Clamp(ea / es * 100f, 0f, 100f);
                }

                // Radiation: keep if provided; otherwise 0 for now (to be backfilled per day if possible)
                gw.rad = (radIdx >= 0 && TryParseFloat(RemoveQuotes(line[radIdx]), out float radh)) ? radh : 0f;

                // Latitude (required later for radiation estimation if rad is missing)
                if (latIdx >= 0 && TryParseFloat(RemoveQuotes(line[latIdx]), out float lat))
                    gw.latitude = lat;

                // Leaf wetness: simple rule combining high RH and/or measurable precipitation
                gw.leafWetness = (gw.relativeHumidity > 90f || gw.precipitation >= 0.2f) ? 1 : 0;

                // Group by calendar day for possible per-day radiation estimation
                DateTime dayKey = ts.Date;
                if (!dailyRecords.TryGetValue(dayKey, out var list))
                {
                    list = new List<inputsHourly>();
                    dailyRecords[dayKey] = list;
                }
                list.Add(gw);
            }
        }

        // --- Post-process per day to fill missing hourly radiation ----------------
        foreach (var kvp in dailyRecords)
        {
            var day = kvp.Key;
            var records = kvp.Value;
            if (records.Count == 0) continue;

            float tmin = records.Min(r => r.airTemperature);
            float tmax = records.Max(r => r.airTemperature);

            // Latitude from the first hourly record of the day (assumed constant per site/day)
            float lat = records.First().latitude;
            bool hasLat = !float.IsNaN(lat) && !float.IsInfinity(lat);

            // If no hourly radiation and no latitude, we cannot estimate
            if (records.All(r => r.rad <= 0f) && !hasLat)
                throw new Exception($"☠: Day {day:yyyy-MM-dd} has neither usable radiation nor latitude for estimation.");

            // Compute daily extraterrestrial radiation and hour fractions (if we have latitude)
            var sample = records.First();
            sample.latitude = lat;
            var rd = hasLat ? dayLength(sample, tmax, tmin) : null;

            foreach (var rec in records)
            {
                int h = rec.date.Hour;

                // If radiation is missing/non-positive, fill from daily GSR distributed by clear-sky fraction
                if (rec.rad <= 0f && rd?.gsrHourly != null && h >= 0 && h < rd.gsrHourly.Length)
                    rec.rad = rd.gsrHourly[h];

                // Re-evaluate leaf wetness after radiation fill (keeps the same rule)
                rec.leafWetness = (rec.relativeHumidity > 90f || rec.precipitation >= 0.2f) ? 1 : 0;

                // Store the hourly record if not already present
                if (!gridWeathersHourly.ContainsKey(rec.date))
                    gridWeathersHourly.Add(rec.date, rec);
            }
        }

        return gridWeathersHourly;
    }

    // Normalize header names for lookups: trim, lower, strip spaces/underscores.
    private static string Clean(string name) =>
        RemoveQuotes(name).Trim().ToLowerInvariant().Replace(" ", "").Replace("_", "");

    // Get column index by any of the provided aliases (normalized). Throws if required and not found.
    private static int GetIdx(Dictionary<string, int> cols, string[] aliases, bool optional = false)
    {
        foreach (var a in aliases)
        {
            string key = Clean(a);
            if (cols.TryGetValue(key, out int idx))
                return idx;
        }
        if (!optional) throw new Exception($"❌ Missing required column(s): {string.Join("/", aliases)}");
        return -1;
    }

    // Float parse with invariant culture; returns false if parsing fails.
    private static bool TryParseFloat(string s, out float v) =>
        float.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture, out v);

    // Container for daily/hourly radiation and solar geometry
    public class radData
    {
        public float latitude { get; set; }         // decimal degrees (positive North)
        public float etr { get; set; }              // daily extraterrestrial solar radiation, MJ m⁻² d⁻¹
        public float dayLength { get; set; }        // day length (hours)
        public float dayLengthYesterday { get; set; } // not used here (reserved)
        public float hourSunrise { get; set; }      // local hour of sunrise (solar-time approximation)
        public float hourSunset { get; set; }       // local hour of sunset  (solar-time approximation)
        public float gsr { get; set; }              // daily global solar radiation, MJ m⁻² d⁻¹

        public float[] etrHourly = new float[24];   // hourly extraterrestrial radiation, MJ m⁻² h⁻¹
        public float[] gsrHourly = new float[24];   // hourly global solar radiation, MJ m⁻² h⁻¹ (estimated)
    }

    // Compute solar geometry for a given day and latitude, estimate daily GSR if missing,
    // and distribute it across hours using the clear-sky (ETR) hourly fraction.
    public radData dayLength(inputsHourly input, float Tmax, float Tmin)
    {
        radData _radData = new radData();

        // 1) Ensure latitude is supplied
        _radData.latitude = input.latitude;

        // 2) Temperature checks (used for Hargreaves-like daily GSR estimate)
        if (!float.IsFinite(Tmax) || !float.IsFinite(Tmin)) { _radData.gsr = float.NaN; return _radData; }
        if (Tmax < Tmin) { var tmp = Tmax; Tmax = Tmin; Tmin = tmp; }

        // Constants and conversions
        float solarConstant = 4.921f;         // MJ m⁻² h⁻¹ (≈ 1367 W m⁻² converted to MJ/h/m²)
        float DtoR = (float)Math.PI / 180f;   // degrees → radians

        int doy = input.date.DayOfYear;
        float latitudeRad = _radData.latitude * DtoR;

        // Basic solar geometry (day-of-year based)
        float inverseEarthSun = 1f + 0.0334f * (float)Math.Cos(0.01721f * doy - 0.0552f);
        float solarDeclination = 0.4093f * (float)Math.Sin((6.284f / 365f) * (284 + doy));
        float sinDec = (float)Math.Sin(solarDeclination);
        float cosDec = (float)Math.Cos(solarDeclination);
        float sinLat = (float)Math.Sin(latitudeRad);
        float cosLat = (float)Math.Cos(latitudeRad);
        float ss = sinDec * sinLat;  // helper
        float cc = cosDec * cosLat;  // helper

        // Sunset hour angle; clamp acos argument for numerical stability
        float wsArg = -(float)(Math.Tan(solarDeclination) * Math.Tan(latitudeRad));
        wsArg = Math.Max(-1f, Math.Min(1f, wsArg));
        float ws = (float)Math.Acos(wsArg);   // radians

        // Arrays for hourly extraterrestrial radiation and hourly GSR distribution
        _radData.etrHourly = new float[24];
        _radData.gsrHourly = new float[24];

        // Hourly extraterrestrial radiation (clear-sky proxy for distribution)
        _radData.etr = 0f;
        int dayHours = 0;
        for (int h = 0; h < 24; h++)
        {
            float hourAngleDeg = 15f * (h - 12f);                    // solar hour angle ~ 15° per hour
            float sinElev = ss + cc * (float)Math.Cos(DtoR * hourAngleDeg); // sin of solar elevation
            sinElev = Math.Max(0f, sinElev);                          // nighttime → 0

            float hourlyETR = solarConstant * inverseEarthSun * sinElev; // MJ m⁻² h⁻¹
            _radData.etrHourly[h] = hourlyETR;
            _radData.etr += hourlyETR;
            if (hourlyETR > 0f) dayHours++;
        }

        // Analytical daily Ra (preferred) and day length when |lat| < 65° (avoid polar issues)
        if (_radData.latitude < 65 && _radData.latitude > -65)
        {
            _radData.dayLength = (24f / (float)Math.PI) * ws; // hours
            float Ra_analytical = (24f / (float)Math.PI) * solarConstant * inverseEarthSun *
                                  (ws * ss + cc * (float)Math.Sin(ws));
            _radData.etr = Ra_analytical; // override sum-of-hourly for stability
        }
        else
        {
            // At high latitudes, use count of “sunlit” hours from hourly ETR > 0
            _radData.dayLength = dayHours;
        }

        // 3) Estimate daily GSR if missing/non-positive using Hargreaves-like formula
        bool missingGsr = !float.IsFinite(_radData.gsr) || _radData.gsr <= 0f;
        if (missingGsr)
        {
            const float kRs = 0.19f;                    // coastal default; 0.16 for inland is common
            float td = MathF.Max(0f, Tmax - Tmin);      // diurnal temperature range
            float Rs = kRs * MathF.Sqrt(td) * _radData.etr;  // MJ m⁻² d⁻¹
            Rs = MathF.Max(0f, MathF.Min(Rs, _radData.etr)); // clip to [0, Ra]
            _radData.gsr = MathF.Round(Rs, 2);
        }

        // 4) Distribute daily GSR into hourly values by clear-sky fraction
        for (int h = 0; h < 24; h++)
        {
            float frac = _radData.etr > 0f ? _radData.etrHourly[h] / _radData.etr : 0f;
            _radData.gsrHourly[h] = frac * _radData.gsr;
        }

        // Approximate sunrise/sunset around solar noon = 12:00
        _radData.hourSunrise = 12f - _radData.dayLength / 2f;
        _radData.hourSunset = 12f + _radData.dayLength / 2f;
        return _radData;
    }

    // --- Low-level helpers ----------------------------------------------------

    // Remove surrounding double quotes from a token (simple CSV hygiene).
    private static string RemoveQuotes(string s)
    {
        if (string.IsNullOrWhiteSpace(s)) return s;
        s = s.Trim();
        if (s.StartsWith("\"") && s.EndsWith("\""))
            return s.Substring(1, s.Length - 2);
        return s;
    }

    // Normalize a header token for alias matching (lowercase, strip spaces/underscores).
    private static string CleanColumnName(string name) =>
        name.Trim().ToLowerInvariant().Replace(" ", "").Replace("_", "");

    // Get column index from name→index map by trying aliases. Throws if required and not found.
    private static int GetColumnIndex(Dictionary<string, int> cols, string[] aliases, bool optional = false)
    {
        foreach (var alias in aliases)
        {
            string clean = CleanColumnName(alias);
            if (cols.TryGetValue(clean, out int idx))
                return idx;
        }
        if (!optional)
            throw new Exception($"❌ Missing required column(s): {string.Join("/", aliases)}");
        return -1;
    }

    // Parse a float from a row at a given index using invariant culture.
    private static float ParseFloat(string[] row, int idx) =>
        float.Parse(RemoveQuotes(row[idx]), NumberStyles.Float, CultureInfo.InvariantCulture);

    // --- Meteorological helper functions -------------------------------------

    // Empirical dew point estimate from daily max/min temperature (simple quadratic form).
    private float dewPoint(float tmax, float tmin)
    {
        return 0.38F * tmax - 0.018F * (float)Math.Pow(tmax, 2) + 1.4F * tmin - 5F;
    }

    // Smooth hourly RH curve given daily RH min/max:
    // Cosine interpolation with driest hour around 14:00 (typical mid-afternoon minimum).
    private float EstimateHourlyRH(float rhMin, float rhMax, int hour)
    {
        double radians = Math.PI * (hour - 14) / 12.0;
        double rh = rhMin + (rhMax - rhMin) * 0.5 * (1 + Math.Cos(radians));
        return (float)Math.Max(0, Math.Min(100, rh));
    }

    // Daily → Hourly synthesis using simple diurnal shapes:
    // - Temperature: cosine wave centered near 15:00 (afternoon peak).
    // - RH: from dew point (if no RHx/RHn) or from a smooth RHmin/RHmax curve.
    // - Precipitation: uniform split across hours (very simple assumption).
    // - Radiation: uniform split across hours (for daily inputs only; when hourly file is used,
    //   radiation is handled via dayLength() for better realism).
    public Dictionary<DateTime, inputsHourly> estimateHourly(inputsDaily inputDaily, DateTime date)
    {
        Dictionary<DateTime, inputsHourly> gridWeathers = new Dictionary<DateTime, inputsHourly>();

        float avgT = (inputDaily.Tmax + inputDaily.Tmin) / 2f; // mean temperature
        float dailyRange = inputDaily.Tmax - inputDaily.Tmin;       // amplitude
        float dewPoint = inputDaily.DewPoint;
        float rain = inputDaily.Precipitation;
        float rad = inputDaily.Rad;
        float rhx = inputDaily.RHx; // RH max (optional)
        float rhn = inputDaily.RHn; // RH min (optional)

        for (int i = 0; i < 24; i++)
        {
            var gw = new inputsHourly { date = date.AddHours(i) };

            // Temperature: cosine diurnal cycle; 0.2618 ≈ π/12 to yield 24-hour periodicity.
            float hourlyT = (float)(avgT + dailyRange / 2 * Math.Cos(0.2618f * (i - 15)));
            gw.airTemperature = hourlyT;

            // Relative humidity:
            // - If no daily RH extrema were provided, compute from dew point (capped at 100%).
            // - Else, use a bounded cosine interpolation between RHmin and RHmax.
            float rh_hour;
            if (rhx == 0)
            {
                float es = 0.61121f * (float)Math.Exp((17.502f * hourlyT) / (240.97f + hourlyT));
                float ea = 0.61121f * (float)Math.Exp((17.502f * dewPoint) / (240.97f + dewPoint));
                rh_hour = Math.Min(100f, ea / es * 100f);
            }
            else
            {
                rh_hour = EstimateHourlyRH(rhn, rhx, i);
            }
            gw.relativeHumidity = rh_hour;

            // Simple precipitation split (mm d⁻¹ → mm h⁻¹)
            gw.precipitation = rain / 24f;

            // Leaf wetness: rule-of-thumb threshold on RH and precipitation
            gw.leafWetness = (gw.precipitation >= 0.2f || gw.relativeHumidity >= 90f) ? 1 : 0;

            // Radiation: uniform split (daily total / 24). For more realism, prefer readHourly + dayLength().
            gw.rad = rad / 24f;

            gridWeathers.Add(gw.date, gw);
        }

        return gridWeathers;
    }
}

#region utility
// DictionaryExtensions: helper to add many entries at once with optional overwrite.
// Useful when merging per-day synthesized hours into a single dictionary.
public static class DictionaryExtensions
{
    // Adds a range of key-value pairs to a dictionary.
    // If overwriteExisting is true, existing keys are replaced; otherwise they are kept.
    public static void AddRange<TKey, TValue>(this Dictionary<TKey, TValue> target, IEnumerable<KeyValuePair<TKey, TValue>> source, bool overwriteExisting = false)
    {
        if (target == null)
            throw new ArgumentNullException(nameof(target));
        if (source == null)
            throw new ArgumentNullException(nameof(source));

        foreach (var kvp in source)
        {
            if (overwriteExisting || !target.ContainsKey(kvp.Key))
            {
                target[kvp.Key] = kvp.Value;
            }
        }
    }
}
#endregion
