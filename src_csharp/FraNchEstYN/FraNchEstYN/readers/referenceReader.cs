using System.Globalization;
using Microsoft.VisualBasic.FileIO;

namespace runner
{
    //this class reads the reference data 
    internal class referenceReader
    {
        //this method reads the sowing data from the .csv file
        // ---- Public API ---------------------------------------------------------
        // Reads sowing data + fungicide schedule for a given site/variety.
        internal simulationUnit readSowing(string sowingFile, string site, string variety, int startYear, int endYear)
        {
            var sim = new simulationUnit();

            using var sr = new StreamReader(sowingFile);
            var header = sr.ReadLine();
            if (header == null) return sim;

            var cols = header.Split(',').Select(h => h.Trim()).ToArray();
            int cSite = Need(cols, "site");
            int cCrop = Need(cols, "crop");
            int cVariety = Need(cols, "variety");
            int cSowingDOY = Need(cols, "sowingDOY");
            int cYear = Need(cols, "year");

            int[] cFung = cols
                .Select((h, i) => (h, i))
                .Where(t => t.h.StartsWith("treatment", StringComparison.OrdinalIgnoreCase))
                .Select(t => t.i)
                .ToArray();

            Row? allRow = null;
            var perYear = new Dictionary<int, Row>();

            for (string line; (line = sr.ReadLine()) != null;)
            {
                if (string.IsNullOrWhiteSpace(line)) continue;
                var p = line.Split(',').Select(s => s.Trim()).ToArray();
                if (p.Length < cols.Length) continue;

                if (!Eq(p[cSite], site) || !Eq(p[cVariety], variety)) continue;

                sim.crop = p[cCrop];
                sim.variety = p[cVariety];

                if (!int.TryParse(p[cSowingDOY], NumberStyles.Integer, CultureInfo.InvariantCulture, out int sowDOY))
                    continue;

                var fungDOY = cFung.Where(i => i < p.Length)
                                   .Select(i => SafeInt(p[i]))
                                   .Where(d => d > 0)
                                   .Distinct()
                                   .OrderBy(d => d)
                                   .ToList();

                var yearCell = p[cYear];
                if (Eq(yearCell, "All"))
                {
                    allRow = new Row { SowingDOY = sowDOY, FungDOY = fungDOY };
                }
                else if (int.TryParse(yearCell, out int y))
                {
                    perYear[y] = new Row { SowingDOY = sowDOY, FungDOY = fungDOY };
                }
            }

            // Apply "All" to all years
            if (allRow.HasValue)
            {
                for (int y = startYear; y < endYear; y++)
                {
                    ApplyRow(sim, y, allRow.Value);
                }
            }

            // Override for specific years
            foreach (var (y, row) in perYear)
            {
                ApplyRow(sim, y, row);
            }

            return sim;
        }

        // ----------------- Helpers -------------------
        private static void ApplyRow(simulationUnit sim, int year, Row row)
        {
            int sowDOY = row.SowingDOY;

            // semina sempre nell’anno specificato
            sim.year_sowingDOY[year] = sowDOY;

            // calcola le date dei trattamenti e le aggiunge alla lista
            foreach (var fDOY in row.FungDOY)
            {
                int treatYear = (fDOY < sowDOY) ? year + 1 : year;
                var date = new DateTime(treatYear, 1, 1).AddDays(fDOY - 1);
                sim.fungicideTreatmentSchedule.AddTreatment(date);
            }
        }

        private static bool Eq(string a, string b) =>
            string.Equals(a?.Trim(), b?.Trim(), StringComparison.OrdinalIgnoreCase);

        private static int Need(string[] headers, string name)
        {
            int i = Array.FindIndex(headers, h => string.Equals(h, name, StringComparison.OrdinalIgnoreCase));
            if (i < 0) throw new InvalidDataException($"Colonna mancante: {name}");
            return i;
        }

        private static int SafeInt(string s) =>
            int.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture, out var v) ? v : 0;

        private struct Row
        {
            public int SowingDOY;     // sowing DOY
            public List<int> FungDOY; // DOY of fungicide treatment
        }


        internal simulationUnit readReference(
     string Reffile, string sowingFile, string site, string variety,
     int startYear, int endYear, ref simulationUnit experiment_simUnit, string disease)
        {
            // Ensure sowings loaded
            if (experiment_simUnit.year_sowingDOY.Keys.Count == 0)
                experiment_simUnit = readSowing(sowingFile, site, variety, startYear, endYear);

            experiment_simUnit.site = site;

            var path = Path.Combine(Reffile, "referenceData.csv");
            if (!File.Exists(path))
                throw new FileNotFoundException($"referenceData.csv not found at '{path}'");

            // --- helpers ------------------------------------------------------------
            static string Norm(string s)
                => new string((s ?? "").Where(ch => ch != ' ' && ch != '_').ToArray()).ToLowerInvariant();

            int GetCol(Dictionary<string, int> headerMap, params string[] aliases)
            {
                foreach (var a in aliases)
                {
                    var key = Norm(a);
                    if (headerMap.TryGetValue(key, out var idx)) return idx;
                }
                return -1;
            }

            static bool TryParseFloat(string s, out float v)
            {
                // try dot-decimal first, then Italian comma-decimal
                return float.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands,
                                      CultureInfo.InvariantCulture, out v)
                    || float.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands,
                                      CultureInfo.GetCultureInfo("it-IT"), out v);
            }

            static void EnsureDict<TK, TV>(ref Dictionary<TK, TV> d)
            {
                if (d == null) d = new Dictionary<TK, TV>();
            }

            // --- open CSV with proper quoting --------------------------------------
            using var parser = new TextFieldParser(path);
            parser.SetDelimiters(",");
            parser.HasFieldsEnclosedInQuotes = true;
            parser.TrimWhiteSpace = true;

            if (parser.EndOfData)
                throw new InvalidOperationException($"File '{path}' is empty.");

            // header
            var header = parser.ReadFields() ?? Array.Empty<string>();
            var headerMap = new Dictionary<string, int>(StringComparer.Ordinal);
            for (int i = 0; i < header.Length; i++)
            {
                var key = Norm(header[i]?.Trim());
                if (!headerMap.ContainsKey(key)) headerMap[key] = i;
            }

            // columns (with aliases)
            int fintCol = GetCol(headerMap, "fint", "f_int", "lightInterception", "light_int", "lightinterception");
            int agbCol = GetCol(headerMap, "agb", "above_ground_biomass", "biomass", "abovegroundbiomass", "wtop");
            int yearCol = GetCol(headerMap, "year", "yr");
            int doyCol = GetCol(headerMap, "doy", "day_of_year", "dy", "d");
            int varietyCol = GetCol(headerMap, "variety", "cultivar", "cv");

            // per-disease severity
            int diseaseCol = GetCol(headerMap, disease, $"{disease}_sev", $"{disease}_severity");

            // yields (optional)
            int yieldAttCol = GetCol(headerMap, "YieldAttainable", "YieldUnlimited", "YieldPotential", "Yield", "wgrn", "GrainYieldPotential");
            int yieldActCol = GetCol(headerMap, "YieldActual", "YieldDiseased", "YieldAct", "YieldLimited", "GrainYieldLimited");

            bool warnedNoDiseaseCol = false;

            // --- read rows ----------------------------------------------------------
            string varietyWanted = (variety ?? "").Trim();
            while (!parser.EndOfData)
            {
                var line = parser.ReadFields();
                if (line == null || line.Length == 0) continue;

                // variety filter (robust)
                if (varietyCol >= 0)
                {
                    var vcell = (varietyCol < line.Length ? line[varietyCol] : null)?.Trim();
                    if (!string.Equals(vcell, varietyWanted, StringComparison.OrdinalIgnoreCase))
                        continue;
                }

                // date (optional)
                DateTime date = DateTime.MinValue;
                if (yearCol >= 0 && doyCol >= 0 &&
                    yearCol < line.Length && doyCol < line.Length &&
                    int.TryParse(line[yearCol], out int y) &&
                    int.TryParse(line[doyCol], out int doy) &&
                    doy >= 1 && doy <= 366)
                {
                    date = new DateTime(y, 1, 1).AddDays(doy - 1);
                }

                // FINT
                if (fintCol >= 0 && fintCol < line.Length && TryParseFloat(line[fintCol], out var fint))
                    experiment_simUnit.referenceData.date_fint[date] = fint;

                // AGB
                if (agbCol >= 0 && agbCol < line.Length && TryParseFloat(line[agbCol], out var agb))
                    experiment_simUnit.referenceData.date_agb[date] = agb;

                // Yield attainable (optional)
                if (yieldAttCol >= 0 && yieldAttCol < line.Length && TryParseFloat(line[yieldAttCol], out var yAtt))
                {
                    EnsureDict(ref experiment_simUnit.referenceData.date_yieldAttainable);
                    experiment_simUnit.referenceData.date_yieldAttainable[date] = yAtt;
                }

                // Yield actual (optional)
                if (yieldActCol >= 0 && yieldActCol < line.Length && TryParseFloat(line[yieldActCol], out var yAct))
                {
                    EnsureDict(ref experiment_simUnit.referenceData.date_yieldActual);
                    experiment_simUnit.referenceData.date_yieldActual[date] = yAct;
                }

                // Disease severity
                if (diseaseCol >= 0 && diseaseCol < line.Length && TryParseFloat(line[diseaseCol], out var dsev))
                {
                    if (!experiment_simUnit.referenceData.disease_date_diseaseSev
                            .TryGetValue(disease, out var dict))
                    {
                        dict = new Dictionary<DateTime, float>();
                        experiment_simUnit.referenceData.disease_date_diseaseSev[disease] = dict;
                    }
                    dict[date] = dsev;
                }
                else if (diseaseCol < 0 && !warnedNoDiseaseCol)
                {
                    Console.WriteLine($"[readReference] Column for disease '{disease}' not found in {path}. " +
                                      $"Available columns: {string.Join(", ", header)}");
                    warnedNoDiseaseCol = true;
                }
            }

            return experiment_simUnit;
        }

        static bool TryReadFloat(string[] row, int col, out float value)
        {
            value = default;
            if (col < 0 || col >= row.Length) return false;

            string s = (row[col] ?? "").Trim();
            if (s.Length == 0 || s.Equals("NA", StringComparison.OrdinalIgnoreCase)) return false;

            return float.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture, out value);
        }

        static void EnsureDict(ref Dictionary<DateTime, float>? dict)
        {
            if (dict == null) dict = new Dictionary<DateTime, float>();
        }

        //read external crop model outputs
        internal cropModelData readCropModelData(
            string cropModelFile)
        {
            cropModelData cropModelData = new cropModelData();

            var path = Path.Combine(cropModelFile, "cropModelData.csv");
            using var sr = new StreamReader(path);

            // --- read header ---
            string headerLine = sr.ReadLine();
            if (headerLine == null)
                throw new InvalidOperationException($"File '{path}' is empty.");

            // Naive CSV split (kept as in your code)
            string[] header = headerLine.Split(',', '"');

            // Normalizer: lowercase + remove spaces/underscores for robust matching
            static string Norm(string s) => new string(s
                .Where(ch => ch != ' ' && ch != '_')
                .ToArray()).ToLowerInvariant();

            // Precompute normalized header for fast lookup
            string[] headerNorm = header.Select(h => h is null ? "" : Norm(h.Trim())).ToArray();

            // Helper to find first matching column from a list of aliases (case/space/underscore-insensitive)
            int GetCol(params string[] aliases)
            {
                var alts = aliases.Select(a => Norm(a)).ToArray();
                for (int i = 0; i < headerNorm.Length; i++)
                {
                    string h = headerNorm[i];
                    if (string.IsNullOrWhiteSpace(h)) continue;
                    if (alts.Any(a => h.Equals(a, StringComparison.Ordinal)))
                        return i;
                }
                return -1;
            }

            // ---- column indices (with alias sets) ----
            int fintCol = GetCol("fint", "f_int", "lightInterception", "light_int", "lightinterception");
            int agbCol = GetCol("agb", "above_ground_biomass", "biomass", "abovegroundbiomass", "wtop");
            int yearCol = GetCol("year", "yr");
            int doyCol = GetCol("doy", "day_of_year", "dy", "d");
            int yieldAttCol = GetCol("YieldAttainable", "YieldUnlimited", "YieldPotential", "Yield", "wgrn", "GrainYieldPotential");


            // --- loop over lines ---
            while (!sr.EndOfStream)
            {
                string? raw = sr.ReadLine();
                if (string.IsNullOrEmpty(raw)) continue;

                string[] line = raw.Split(',', '"');
                if (line.Length == 0) continue;


                DateTime date = DateTime.MinValue;
                if (yearCol >= 0 && doyCol >= 0 &&
                    int.TryParse(line[yearCol], out int y) &&
                    int.TryParse(line[doyCol], out int doy))
                {
                    date = new DateTime(y, 1, 1).AddDays(doy-1);
                }

                // FINT
                if (TryReadFloat(line, fintCol, out float fint))
                    cropModelData.fInt[date] = fint;

                // AGB
                if (TryReadFloat(line, agbCol, out float agb))
                    cropModelData.agb[date] = agb;

                // YIELD
                if (TryReadFloat(line, yieldAttCol, out float yield))
                    cropModelData.yield[date] = yield; 
            }


            // --- compute cycleLength based on DOY resets ---
            var dates = cropModelData.yield.Keys
      .Concat(cropModelData.fInt.Keys)
      .Concat(cropModelData.agb.Keys)
      .Distinct()
      .OrderBy(d => d)
      .ToList();

            List<(DateTime start, DateTime end)> cycles = new();
            DateTime cycleStart = dates.First();

            for (int i = 1; i < dates.Count; i++)
            {
                var prev = dates[i - 1];
                var curr = dates[i];

                // detect DOY jump that is not a year wrap
                bool doyBackwards = curr.DayOfYear < prev.DayOfYear;
                bool yearWrap = prev.Month == 12 && curr.Month == 1;
                bool sowingJump = doyBackwards && !yearWrap;

                // detect harvest reset
                cropModelData.yield.TryGetValue(prev, out float yPrev);
                cropModelData.yield.TryGetValue(curr, out float yCurr);
                bool harvestReset = (yCurr <= 100 && yPrev > 100);

                if (sowingJump || harvestReset)
                {
                    cycles.Add((cycleStart, prev));
                    cycleStart = curr;
                }
            }

            cycles.Add((cycleStart, dates.Last()));  // close final cycle



            // assign progress
            foreach (var (start, end) in cycles)
            {
                double total = (end - start).TotalDays;
                if (total <= 0) continue;

                foreach (var d in dates.Where(dd => dd >= start && dd <= end))
                {
                    double frac = (d - start).TotalDays / total;
                    cropModelData.cyclePercentage[d] = (float)frac*100;
                }
            }



            return cropModelData;
        }
    }
}
