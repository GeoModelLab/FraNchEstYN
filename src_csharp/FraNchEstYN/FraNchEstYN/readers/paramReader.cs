// -----------------------------------------------------------------------------
// paramReader: reads CSV parameter definitions for simulation setup/calibration.
//
// read(file, calibrationVariable):
//   Expected CSV columns (0-based):
//     0: Name | 1: Class | 2–3: (ignored, description and unit) | 4: Min | 5: Max | 6: Value | 7: CalibrationSubset
//   • Dictionary key: "Name_Class"
//   • Supports boolean parameters: true/false/1/0 (case-insensitive)
//   • Numeric parsing uses InvariantCulture; Min/Max required for numeric params
//
// calibratedRead(file):
//   Expected CSV columns (0-based): 0: Name | 1: Class | 2: Value
//   • Dictionary key: "Name_Class"
//   • Value parsed as float with current culture (consider InvariantCulture for portability)
// -----------------------------------------------------------------------------

using System.Globalization;

public class paramReader
{
    public Dictionary<string, parameter> read(string file, string calibrationVariable)
    {
        var nameParam = new Dictionary<string, parameter>();

        // Open the file and skip the header row
        using var sr = new StreamReader(file);
        sr.ReadLine();

        while (!sr.EndOfStream)
        {
            // Split CSV line by comma; this assumes no quoted commas
            string[] line = sr.ReadLine().Split(',');

            // Create and populate a parameter object from the row
            parameter parameter = new parameter();

            // Column 6: parameter value (can be boolean or numeric)
            string rawValue = line[6].Trim();

            // Simple boolean detection: true/false/1/0 (case-insensitive)
            bool isBoolParam =
                rawValue.Equals("true", StringComparison.OrdinalIgnoreCase) ||
                rawValue.Equals("false", StringComparison.OrdinalIgnoreCase) ||
                rawValue.Equals("1", StringComparison.OrdinalIgnoreCase) ||
                rawValue.Equals("0", StringComparison.OrdinalIgnoreCase);

            if (isBoolParam)
            {
                // Set boolean fields and mark the parameter as boolean
                parameter.valueBool = rawValue.Equals("true", StringComparison.OrdinalIgnoreCase) || rawValue == "1";
                parameter.isBoolean = true;
            }
            else
            {
                // Numeric parameter: parse using invariant culture
                parameter.value = float.Parse(rawValue, CultureInfo.InvariantCulture);
                parameter.minimum = float.Parse(line[4], CultureInfo.InvariantCulture);
                parameter.maximum = float.Parse(line[5], CultureInfo.InvariantCulture);
            }

            // Column 7: calibration subset/tag (free text)
            parameter.calibration = line[7];

            // Dictionary key composed from columns 0 and 1: "Name_Class"
            nameParam[line[0] + "_" + line[1]] = parameter;
        }

        return nameParam;
    }

    public Dictionary<string, float> calibratedRead(string file)
    {
        var paramCalibValue = new Dictionary<string, float>();

        if (File.Exists(file))
        {
            // Open the file and skip the header row
            using var sr = new StreamReader(file);
            sr.ReadLine();

            while (!sr.EndOfStream)
            {
                string[] line = sr.ReadLine().Split(',');

                // Map "Name_Class" → calibrated value
                // Note: this uses the current culture for parsing; consider InvariantCulture for portability
                paramCalibValue.Add(line[0] + "_" + line[1], float.Parse(line[2]));
            }
        }

        return paramCalibValue;
    }
}

// Represents a single parameter (numeric or boolean) used in the simulation/calibration
public class parameter
{
    public float minimum { get; set; }      // Lower bound (e.g., calibration search space)
    public float maximum { get; set; }      // Upper bound (e.g., calibration search space)
    public float value { get; set; }        // Current numeric value (when not boolean)
    public string calibration { get; set; } // Calibration subset/tag (e.g., "included", "excluded", subset name)
    public string paramClass { get; set; }  // Optional category/class of the parameter
    public bool valueBool { get; set; }     // Current boolean value (when isBoolean = true)
    public bool isBoolean { get; set; }     // True if the parameter is boolean; otherwise numeric
}
