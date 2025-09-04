// -----------------------------------------------------------------------------
// Runner entry: reads a JSON configuration, sets paths & switches between
// calibration and validation modes, runs the optimizer, and writes/merges
// calibrated parameters to disk.
// -----------------------------------------------------------------------------

using System.Text.Json;
using runner;
using UNIMI.optimizer;

#region read the configuration file (FraNchEstYNConfig.json)
// Read command-line args. Expected usage:
//   app.exe <config.json> [--workdir <folder>]
var cmdArgs = Environment.GetCommandLineArgs();
if (cmdArgs.Length < 2 || !File.Exists(cmdArgs[1]))
{
    // Fail fast if the config path is missing or invalid
    Console.WriteLine("❌ Config path missing or invalid.");
    return;
}

string configPath = cmdArgs[1];

// Default working directory is the executable folder.
// This can be overridden via "--workdir <path>".
string workdir = AppContext.BaseDirectory;

// Parse additional CLI switches starting at arg #2
for (int i = 2; i < cmdArgs.Length; i++)
{
    // Optional switch to set a sandbox-like working directory
    if (cmdArgs[i].Equals("--workdir", StringComparison.OrdinalIgnoreCase) && i + 1 < cmdArgs.Length)
    {
        workdir = Path.GetFullPath(cmdArgs[i + 1]);
        Directory.CreateDirectory(workdir);
        Console.WriteLine($"[FraNchEstYN] Using workdir: {workdir}");
        i++; // skip the value we just consumed
    }
}

// Load and deserialize the configuration JSON into Root
string jsonString = File.ReadAllText(configPath);
var config = JsonSerializer.Deserialize<Root>(jsonString);
#endregion

#region settings
// Read top-level settings; note that several JSON fields are nullable, so
// we use GetValueOrDefault where appropriate.
bool isCalibration = bool.Parse(config.settings.isCalibration);
string calibrationVariable = Convert.ToString(config.settings.calibrationVariable);
int startYear = config.settings.startYear.GetValueOrDefault();
int endYear = config.settings.endYear.GetValueOrDefault();
List<string> sites = config.settings.sites;
List<string> varieties = config.settings.varieties;
string disease = config.settings.disease;
int simplexes = config.settings.simplexes.GetValueOrDefault();
int iterations = config.settings.iterations.GetValueOrDefault();
string weatherTimeStep = config.settings.weatherTimeStep;

// Human-readable mode label
string runningMode = "";
if (isCalibration) runningMode = "calibration"; else runningMode = "validation";

// Optional external crop model data source (could be "no")
string cropModelSource = config.settings.cropModel;

// Reader for reference data (observations, sowing, etc.)
var referenceReader = new runner.referenceReader();
cropModelData cropModelData = new cropModelData();

// If crop model data are provided in config, read them now
if (cropModelSource != "no")
{
    cropModelData = referenceReader.readCropModelData(config.paths.cropModelFile);
}
#endregion

#region paths
// Resolve all input/output paths, either relative to a sandbox workdir
// (when --workdir is used) or directly from the JSON configuration.
string weatherDir, paramFile, sowingDOY, referenceDir, outputDir;

if (cmdArgs.Contains("--workdir"))
{
    // Sandbox mode: use a fixed folder structure under the chosen workdir
    weatherDir = Path.Combine(workdir, "files", "weather");
    paramFile = Path.Combine(workdir, "files", "parameters", "franchestynParameters.csv");
    sowingDOY = Path.Combine(workdir, "files", "management", "sowing.csv");
    referenceDir = Path.Combine(workdir, "files", "reference");
    outputDir = Path.Combine(workdir, "outputs");

    // Make sure all directories exist
    Directory.CreateDirectory(weatherDir);
    Directory.CreateDirectory(Path.GetDirectoryName(paramFile));
    Directory.CreateDirectory(Path.GetDirectoryName(sowingDOY));
    Directory.CreateDirectory(referenceDir);
    Directory.CreateDirectory(outputDir);
}
else
{
    // Normal mode: trust the JSON config paths as-is
    weatherDir = config.paths.weatherDir;
    paramFile = config.paths.paramFile;
    sowingDOY = config.paths.sowingFile;
    referenceDir = config.paths.referenceFilePaths;

    // Keep outputs near the executable (could also come from JSON if preferred)
    outputDir = Path.Combine(AppContext.BaseDirectory, "outputs");
    Directory.CreateDirectory(outputDir);
}
#endregion

// Create optimizer instance (provided by UNIMI.optimizer)
var optimizer = new Optimizer();

// Read parameter definitions (names, bounds, defaults, calibration flags)
var paramReader = new paramReader();
var nameParam = paramReader.read(paramFile, calibrationVariable);

// Storage for calibrated results (param → value). Empty by default.
// During validation we typically pass an empty set to run with defaults.
Dictionary<string, float> paramCalibValue = new Dictionary<string, float>();

// Ensure the console is unbuffered for real-time progress printing
var stdout = new StreamWriter(Console.OpenStandardOutput()) { AutoFlush = true };
Console.SetOut(stdout);

#region switch between calibration and validation
if (isCalibration)
{
    #region calibration

    #region define optimizer settings
    // Multistart Nelder–Mead simplex configuration:
    // - NofSimplexes: number of random starts
    // - Ftol: tolerance on objective function for convergence
    // - Itmax: maximum iterations per simplex
    MultiStartSimplex msx = new MultiStartSimplex();
    msx.NofSimplexes = simplexes;
    msx.Ftol = 0.000000000001;
    msx.Itmax = iterations;
    #endregion

    // Iterate over sites and varieties to calibrate each pair
    foreach (var site in sites)
    {
        foreach (var variety in varieties)
        {
            #region read reference data
            // Build the simulation unit and attach observed/reference data for this site/variety
            simulationUnit simulationUnit = new simulationUnit();

            optimizer.simulationUnit = referenceReader.readReference(
                config.paths.referenceFilePaths,
                config.paths.sowingFile,
                site, variety,
                startYear, endYear,
                ref simulationUnit,
                disease
            );

            // If calibration is on disease metrics but no disease data exist, skip with a warning
            if (optimizer.simulationUnit.referenceData.disease_date_diseaseSev.Count == 0 &&
                calibrationVariable == "disease")
            {
                Console.WriteLine("disease data not found for site {0} and variety {1}. Please check", site, variety);
                break;
            }
            #endregion

            optimizer.availableSites = sites;

            #region Define parameter settings for calibration
            // The entire parameter space (nameParam) is available to the optimizer
            optimizer.nameParam = nameParam;

            // Determine which parameters are in the calibration subset
            int paramCalibrated = 0;
            var param_outCalibration = new Dictionary<string, float>();
            var calibratedParamNames = new List<string>();

            // Decide: parameters matching the calibrationVariable (or "all") and marked with a non-empty calibration tag
            foreach (var kvp in nameParam)
            {
                string name = kvp.Key;
                var param = kvp.Value;

                if (!string.IsNullOrWhiteSpace(param.calibration) &&
                    (kvp.Key.IndexOf(calibrationVariable, StringComparison.OrdinalIgnoreCase) >= 0 ||
                     string.Equals(calibrationVariable, "all", StringComparison.OrdinalIgnoreCase)))
                {
                    paramCalibrated++;
                    calibratedParamNames.Add(name);
                }
                else
                {
                    // Keep default value for parameters outside the calibration subset
                    param_outCalibration[name] = param.value;
                }
            }

            // Build bounds array (Limits) for the calibrated subset [min, max] per parameter
            double[,] Limits = new double[paramCalibrated, 2];
            for (int i = 0; i < calibratedParamNames.Count; i++)
            {
                var name = calibratedParamNames[i];
                var param = nameParam[name];
                Limits[i, 0] = param.minimum;
                Limits[i, 1] = param.maximum;
            }

            // Results buffer returned by the optimizer (1 row x N params here)
            double[,] results = new double[1, 1];
            #endregion

            #region set optimizer calibration properties
            // Set all context needed by the objective function (inside optimizer)
            optimizer.isCalibration = isCalibration;
            optimizer.calibrationVariable = calibrationVariable;
            optimizer.param_outCalibration = param_outCalibration;
            optimizer.startYear = startYear;
            optimizer.endYear = endYear;
            optimizer.weatherDir = weatherDir;
            optimizer.site = site;
            optimizer.paramFile = paramFile;
            optimizer.weatherTimeStep = weatherTimeStep;
            optimizer.disease = disease;
            optimizer.variety = variety;
            optimizer.cropModelData = cropModelData; // pass external crop model data if any
            #endregion

            // Run the multistart simplex optimizer
            msx.Multistart(optimizer, paramCalibrated, Limits, out results);

            // Extract calibrated values in dictionary form (param name → float value)
            paramCalibValue = new Dictionary<string, float>();
            int count = 0;

            #region write calibrated parameters
            // For writing, we introspect the 'parameters' structure to ensure the param path exists.
            var parameters = new parameters();
            HashSet<string> parProperties;

            // Build the set of "parX_Y" property names via reflection (top-level and nested)
            parProperties = parameters.GetType()
                            .GetProperties()
                            .SelectMany(p =>
                            {
                                var value = p.GetValue(parameters);
                                if (value == null)
                                    return new[] { p.Name };

                                return value.GetType()
                                            .GetProperties()
                                            .Select(np => $"{p.Name}_{np.Name}");
                            })
                            .ToHashSet();

            // Prepare a CSV with "model,param,value" header
            string header = "model, param, value";
            List<string> writeParam = new List<string> { header };

            // Iterate through all known parameters and pick those in the calibrated subset
            foreach (var param in optimizer.nameParam.Keys)
            {
                if (optimizer.nameParam[param].calibration != "" &&
                    parProperties.Contains("par" + param) &&
                    (param.Contains(calibrationVariable) || calibrationVariable == "all"))
                {
                    // Build CSV line: model,paramName,value
                    string line = "";
                    string model = param.Split("_")[0];
                    string paramName = param.Split("_")[1];
                    line += model + ",";
                    line += paramName + ",";
                    line += results[0, count];
                    writeParam.Add(line);

                    // Store calibrated numeric value
                    paramCalibValue.Add(param, (float)results[0, count]);
                    count++;
                }
            }

            // Persist or merge the calibrated parameters for this site/variety
            WriteOrMergeParameters(site, variety, writeParam);
            #endregion

            // Run a one-shot simulation with calibrated parameters to produce outputs
            var dateOutputs = new Dictionary<DateTime, outputs>();
            optimizer.oneShot(paramCalibValue, out dateOutputs);
            Console.WriteLine(); // spacing
        }
    }
    #endregion
}
else
{
    #region validation
    // Validation mode: no optimization. Use default values (or previously calibrated values
    // if paramCalibValue is populated) and run the model directly.
    foreach (var site in sites)
    {
        foreach (var variety in varieties)
        {
            // Create a fresh simulation unit for this site/variety
            var simulationUnit = new simulationUnit();

            #region read reference data
            optimizer.simulationUnit = referenceReader.readReference(
                config.paths.referenceFilePaths,
                config.paths.sowingFile,
                site, variety,
                startYear, endYear,
                ref simulationUnit,
                disease
            );
            #endregion

            // Build parameter map as “out-of-calibration” set (i.e., default values)
            var param_outCalibration = new Dictionary<string, float>();
            var calibratedParamNames = new List<string>();

            foreach (var kvp in nameParam)
            {
                string name = kvp.Key;
                var param = kvp.Value;
                param_outCalibration[name] = param.value;
            }

            #region assign default parameters (out of calibration subset)
            optimizer.nameParam = nameParam;
            #endregion

            #region set optimizer validation properties
            optimizer.availableSites = sites;
            optimizer.isCalibration = isCalibration;
            optimizer.calibrationVariable = calibrationVariable;
            optimizer.weatherDir = weatherDir;
            optimizer.nameParam = nameParam;
            optimizer.startYear = startYear;
            optimizer.endYear = endYear;
            optimizer.weatherTimeStep = weatherTimeStep;
            optimizer.param_outCalibration = param_outCalibration;
            optimizer.site = site;
            optimizer.paramFile = paramFile;
            optimizer.weatherTimeStep = weatherTimeStep;
            optimizer.cropModelData = cropModelData;
            #endregion

            // Run validation (no optimization): produce outputs for analysis/plots
            var dateOutputs = new Dictionary<DateTime, outputs>();
            optimizer.disease = disease;
            optimizer.variety = variety;
            optimizer.oneShot(paramCalibValue, out dateOutputs);
        }
    }
    #endregion
}
#endregion

// Resolve a parameter file to use (most specific wins):
//   1) parameters_{site}_{variety}.csv
//   2) parameters_{site}.csv
//   3) latest parameters_{site}_*.csv
string ResolveParamFile(string site, string variety)
{
    var dir = "calibratedParameters";
    Directory.CreateDirectory(dir);

    var exact = Path.Combine(dir, $"parameters_{site}_{variety}.csv");
    if (File.Exists(exact)) return exact;

    var siteOnly = Path.Combine(dir, $"parameters_{site}.csv");
    if (File.Exists(siteOnly)) return siteOnly;

    // Look for any file for this site, pick most recent
    var pattern = $"parameters_{site}_*.csv";
    var candidates = Directory.EnumerateFiles(dir, pattern)
                              .OrderByDescending(File.GetLastWriteTimeUtc)
                              .ToList();

    if (candidates.Count > 0) return candidates[0];

    return null; // none found
}

// Writes newly calibrated parameters, merging with an existing CSV if present.
// - Uses the first two CSV columns (model,param) as a unique key for merging.
// - Keeps any non-keyed prefix (comments/headers) at the top.
void WriteOrMergeParameters(string site, string variety, IEnumerable<string> writeParam)
{
    // Base executable directory (unused but could be used for alternative placements)
    string exeDir = AppContext.BaseDirectory;

    // Preferred output dir: <workdir>/calibratedParameters
    string dir = Path.Combine(workdir, "calibratedParameters");
    Directory.CreateDirectory(dir);

    // Fallback location if we lack permissions (e.g., Program Files)
    try
    {
        Directory.CreateDirectory(dir);
    }
    catch (UnauthorizedAccessException)
    {
        string fallback = Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
            "FraNchEstYN", "calibratedParameters");
        Directory.CreateDirectory(fallback);
        dir = fallback;
    }

    // Final CSV path (per site+variety)
    string fileName = $"parameters_{site}_{variety}.csv";
    string path = Path.Combine(dir, fileName);

    // --- CSV helpers (unchanged logic) ---
    // Robust-ish CSV splitter that respects simple quoted fields and double quotes
    static List<string> SplitCsv(string line)
    {
        var cols = new List<string>();
        if (line == null) return cols;

        bool inQuotes = false;
        var cur = new System.Text.StringBuilder();
        for (int i = 0; i < line.Length; i++)
        {
            char c = line[i];
            if (c == '"')
            {
                if (inQuotes && i + 1 < line.Length && line[i + 1] == '"')
                { cur.Append('"'); i++; }              // Escaped quote ("")
                else { inQuotes = !inQuotes; }         // Toggle quoted state
            }
            else if (c == ',' && !inQuotes)
            { cols.Add(cur.ToString()); cur.Clear(); } // Field boundary
            else
            { cur.Append(c); }
        }
        cols.Add(cur.ToString());
        return cols;
    }

    // Number of columns used to form the merge key (model,param)
    const int keyColumns = 2;

    // Extract a merge key from a CSV line (first two columns), ignoring comments
    static string GetKey(string line)
    {
        if (string.IsNullOrWhiteSpace(line)) return null;
        var t = line.Trim();
        if (t.StartsWith("#")) return null;
        var cols = SplitCsv(t);
        if (cols.Count == 0) return null;
        var take = Math.Min(keyColumns, cols.Count);
        var key = string.Join(",", cols.Take(take)).Trim();
        return string.IsNullOrEmpty(key) ? null : key;
    }

    // Load existing file (if any), preserving any non-keyed prefix lines
    var orderedKeys = new List<string>();
    var map = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
    var prefix = new List<string>();

    if (File.Exists(path))
    {
        bool seenKeyed = false;
        foreach (var line in File.ReadAllLines(path))
        {
            var key = GetKey(line);
            if (key == null && !seenKeyed) { prefix.Add(line); continue; } // still in header/comments
            if (key == null) continue;                                     // ignore extra blanks
            seenKeyed = true;
            if (!map.ContainsKey(key)) orderedKeys.Add(key);
            map[key] = line; // last one wins
        }
    }

    // Merge incoming lines (replace by key or append new)
    foreach (var line in writeParam)
    {
        var key = GetKey(line);
        if (key == null) continue;
        if (!map.ContainsKey(key)) orderedKeys.Add(key);
        map[key] = line;
    }

    // Reconstruct file: prefix + ordered keyed lines
    var output = new List<string>(prefix);
    foreach (var k in orderedKeys)
        if (map.TryGetValue(k, out var line))
            output.Add(line);

    File.WriteAllLines(path, output);
}

// -----------------------------------------------------------------------------
// Configuration objects for JSON deserialization
// -----------------------------------------------------------------------------
public class Root
{
    public Settings? settings { get; set; }
    public Paths? paths { get; set; }
}

// Settings block: high-level run options and model toggles
public class Settings
{
    public string? isCalibration { get; set; }     // "true"/"false" as string
    public string? calibrationVariable { get; set; } // e.g., "disease" or "all"
    public int? startYear { get; set; }
    public int? endYear { get; set; }
    public List<string>? sites { get; set; }
    public List<string>? varieties { get; set; }
    public string? disease { get; set; }
    public int? simplexes { get; set; }
    public int? iterations { get; set; }
    public string weatherTimeStep { get; set; }    // e.g., "daily" or "hourly"
    public string cropModel { get; set; }          // e.g., "no" or a file reference
}

// Paths block: file system locations used by the run
public class Paths
{
    public string? weatherDir { get; set; }
    public string? paramFile { get; set; }
    public string? referenceFilePaths { get; set; }
    public string? referenceFileDiseases { get; set; }
    public string? sowingFile { get; set; }
    public string? cropModelFile { get; set; }
}
