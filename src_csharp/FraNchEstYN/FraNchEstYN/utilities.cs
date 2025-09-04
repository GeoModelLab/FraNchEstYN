// -----------------------------------------------------------------------------
// utilities: helper methods for parameter I/O and simple biophysical functions.
// -----------------------------------------------------------------------------
internal class utilities
{
    // Reads a CSV of calibrated parameters and returns a dictionary:
    //   key: "Name_Class", value: calibrated float
    // Uses paramReader.calibratedRead(filePath).
    // 'variety' is currently unused (kept for signature compatibility).
    public static Dictionary<string, float> ReadFileOrExitsParameters(string filePath, string variety)
    {
        Dictionary<string, float> ReadFileOrExit = new Dictionary<string, float>();
        paramReader _paramReader = new paramReader();
        ReadFileOrExit = _paramReader.calibratedRead(filePath);
        return ReadFileOrExit; // Return parsed (possibly empty) dictionary
    }

    // Temperature response function (beta-like):
    // Returns a dimensionless efficiency (0..1) as a function of average temperature (tAve)
    // relative to base (tBase), optimum (tOpt), and maximum (tMax) temperatures.
    //
    // Behavior:
    //   - tAve <= tBase or tAve >= tMax  → 0
    //   - Peak near tOpt
    //   - Asymmetric shape controlled by distances to tBase and tMax
    //
    // Typical units: °C for all temperature inputs.
    public static float Tresponse(float tAve, float tBase, float tOpt, float tMax)
    {
        float Tfunction = 0;

        // Outside thermal bounds → no response
        if (tAve < tBase || tAve > tMax)
        {
            Tfunction = 0;
        }
        else
        {
            // Beta-style temperature response:
            // f = ((tMax - tAve) / (tMax - tOpt)) * ((tAve - tBase) / (tOpt - tBase))^((tOpt - tBase)/(tMax - tOpt))
            float firstTerm = (tMax - tAve) / (tMax - tOpt);
            float secondTerm = (tAve - tBase) / (tOpt - tBase);
            float Exponential = (tOpt - tBase) / (tMax - tOpt);

            Tfunction = firstTerm * (float)Math.Pow(secondTerm, Exponential);
        }
        return Tfunction;
    }

    // Rain detachment index (dimensionless, 0..1):
    // Scales with rainfall and saturates as rainfall increases relative to a capacity term.
    //
    // Inputs:
    //   rainfall : precipitation over the time step (e.g., mm)
    //   rain50   : half-saturation parameter (rainfall at which the index reaches ~0.5 when LAI=1)
    //   fInt     : light interception (dimensionless)
    //
    // Formula:
    //   detachment = rainfall / (rain50 * fInt + rainfall)
    //
    // Notes:
    //   - Increases with rainfall; decreases with larger fInt or rain50.
    //   - Clamped only by arithmetic; caller should ensure non-negative inputs.
    public static float RainDetachment(float rainfall, float rain50, float fInt)
    {
        float RainDetachment = 0;

        RainDetachment = rainfall / ((rain50 * fInt) + rainfall);
        return RainDetachment;
    }
}
