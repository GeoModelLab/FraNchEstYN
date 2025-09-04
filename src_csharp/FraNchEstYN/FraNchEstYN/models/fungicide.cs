
/// Models daily fungicide dynamics (degradation, tenacity, efficacy) after an application, updating the provided output object based on time since last treatment and weather.

public class fungicideModel
{
    
    /// Runs one daily step of the fungicide model.
    public void fungicideRun(inputsDaily input, parameters parameters, outputs output, outputs output1)
    {
        // Compute days elapsed since the last treatment
        TimeSpan _timeSpan = input.date - input.DateTreatmentLast;
        float DaysFromApplication = (float)_timeSpan.TotalDays;

        // Only proceed if a valid treatment date exists (Year > 1 used as a "has value" sentinel).
        if (input.DateTreatmentLast.Year > 1)
        {
            #region Fungicide concentration factor
            // At the exact day of application, set baseline concentration/tenacity to 1 (full).
            if (input.date == input.DateTreatmentLast)
            {
                // Factor relating today’s concentration to previous day’s actual degradation (baseline at application).
                output1.fungicide.FungicideConcentrationFactor = 1;

                // Tenacity function is also baseline at application day.
                output1.fungicide.FungicideTenacityFunction = 1;

                // Carry forward / initialize tenacity state to full strength.
                output.fungicide.FungicideTenacity = 1;
            }
            else
            {
                // After the application day, concentration factor depends exponentially on
                // yesterday’s actual degradation relative to its maximum (1).
                // If FungicideActualDegradation < 1, the exponent is negative -> factor < 1.
                // The "3" is a scaling constant that sharpens/softens the response.
                output1.fungicide.FungicideConcentrationFactor =
                    (float)Math.Exp((output.fungicide.FungicideActualDegradation - 1) * 3);
            }
            #endregion

            #region Fungicide potential degradation
            // First-order decay of the initial dose with time since application.
            // This represents how much of the fungicide would remain (or be "available")
            // absent wash-off/tenacity effects.
            output1.fungicide.FungicidePotentialDegradation = parameters.parfungicide.InitialDose *
               (float)Math.Exp(-parameters.parfungicide.DegradationRate * DaysFromApplication);
            #endregion

            #region Tenacity function
            // Tenacity captures rainfall-driven wash-off or loss of protection.
            // Here it decays exponentially with sqrt(precipitation), scaled by TenacityFactor
            // and the concentration factor computed above.
            output.fungicide.FungicideTenacityFunction = (float)Math.Exp(
                -parameters.parfungicide.TenacityFactor *
                output1.fungicide.FungicideConcentrationFactor *
                (float)Math.Sqrt(input.Precipitation)
            );

            // Update cumulative/ongoing tenacity state:
            // today’s tenacity = yesterday’s tenacity * today’s tenacity function.
            output1.fungicide.FungicideTenacity = output.fungicide.FungicideTenacity *
                output.fungicide.FungicideTenacityFunction;
            #endregion

            #region Fungicide actual degradation
            // Actual degradation considers both potential chemical decay and loss of tenacity.
            // (Think of this as the "effective" remaining fraction after environmental effects.)
            output1.fungicide.FungicideActualDegradation = output1.fungicide.FungicideTenacity *
                output1.fungicide.FungicidePotentialDegradation;
            #endregion

            #region Fungicide efficacy
            // Map the actual degradation to efficacy using a logistic response.
            // AShapeParameter shifts the curve; BShapeParameter controls its slope.
            // InitialEfficacy caps the maximum.
            output1.fungicide.FungicideEfficacy = parameters.parfungicide.InitialEfficacy /
                (1 + (float)Math.Exp(
                    parameters.parfungicide.AShapeParameter -
                    parameters.parfungicide.BShapeParameter * output1.fungicide.FungicideActualDegradation
                ));
            #endregion

            // TODO Maximum duration: hard stop after 30 days from application.
            // After this threshold, all effects are zeroed.
            if (DaysFromApplication >= 30)
            {
                output1.fungicide.FungicideEfficacy = 0;
                output1.fungicide.FungicideActualDegradation = 0;
                output1.fungicide.FungicideConcentrationFactor = 0;
                output1.fungicide.FungicidePotentialDegradation = 0;
                output1.fungicide.FungicideTenacityFunction = 0;
            }
        }
        // If there's no valid treatment date, the method leaves outputs unchanged.
    }
}
