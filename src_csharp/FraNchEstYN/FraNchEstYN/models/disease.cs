// This class computes hourly and daily disease progression in the SEIR model.
public class diseaseModel
{
    // Lists to store hourly weather data and then derive daily values
    List<float> temp = new List<float>();  // °C - hourly temperatures
    List<float> rh = new List<float>();    // % - hourly relative humidity
    List<float> rain = new List<float>();  // mm - hourly rainfall
    List<float> lw = new List<float>();    // 0/1 - hourly leaf wetness presence

    // run the hourly disease model
    public void runHourly(inputsHourly input, parameters parameters, outputs output, outputs output1)
    {
        // Add hourly data to the lists
        temp.Add(input.airTemperature);
        rh.Add(input.relativeHumidity);
        rain.Add(input.precipitation);
        lw.Add(input.leafWetness);

        // Reset dry counter if wetness present
        if (input.leafWetness == 1) output1.disease.counterDry = 0;
        else output1.disease.counterDry += 1;

        // Temperature suitability function (0–1)
        output1.disease.tempFunction = utilities.Tresponse(input.airTemperature,
            parameters.pardisease.Tmin,
            parameters.pardisease.Topt,
            parameters.pardisease.Tmax);

        // If it's the end of the day, compute daily aggregates and rates
        if (input.date.Hour == 23)
        {
            // Daily average sporulation efficiency is derived with the same function than for infection
            output1.disease.sporulationEfficiency = output1.disease.hydroThermalTimeRate / 24;

            // Infection efficiency based on wetness duration
            if (output1.disease.hydroThermalTimeRate > parameters.pardisease.WetnessDurationMinimum)
            {
                output1.disease.hydroThermalTimeInfection = output1.disease.hydroThermalTimeRate /
                    parameters.pardisease.WetnessDurationOptimum;

                if (output1.disease.hydroThermalTimeInfection >= 1)
                    output1.disease.hydroThermalTimeInfection = 1;
            }
            else //the leaf wetness duration was below the minimum threshold for infection
            {
                output1.disease.hydroThermalTimeInfection = 0;
            }

            // Normalize HTT rate and clamp it to 1 (optimal conditions for the pathogen)
            output1.disease.hydroThermalTimeRate /= parameters.pardisease.WetnessDurationOptimum;
            if (output1.disease.hydroThermalTimeRate >= 1) output1.disease.hydroThermalTimeRate = 1;

            // Update hydrothermal time
            output1.disease.hydroThermalTimeState =
                output.disease.hydroThermalTimeState + output1.disease.hydroThermalTimeRate;

            // Interrupt infection progression if dry period is above the critical threshold (hours)
            if (output1.disease.counterDry > parameters.pardisease.DryCriticalInterruption)
                output1.disease.hydroThermalTimeInfection = 0;

            // Assign daily weather summary values (just to have them in the output)
            output1.disease.tmaxDaily = temp.Max();
            output1.disease.tminDaily = temp.Min();
            output1.disease.rhmaxDaily = rh.Max();
            output1.disease.rhminDaily = rh.Min();
            output1.disease.rainDaily = rain.Sum();      // mm total rainfall
            output1.disease.lwDaily = lw.Sum();          // hours with leaf wetness

            // Clear for next day
            temp.Clear(); rh.Clear(); rain.Clear(); lw.Clear();
        }
        else // hourly loop
        {
            // Update infection rate if within wet period
            if (output1.disease.counterDry <= parameters.pardisease.DryCriticalInterruption)
            {
                //compute the relative humidity function
                output1.disease.rhFunction = rhFunction(input.relativeHumidity, input.leafWetness,
                    parameters.pardisease.RelativeHumidityNotLimiting,
                    parameters.pardisease.RelativeHumidityCritical);

                //compute the hydrothermal time rate
                output1.disease.hydroThermalTimeRate +=
                output1.disease.tempFunction * output1.disease.rhFunction;
            }
            else // dry period, no humidity function computed
            {
                output1.disease.rhFunction = 0;
            }
        }
    }

    // RH response function based on critical thresholds and leaf wetness
    private float rhFunction(float rh, float lw, float rhLW, float rhCrit)
    {
        float rhFunction = 0;
        float rhMid = (rhCrit + rhLW) / 2f;
        float k = 10f / (rhLW - rhCrit); // steepness

        if (lw == 1) return 1;
        if (rh <= rhCrit) return 0;
        if (rh >= rhLW) return 1;

        return 1f / (1f + (float)Math.Exp(-k * (rh - rhMid)));
    }

    // Tracks tissue states through SEIR disease progression
    private List<TissueState> tissueTracking = new List<TissueState>();

    // Run the daily SEIR progression and tissue updates
    public void runDaily(inputsDaily input, parameters parameters, outputs output, outputs output1)
    {
        //local copy 
        var par = parameters.pardisease;
        //updated phenocode
        float phenocode = output1.crop.phenoCode;

        // --- New infection logic ---
        //if the cumulated hydrothermal time is above the threshold and the plant starts to be susceptible
        if (output.disease.hydroThermalTimeState >= par.HydroThermalTimeOnset &&
        output.crop.cycleCompletionPercentage >= par.CyclePercentageOnset)
        {
            if (output.disease.isPrimaryInoculumStarted == false)
            {
                output1.disease.firstSeasonalInfection = input.date;
                output1.disease.cyclePercentageFirstInfection = output.crop.cycleCompletionPercentage;
                output1.disease.isPrimaryInoculumStarted = true;
            }

            //update tissue tracking states based on updated state values from the crop model
            //this is used to adjust the disease severity according to lightInterception growth rate
            float lightToday = output1.crop.lightInterceptionAttainable;
            float lightYesterday = output.crop.lightInterceptionAttainable;

            if (lightToday > 0)
            {
                if (output.disease.affectedSum <= 1 && output1.crop.senescenceStarted == false)
                {
                    for (int i = 0; i < tissueTracking.Count; i++)
                    {
                        var tissue = tissueTracking[i];
                        tissue.latentState = (tissue.latentState * lightYesterday) / lightToday;
                        tissue.sporulatingState = (tissue.sporulatingState * lightYesterday) / lightToday;
                        tissue.deadState = (tissue.deadState * lightYesterday) / lightToday;
                    }
                }
            }

            //primary inoculum model
            output1.disease.outerInoculum = inoculumModel(input, parameters, output, output1) *
               (1f - output1.fungicide.FungicideEfficacy);

            //initialize latent value
            float latentValue = 0f;

            // Calculate detachment and external/internal inoculum
            float sporulationDetachmentEfficiency = 0;
            if (parameters.pardisease.IsSplashBorne == 1)
            {
                //potential is used because the sporulating tissue is there
                sporulationDetachmentEfficiency = 1 - 
                    (utilities.RainDetachment(input.Precipitation, par.Rain50Detachment,
                output1.crop.lightInterceptionAttainable));
            }

            float externalInfection = 
                output.disease.hydroThermalTimeInfection * output1.disease.outerInoculum;
            float internalInfection = output.disease.sporulatingSum *
                output.disease.sporulationEfficiency * par.PathogenSpread * (1 - sporulationDetachmentEfficiency);

            // Combine infection sources adjusted by resistance and healthy tissue
            latentValue = ((externalInfection + internalInfection) *
                output.disease.susceptibleFraction * output1.crop.lightInterceptionAttainable *
               (1f - parameters.parcrop.VarietalResistance)) * 
               (1f - output1.fungicide.FungicideEfficacy);

            //add a tissue state only when conditions are favorable for infection
            if (latentValue > 0)
            {
                tissueTracking.Add(new TissueState { latentState = latentValue });
            }
        }

        // --- Progression through SEIR states based on GDD ---
        float Tave = (input.Tmin + input.Tmax) / 2f;
        float gddDisease = utilities.Tresponse(Tave, par.Tmin, par.Topt, par.Tmax);
        float latProgress = gddDisease / par.LatencyDuration;
        float spoProgress = gddDisease / par.SporulationDuration;

        for (int i = 0; i < tissueTracking.Count; i++)
        {
            var tissue = tissueTracking[i];
            if (tissue.deadState == 1) continue;

            tissue.latentCounter += latProgress *
               (1f - output1.fungicide.FungicideEfficacy);

            if (tissue.latentCounter >= 1f && tissue.latentState > 0)
            {
                tissue.sporulatingState = tissue.latentState;
                tissue.latentState = 0f;
            }

            if (tissue.sporulatingState > 0)
                tissue.sporulatingCounter += spoProgress *
               (1f - output1.fungicide.FungicideEfficacy);

            if (tissue.sporulatingCounter >= 1f && tissue.sporulatingState > 0)
            {
                tissue.deadState = tissue.sporulatingState;
                tissue.sporulatingState = 0f;
            }

            tissueTracking[i] = tissue;
        }

        // --- Aggregate tissue states ---
        float latentSum = tissueTracking.Sum(t => t.latentState); if (latentSum > 1f) latentSum = 1f;
        float sporSum = tissueTracking.Sum(t => t.sporulatingState); if (sporSum > 1f) sporSum = 1f;
        float deadSum = tissueTracking.Sum(t => t.deadState); if (deadSum > 1f) deadSum = 1f;

        //compute total affected tissue
        float affectedSum = latentSum + sporSum + deadSum; if (affectedSum > 1f) affectedSum = 1f;
        //compute disease severity (visible damage)
        float diseaseSeverity = sporSum + deadSum; if (diseaseSeverity > 1f) diseaseSeverity = 1f;

        //update state variables
        output1.disease.latentSum = latentSum;
        output1.disease.sporulatingSum = sporSum;
        output1.disease.deadSum = deadSum;
        output1.disease.affectedSum = affectedSum;
        output1.disease.diseaseSeverity = diseaseSeverity;

        // --- Update susceptible tissue fraction ---
        float tissueAvailability = 1f - (output1.disease.affectedSum);
        float susceptibleFraction = Math.Clamp(tissueAvailability, 0f, 1f);

        //growth phase
        if (output1.crop.lightInterceptionAttainable >= output.crop.lightInterceptionAttainable &&
            !output1.crop.senescenceStarted)
        {
            output1.disease.susceptibleFraction = susceptibleFraction;
        }
        else //senescence phase, need to consider also crop senescence
        {
            output1.disease.susceptibleFraction = susceptibleFraction -
                ((output1.crop.fIntPeak - output1.crop.lightInterceptionAttainable) / output1.crop.fIntPeak);
        }
        output1.disease.susceptibleFraction = Math.Clamp(output1.disease.susceptibleFraction, 0f, 1f);

        // --- Reset if no green tissue remains ---
        float fIntHealthy = output1.crop.lightInterceptionAttainable;
        if (fIntHealthy == 0)
        {
            output1.disease.latentSum = 0;
            output1.disease.sporulatingSum = 0;
            output1.disease.deadSum = 0;
            output1.disease.affectedSum = 0;
        }
    }

    //inoculum model 
    float inoculumModel(inputsDaily input, parameters parameters, outputs output, outputs output1)
    {
        float outerInoculum = 0;

        //constant
        if(parameters.pardisease.OuterInoculumShapeRelease==0)
        {
            outerInoculum = parameters.pardisease.OuterInoculumMax;
        }
        //bell-shaped
        else if (parameters.pardisease.OuterInoculumShapeRelease == 1)
        {
            //model for outer inoculum
            float inoSlope = parameters.pardisease.OuterInoculumShapeParameter;
            float inoMax = output1.disease.cyclePercentageFirstInfection + (100 - output1.disease.cyclePercentageFirstInfection) / 2;
            float inoGro = 1 / (1 + (float)Math.Exp(-inoSlope * ((output.crop.cycleCompletionPercentage - output1.disease.cyclePercentageFirstInfection) -
                .5 * (inoMax - output1.disease.cyclePercentageFirstInfection))));
            float inoDec = 1 / (1 + (float)Math.Exp(inoSlope *
                ((output.crop.cycleCompletionPercentage - (output1.disease.cyclePercentageFirstInfection + (inoMax - output1.disease.cyclePercentageFirstInfection)) -
                .5 * output1.disease.cyclePercentageFirstInfection))));
            float inoMod = Math.Min(inoGro, inoDec);

            outerInoculum = inoMod * parameters.pardisease.OuterInoculumMax;
        }
        //logistic decrease
        else if (parameters.pardisease.OuterInoculumShapeRelease == 2)
        {
            float inoSlope = parameters.pardisease.OuterInoculumShapeParameter;
            float inoMax = output1.disease.cyclePercentageFirstInfection + (100 - output1.disease.cyclePercentageFirstInfection) / 2;
            float inoDec = 1 / (1 + (float)Math.Exp(inoSlope *
                ((output.crop.cycleCompletionPercentage - inoMax))));

            outerInoculum = inoDec * parameters.pardisease.OuterInoculumMax;
        }    

        return outerInoculum;
    }
}
