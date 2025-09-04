// This is the simplified crop model simulating growth and damage effects for a single timestep.
public class cropModel
{
    //this method computes the damage mechanisms and then either runs the internal crop model or modify the outputs of an external crop model
    public void run(inputsDaily input, parameters parameters, outputs output, outputs output1)
    {

        #region damage mechanisms
        // --- Disease feedback and damage mechanisms ---
        float diseaseImpact = output.disease.diseaseSeverity; // fraction of affected tissue (0–1)

        //light stealers
        output1.disease.damageMechanisms.lightStealers =
            (1 - LightStealersFunction(diseaseImpact, parameters.pardisease.LightStealerDamage));

        //rue reducers
        output1.disease.damageMechanisms.RUEreducers =
            (1 - RUEreductionFunction(diseaseImpact, parameters.pardisease.RUEreducerDamage));

        //assimilate sappers
        output1.disease.damageMechanisms.assimilateSappers =
            AssimilateSappersFunction(diseaseImpact, parameters.pardisease.AssimilateSappersDamage);

        //senescence accelerators
        output1.disease.damageMechanisms.senescenceAccelerators =
                        SenescenceAcceleratorFunction(diseaseImpact, parameters.pardisease.SenescenceAcceleratorDamage);
        #endregion


        //FraNchEstYN internal crop model
        if (input.cropModelData.fInt.Keys.Count==0)
        {
            // --- GDD calculation ---
            // Compute temperature response function (0–1) based on average temperature
            float Tfunc = utilities.Tresponse((input.Tmin + input.Tmax) / 2,
                parameters.parcrop.TbaseCrop, parameters.parcrop.ToptCrop, parameters.parcrop.TmaxCrop);

            // Accumulate growing degree days (°C·days)
            output1.crop.growingDegreeDays = output.crop.growingDegreeDays +
                Tfunc * (parameters.parcrop.ToptCrop - parameters.parcrop.TbaseCrop);

            // --- Crop cycle progress ---
            if (output.crop.cycleCompletionPercentage <= 100)
            {
                // Determine phenological stage (e.g., 1 = vegetative, 2 = reproductive)
                output1.crop.phenoCode = PhenocodeFunction(output1.crop.growingDegreeDays,
                    parameters.parcrop.FloweringStart / 100 * parameters.parcrop.CycleLength);

                // Compute potential light interception (fraction, 0–1) and senescence started
                var fIntResult = fIntCompute(
                    parameters.parcrop.CycleLength,
                    parameters.parcrop.SlopeGrowth,
                    parameters.parcrop.HalfIntGrowth,
                    parameters.parcrop.SlopeSenescence,
                    parameters.parcrop.HalfIntSenescence,
                    output1.crop.growingDegreeDays);

                //assign variables to the outputs
                output1.crop.lightInterceptionAttainable = fIntResult.fInt;
                output1.crop.senescenceStarted = fIntResult.senescenceStarted;

                //save the peak of light interception
                if (output1.crop.senescenceStarted &&
                    output1.crop.fIntPeak == 0)
                {
                    output1.crop.fIntPeak = output.crop.lightInterceptionAttainable;
                }

                // Shift senescence phase due to senescence accelerators
                float halfIntSenescenceShifted = parameters.parcrop.HalfIntSenescence - 
                    output1.disease.damageMechanisms.senescenceAccelerators * 100;

                // Compute healthy (actual) light interception under disease pressure
                output1.crop.lightInterceptionActual = fIntCompute(parameters.parcrop.CycleLength,
                    parameters.parcrop.SlopeGrowth, parameters.parcrop.HalfIntGrowth,
                    parameters.parcrop.SlopeSenescence, halfIntSenescenceShifted,
                    output1.crop.growingDegreeDays).fInt;

                //reduce light interception due to light stealers
                output1.crop.lightInterceptionActual = output1.crop.lightInterceptionActual - 
                    output1.crop.lightInterceptionActual *
                    (output1.disease.damageMechanisms.lightStealers);
                //clamp at 0
                if (output1.crop.lightInterceptionActual < 0) output1.crop.lightInterceptionActual = 0;

                // --- Biomass accumulation ---
                // rate variable (potential)
                float carbonRatePotential = CarbonRate(parameters.parcrop.RadiationUseEfficiency,
                    input.Rad, Tfunc, output1.crop.lightInterceptionAttainable) * 10; // kg/ha

                // state variable (potential)
                output1.crop.agbAttainable = output.crop.agbAttainable + carbonRatePotential; // kg/ha

                // rate variable (actual)
                float carbonRateActual = CarbonRate(parameters.parcrop.RadiationUseEfficiency -
                    parameters.parcrop.RadiationUseEfficiency * output1.disease.damageMechanisms.RUEreducers,
                    input.Rad, Tfunc, output1.crop.lightInterceptionActual) * 10; //kg/ha
                // reduce carbon rate due to assimilate sappers
                carbonRateActual = carbonRateActual - output1.disease.damageMechanisms.assimilateSappers;

                //update agb
                output1.crop.agbActual = output.crop.agbActual + carbonRateActual;
                //limit to 0
                if (output1.crop.agbActual < 0) output1.crop.agbActual = 0;

                // --- Yield accumulation ---
                output1.crop.yieldAttainable = output.crop.yieldAttainable +
                    YieldRate(output1.crop.phenoCode, carbonRatePotential, parameters.parcrop.PartitioningMaximum);

                //update yield
                output1.crop.yieldActual = output.crop.yieldActual +
                    YieldRate(output1.crop.phenoCode, carbonRateActual, parameters.parcrop.PartitioningMaximum);

                //limit to 0 
                if (output1.crop.yieldActual < 0) output1.crop.yieldActual = 0;

                // --- Days after sowing ---
                output1.crop.dayAfterSowing = output.crop.dayAfterSowing + 1;

                // --- Cycle completion update ---
                output1.crop.cycleCompletionPercentage =
                    output1.crop.growingDegreeDays / parameters.parcrop.CycleLength * 100f;

                // --- the crop is matured
                if (output1.crop.cycleCompletionPercentage > 100)
                    output1.crop.cycleCompletionPercentage = 100;
            }
            else
            {
                // If crop cycle is complete, reset outputs (optional behavior)
                output1.crop = new crop();
                output.crop = new crop();
            }
        }
        //data from another crop model are used
        else
        {
            //there are inputs from the crop model
            if (input.cropModelData.fInt.ContainsKey(input.date))
            {
                //assign the attainable variables
                output1.crop.lightInterceptionAttainable = input.cropModelData.fInt[input.date];
                output1.crop.agbAttainable = input.cropModelData.agb[input.date]; ;
                output1.crop.yieldAttainable = input.cropModelData.yield[input.date];

                //apply the light stealer mechanism
                output1.crop.lightInterceptionActual = output1.crop.lightInterceptionAttainable -
                    output1.crop.lightInterceptionAttainable * output1.disease.damageMechanisms.lightStealers;

                //determine if we are in the senescence phase (when yield is computed) and
                //apply a further mechanism for light stealers
                output1.crop.lightInterceptionActual = output1.crop.lightInterceptionActual -
                output1.disease.damageMechanisms.senescenceAccelerators;
                //clamp at 0
                if (output1.crop.lightInterceptionActual < 0) output1.crop.lightInterceptionActual = 0;

                //senescence starts when yield grows
                if (input.cropModelData.yield[input.date] > 0) output1.crop.senescenceStarted = true;

                //save the peak of light interception
                output1.crop.fIntPeak = input.cropModelData.fInt.Values.Max();

                //recompute the growth rate
                float potentialAGBrate = 0;
                float potentialFintrate = 0;
                float potentialYieldrate = 0;

                //other days
                if (input.cropModelData.agb.ContainsKey(input.date.AddDays(-1)))
                {
                    potentialAGBrate = input.cropModelData.agb[input.date] -
                        input.cropModelData.agb[input.date.AddDays(-1)];
                    potentialFintrate = input.cropModelData.fInt[input.date] -
                        input.cropModelData.fInt[input.date.AddDays(-1)];
                    potentialYieldrate = input.cropModelData.yield[input.date] -
                        input.cropModelData.yield[input.date.AddDays(-1)];
                }
                else//first day
                {
                    potentialAGBrate = input.cropModelData.agb[input.date];
                    potentialFintrate = input.cropModelData.fInt[input.date];
                    potentialYieldrate = input.cropModelData.yield[input.date];
                }

                //reinitialize variables
                float actualAGBrate = 0;
                float actualYieldrate = 0;

                //when there is light interception
                if (output1.crop.lightInterceptionAttainable > 0)
                {
                    //compute AGB and yield state variables based on
                    //proportion of light interception healthy vs. potential
                    actualAGBrate = potentialAGBrate -
                    potentialAGBrate * (output1.crop.lightInterceptionAttainable -
                    output1.crop.lightInterceptionActual) / output1.crop.lightInterceptionAttainable;

                    actualYieldrate = potentialYieldrate -
                    potentialYieldrate * (output1.crop.lightInterceptionAttainable -
                    output1.crop.lightInterceptionActual) / output1.crop.lightInterceptionAttainable;
                }
                else //first day
                {
                    actualAGBrate = output.crop.agbActual;
                    actualYieldrate = output.crop.yieldActual; 
                }

                //avoid inconsistencies when healthyFintrate < 0
                if (actualAGBrate < 0) actualAGBrate = 0;
                if (actualYieldrate < 0) actualYieldrate = 0;
                //note that this variables CAN BE < 0, see below

                //apply RUE reducer and assimilate sappers
                actualAGBrate = actualAGBrate - actualAGBrate * output1.disease.damageMechanisms.RUEreducers -
                    output1.disease.damageMechanisms.assimilateSappers;
                actualYieldrate = actualYieldrate - actualYieldrate * output1.disease.damageMechanisms.RUEreducers -
                   output1.disease.damageMechanisms.assimilateSappers;

                //update actual state variables
                output1.crop.agbActual = output.crop.agbActual + actualAGBrate;
                output1.crop.yieldActual = output.crop.yieldActual + actualYieldrate;

                //clamp at 0
                if (output1.crop.yieldActual < 0) output1.crop.yieldActual = 0;
                if (output1.crop.agbActual < 0) output1.crop.agbActual = 0;

                //assign cycle percentage
                output1.crop.cycleCompletionPercentage = input.cropModelData.cyclePercentage[input.date];
            }
            else //no more input from the crop model --> the crop is harvested
            {
                // If crop cycle is complete, reset outputs (optional behavior)
                output1.crop = new crop();
                output.crop = new crop();
            }
        }
    }

    #region Crop Growth Functions

    // Determines phenological code (e.g., 1 = vegetative, 2 = reproductive)
    public int PhenocodeFunction(float gddState, float floweringStart)
    {
        return gddState < floweringStart ? 1 : 2;
    }

    // Computes light interception using logistic functions for growth and senescence
    public (float fInt, bool senescenceStarted) fIntCompute(float cycleLength, float slopeGrowth,
        float halfIntGrowth, float slopeSenescence, float halfIntSenescence, float gdd)
    {
        float halfIntGrowthGdd = cycleLength * halfIntGrowth / 100f;
        float halfIntSenescenceGdd = cycleLength * halfIntSenescence / 100f;

        float fIntGrowth = 1f / (1f + (float)Math.Exp(-slopeGrowth * (gdd - halfIntGrowthGdd)));
        float fIntSenescence = 1f / (1f + (float)Math.Exp(slopeSenescence * (gdd - halfIntSenescenceGdd)));

        bool senescenceStarted = false;
        //trigger senescence
        if(fIntSenescence<fIntGrowth)
        {
            senescenceStarted = true;
        }

        return (Math.Min(fIntGrowth, fIntSenescence), senescenceStarted); // light interception limited by both stages
    }

    // Calculates carbon assimilation rate (g m⁻² d⁻¹)
    public float CarbonRate(float rue, float radiation, float fTemp, float fInt)
    {
        return rue * fInt * radiation * 0.5f * fTemp; // 0.5 factor accounts for effective PAR
    }

    // Computes partitioned yield increment based on phenological stage
    public float YieldRate(float phenoCode, float bioRatePot, float partitioningMaximum)
    {
        return phenoCode != 2 ? 0f : bioRatePot * partitioningMaximum;
    }

    #endregion

    #region Damage Functions

    // Simulates reduction in light interception due to visible lesion severity
    public static float LightStealersFunction(float DiseaseSeverity, float virtualVisualLesion)
    {
        return (float)Math.Pow(1f - DiseaseSeverity, virtualVisualLesion);
    }

    // Computes accelerated senescence (percentage) due to infection
    public static float SenescenceAcceleratorFunction(float DiseaseSeverity, float senescenceAccelerationMax)
    {
        return DiseaseSeverity * senescenceAccelerationMax;
    }

    // Reduces RUE (Radiation Use Efficiency) due to pathogen stress
    public static float RUEreductionFunction(float DiseaseSeverity, float RUEreductionFactor)
    {
        return (float)Math.Pow(1f - DiseaseSeverity, RUEreductionFactor);
    }

    // Simulates assimilate drain by the pathogen
    public static float AssimilateSappersFunction(float DiseaseSeverity, float assimilateSappersMax)
    {
        return DiseaseSeverity * assimilateSappersMax;
    }

    #endregion
}
