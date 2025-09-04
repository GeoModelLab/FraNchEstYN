//this file contains the input, output and parameter classes

#region input classes
//hourly input class
public class inputsHourly
{
        public DateTime date { get; set; }                      // timestamp of the observation (local time)
        public float airTemperature { get; set; }               // °C - air temperature
        public float precipitation { get; set; }                // mm - hourly precipitation
        public float relativeHumidity { get; set; }             // % - relative humidity
        public float leafWetness { get; set; }                  // 0/1 - leaf dry or wet
        public float rad { get; set; }                          // MJ m⁻² h⁻¹ - solar radiation
        public double disIdeotypePotentialRate { get; set; }    // unitless - potential growth rate of ideotype
        public float latitude { get; set; }                     // degrees - site latitude
        public DateTime DateTreatmentLast { get; set; }         // date of the last fungicide treatment (for efficacy calculation)

}

//daily input class
public class inputsDaily
{
    public DateTime date { get; set; }                      // date of observation
    public float Tmax { get; set; }                         // °C - daily maximum temperature
    public float Tmin { get; set; }                         // °C - daily minimum temperature
    public float Rad { get; set; }                          // MJ m⁻² d⁻¹ - daily solar radiation
    public float RHx { get; set; }                          // % - maximum relative humidity
    public float RHn { get; set; }                          // % - minimum relative humidity
    public float Precipitation { get; set; }                // mm - daily total precipitation
    public float LeafWetness { get; set; }                  // hours - leaf wetness hours
    public float DewPoint { get; set; }                     // °C - dew point temperature
    public float Latitude { get; set; }                     // degrees - site latitude
    public DateTime DateTreatmentLast { get; set; }         // date of the last fungicide treatment
    public cropModelData cropModelData { get; set; }        // output data from a generic crop model
}

//output from a generic crop model, input for damage mechanisms
public class cropModelData
{
    public Dictionary<DateTime, float> fInt = new Dictionary<DateTime, float>();  // unitless (0-1) - light interception
    public Dictionary<DateTime, float> yield = new Dictionary<DateTime, float>(); // kg ha-1 - dynamic crop yield
    public Dictionary<DateTime, float> agb = new Dictionary<DateTime, float>();   // kg ha-1 - dynamic aboveground biomass
    public Dictionary<DateTime, float> cyclePercentage = new Dictionary<DateTime, float>(); // % - percentage of completion of the crop cycle
}
#endregion

#region output classes

//output data structure (instance of the output classes)
public class outputs
{
    public disease disease = new disease(); // epidemiology output
    public crop crop = new crop();          // crop growth output
    public inputsDaily inputsDaily = new inputsDaily();    // daily input snapshot
    public fungicide fungicide = new fungicide(); // fungicide variables
}

//disease outputs
public class disease
{
    public float counterDry { get; set; }                   // hours - duration since last wet period
    public float tempFunction { get; set; }                 // unitless - temperature suitability function
    public float rhFunction { get; set; }                   // unitless - relative humidity suitability function
    public float hydroThermalTimeState { get; set; }        // hydro-degree days - accumulated progress
    public float hydroThermalTimeRate { get; set; }         // hydro-degree days/day - current infection rate
    public float hydroThermalTimeInfection { get; set; }    // hydro-degree days - infection accumulation
    public float sporulationEfficiency { get; set; }        // % - efficiency of spore production
    public float tmaxDaily { get; set; }                    // °C - daily max temperature
    public float tminDaily { get; set; }                    // °C - daily min temperature
    public float rainDaily { get; set; }                    // mm - daily rainfall
    public float rhmaxDaily { get; set; }                   // % - max daily RH
    public float rhminDaily { get; set; }                   // % - min daily RH
    public float lwDaily { get; set; }                      // % - daily leaf wetness

    public TissueState TissueState = new TissueState();     // state of tissue damage
    public float latentSum { get; set; }                    // % - cumulative latent tissue
    public float sporulatingSum { get; set; }               // % - cumulative sporulating tissue
    public float deadSum { get; set; }                      // % - cumulative dead tissue
    public float affectedSum { get; set; }                  // % - cumulative affected tissue
    public float diseaseSeverity { get; set; }              // % - disease severity
    public float susceptibleFraction { get; set; }          // fraction - healthy susceptible tissue

    public damageMechanisms damageMechanisms = new damageMechanisms(); // damage impact
    public DateTime firstSeasonalInfection { get; set; } //first seasonal infection
    public float cyclePercentageFirstInfection { get; set; }   // percentage of crop cycle at the first infection
    public bool isPrimaryInoculumStarted { get; set; } //determines if primary inoculum contributes to the epidemic
    public float outerInoculum { get; set; } //level of outer inoculum
}

//damage mechanism outputs
public class damageMechanisms
{
    public float lightStealers { get; set; }                // fraction - reduction of light interception
    public float senescenceAccelerators { get; set; }       // fraction - increase in tissue aging
    public float RUEreducers { get; set; }                  // fraction - reduction in radiation use efficiency
    public float assimilateSappers { get; set; }            // kg ha-1 - assimilate loss due to disease
}

//host tissue state
public class TissueState
{
    public float latentState = 0f;                          // fraction - currently latent tissue
    public float sporulatingState = 0f;                     // fraction - currently sporulating tissue
    public float deadState = 0f;                            // fraction - currently dead tissue
    public float latentCounter = 0f;                        // fraction - accumulation in latent phase, when 1 the tissue becomes sporulating
    public float sporulatingCounter = 0f;                   // days - accumulation in sporulating phase, when 1 the tissue becomes dead
}
//crop outputs
public class crop
{
    public int growingSeason { get; set; }                  // identifier of season
    public int dayAfterSowing { get; set; }                 // days since sowing
    public float phenoCode { get; set; }                    // unitless - phenological code
    public float growingDegreeDays { get; set; }            // degree days - crop thermal time
    public float agbAttainable { get; set; }                 // kg ha⁻² - attainable above-ground biomass
    public float agbActual { get; set; }                   // kg ha⁻² - actual above-ground biomass
    public float yieldAttainable { get; set; }              // kg ha⁻² - attainable yield
    public float yieldActual { get; set; }                  // kg ha⁻² - actual yield
    public float lightInterceptionAttainable { get; set; }   // fraction - attainable light interception
    public float lightInterceptionActual { get; set; }     // fraction - actual light interception
    public float cycleCompletionPercentage { get; set; }    // % - % of crop cycle completed
    public bool senescenceStarted { get; set; }             //true if senescence started
    public float fIntPeak { get; set; }                     //fraction - peak of light interception
}

public class fungicide
{
    public float FungicideConcentrationFactor { get; set; } // fraction - fungicide concentration
    public float FungicideTenacityFunction { get; set; }    // unitless - fungicide tenacity function
    public float FungicideTenacity { get; set; }            // fraction - state variable for fungicide tenacity
    public float FungicideActualDegradation { get; set; }   // fraction - fungicide actual degradation (1 = not degradated) 
    public float FungicidePotentialDegradation { get; set; }// fraction - fungicide potential degradation (1 = not degradated)
    public float FungicideEfficacy { get; set; }            // fraction - fungicide efficacy
}

#endregion

#region parameter classes

//instance of parameter classes
public class parameters
{
    public pardisease pardisease { get; set; } = new pardisease(); // disease model parameters
    public parcrop parcrop { get; set; } = new parcrop();   // crop model parameters
    public parfungicide parfungicide { get; set; } = new parfungicide(); // fungicide parameters
}

//disease parameter
public class pardisease
{
    public float OuterInoculumMax { get; set; }             // unitless - initial inoculum level
    public float OuterInoculumShapeRelease { get; set; }    // 0-1-2 - 0 = constant inoculum, 1 = bell-shaped, 2 = logistic decrease....
    public float OuterInoculumShapeParameter { get; set; }  // if outer inoculum shape release is 1 or 2, it is the empirical parameter of the function
    public float PathogenSpread { get; set; }               // unitless - pathogen dispersal potential
    public float WetnessDurationOptimum { get; set; }       // hours - optimal leaf wetness duration
    public float WetnessDurationMinimum { get; set; }       // hours - minimum required wetness
    public float DryCriticalInterruption { get; set; }      // hours - dry spell disrupting infection
    public float Rain50Detachment { get; set; }             // mm - rain needed for spore detachment
    public float CyclePercentageOnset { get; set; }         // % - % of crop cycle when the disease can start
    public float Tmin { get; set; }                         // °C - minimum temperature for pathogen
    public float Topt { get; set; }                         // °C - optimal temperature for pathogen
    public float Tmax { get; set; }                         // °C - maximum temperature for pathogen
    public float RelativeHumidityCritical { get; set; }     // % - minimum RH for infection
    public float RelativeHumidityNotLimiting { get; set; }  // % - RH above which it's not limiting
    public float HydroThermalTimeOnset { get; set; }        // hydro-degree days - infection onset threshold
    public float LatencyDuration { get; set; }              // days - duration of latency period
    public float SporulationDuration { get; set; }          // days - duration of sporulation
    public float LightStealerDamage { get; set; }           // unitless - lesion size representation
    public float RUEreducerDamage { get; set; }             // fraction - RUE reduction due to disease
    public float SenescenceAcceleratorDamage { get; set; }  // fraction - max rate of senescence acceleration
    public float AssimilateSappersDamage { get; set; }      // kg ha⁻² - max loss to assimilate sappers
    public float IsSplashBorne { get; set; }                // 0–1 - 1 for pathogens which are only released by rain splash
}

//crop parameters
public class parcrop
{
    public float TbaseCrop { get; set; }                    // °C - base temp for crop growth
    public float ToptCrop { get; set; }                     // °C - optimum temp for crop growth
    public float TmaxCrop { get; set; }                     // °C - maximum temp for crop growth
    public float CycleLength { get; set; }                  // degree days - total crop cycle
    public float FloweringStart { get; set; }               // % - crop cycle % for flowering start
    public float HalfIntGrowth { get; set; }                // % - cycle % when light interception halves (growth)
    public float HalfIntSenescence { get; set; }            // % - cycle % when light interception halves (senescence)
    public float SlopeGrowth { get; set; }                  // unitless - slope of light interception curve (growth)
    public float SlopeSenescence { get; set; }              // unitless - slope of light interception curve (senescence)
    public float RadiationUseEfficiency { get; set; }       // g MJ⁻¹ - radiation use efficiency
    public float PartitioningMaximum { get; set; }          // unitless - max partitioning to yield
    public float VarietalResistance { get; set; }           // 0–1 - varietal resistance level to pathogens
}

//fungicide parameters
public class parfungicide
{
    public float AShapeParameter { get; set; }              // unitless - empirical shape parameter for fungicide degradation
    public float BShapeParameter { get; set; }              // unitless - empirical shape parameter for fungicide degradation
    public float DegradationRate { get; set; }              // unitless - empirical degradation rate parameter
    public float InitialDose { get; set; }                  // fraction - initial dose of the fungicide, 1 means the maximum dose
    public float InitialEfficacy { get; set; }              // fraction - initial fungicide efficacy, 1 means maximum efficacy
    public float TenacityFactor { get; set; }               // unitless - empirical parameter regulating the degradation of the fungicide due to precipitation
}
#endregion