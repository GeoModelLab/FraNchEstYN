//this class perform the multi-start simplex optimization and validation of FraNchEstYN
internal class Optimizer : UNIMI.optimizer.IOBJfunc //implements the optimizer interface
{
    #region optimizer methods
        int _neval = 0;
        int _ncompute = 0;

        public Dictionary<int, string> _Phenology = new Dictionary<int, string>();

        // the number of times that this function is called        
        public int neval
        {
            get
            {
                return _neval;
            }

            set
            {
                _neval = value;
            }
        }

        // the number of times where the function is evaluated 
        // (when an evaluation is requested outside the parameters domain this counter is not incremented        
        public int ncompute
        {
            get
            {
                return _ncompute;
            }

            set
            {
                _ncompute = value;
            }
        }
        #endregion

    #region instances of data types and functions

    //instance of the model classes
    diseaseModel diseaseModel = new diseaseModel();
    cropModel cropModel = new cropModel();
    fungicideModel fungicideModel = new fungicideModel();

    //output structure
    Dictionary<string, List<outputs>> id_outputs = new Dictionary<string, List<outputs>>();

    #endregion

    #region instance of the weather and parameter reader class
    weatherReader weatherReader = new weatherReader();
    paramReader paramReader = new paramReader();
    #endregion

    #region local variables to perform the optimization
    public simulationUnit simulationUnit = new simulationUnit();
    public string site;
    public string variety;
    public string disease;
    public Dictionary<string, Dictionary<string, parameter>> species_nameParam = new Dictionary<string, Dictionary<string, parameter>>();
    public List<string> availableSites = new List<string>();
    public Dictionary<string, parameter> nameParam = new Dictionary<string, parameter>();
    public Dictionary<string, float> param_outCalibration = new Dictionary<string, float>();
    public Dictionary<DateTime, outputs> date_outputs = new Dictionary<DateTime, outputs>();
    public string weatherDir;
    public string paramFile;
    public List<string> calibratedParamNames;
    public List<string> allWeatherDataFiles;
    public bool isCalibration;
    public bool diseaseSimulation;
    public string calibrationVariable;
    public cropModelData cropModelData = new cropModelData();
    //for validation
    public int startYear;
    public int endYear;
    public Dictionary<string, Dictionary<DateTime, List<string>>> site_date_treatments = new Dictionary<string, Dictionary<DateTime, List<string>>>();
    public string weatherTimeStep;

    //internal weather lists
    List<float> Temperatures = new List<float>();
    List<float> Radiation = new List<float>();
    List<float> Precipitation = new List<float>();
    List<float> RelativeHumidities = new List<float>();
    List<float> LeafWetness = new List<float>();

    #endregion

    //this method perform the multi-start simplex calibration
    public double ObjfuncVal(double[] Coefficient, double[,] limits)
    {
        #region Calibration methods
            for (int j = 0; j < Coefficient.Length; j++)
            {
                if (Coefficient[j] == 0)
                {
                    break;
                }
                if (Coefficient[j] <= limits[j, 0] | Coefficient[j] > limits[j, 1])
                {
                    return 1E+300;
                }

            }
            _neval++;
            _ncompute++;
        #endregion

        #region Parameters assignment
        parameters parameters = new parameters();
        Type parametersTypeCalib = parameters.GetType();
        int coef = 0;

        foreach (var paramEntry in nameParam) // nameParam is assumed to be a field/property of Optimizer
        {
            string paramName = paramEntry.Key.Split("_")[1];
            var paramSpec = paramEntry.Value;

            object valueToSet;
            // Calibrated parameter: use from Coefficient array
            if (!string.IsNullOrWhiteSpace(paramSpec.calibration) && 
                (paramEntry.Key.Contains(calibrationVariable) ||
                calibrationVariable == "all"))
            {
                if (coef >= Coefficient.Length)
                    throw new IndexOutOfRangeException($"Not enough coefficients to assign to parameter '{paramName}'");

                valueToSet = Coefficient[coef];
                coef++;
            }
            else
            {
                // Fixed parameter: use defined value
                valueToSet = paramSpec.value;
            }
            bool isSet = false;

            // Search in each sub-object of the 'parameters' object (e.g., parcrop, pardisease)
            foreach (var classProp in parametersTypeCalib.GetProperties())
            {
                var classInstance = classProp.GetValue(parameters);
                if (classInstance == null) continue;

                var targetProp = classInstance.GetType().GetProperty(paramName);
                if (targetProp?.CanWrite == true)
                {
                    var converted = Convert.ChangeType(valueToSet, targetProp.PropertyType);
                    targetProp.SetValue(classInstance, converted);
                    isSet = true;
                    break;
                }

            }

            if (!isSet)
            {
                Console.WriteLine($"⚠️ Could not find property '{paramName}' in any sub-object of 'parameters'");
            }
        }

        //list of errors
        List<double> errors = new List<double>();

        #endregion

        //objective function
        double objFun = 0;

        //instance of output structure
        outputs output = new outputs();
        outputs outputT1 = new outputs();
        //instance of input structure
        inputsHourly inputs = new inputsHourly();
        //instance of model classes
        diseaseModel = new diseaseModel();
        cropModel = new cropModel();        

        //weather file
        string weatherFile = weatherDir + "//" + weatherTimeStep + "//" + site + ".csv";

        #region model execution
        // Check if the weather file exists
        if (File.Exists(weatherFile) &&
            availableSites.Contains(site))
        {
            //read weather data
            var weatherData = new Dictionary<DateTime, inputsHourly>();

            if (weatherTimeStep == "hourly")
            {
                //TODO:check
                weatherData = weatherReader.readHourly(weatherFile, startYear, endYear, site);
            }
            else if (weatherTimeStep == "daily")
            {
                weatherData = weatherReader.readDaily(weatherFile, startYear, endYear);
            }

            bool isPlanted = false;
            bool isMatured = false;
            DateTime lastTreatmentDate = new DateTime(); ;

            //loop over dates
            foreach (var hour in weatherData.Keys)
            {
                //set the simulation period: 
                if (hour.Year >= startYear && hour.Year <= endYear)
                {
                    weatherData[hour].date = hour;
                    weatherData[hour].latitude = simulationUnit.latitude;
                    if (simulationUnit.year_sowingDOY.ContainsKey(hour.Year) &&
                    hour.DayOfYear == simulationUnit.year_sowingDOY[hour.Year])
                    {
                        isPlanted = true;
                        isMatured = false;
                        output = new outputs();
                        outputT1 = new outputs();
                        diseaseModel = new diseaseModel();
                    }

                    if (isPlanted)
                    {
                        // if today there is a treatment
                        if (simulationUnit.fungicideTreatmentSchedule.Treatments.Contains(hour.Date))
                        {
                            // update last treatment
                            lastTreatmentDate = hour.Date;
                        }

                        // feed the input with the last treatment
                        weatherData[hour].DateTreatmentLast = lastTreatmentDate;

                        //call the models
                        modelCall(weatherData[hour], parameters, ref output, ref outputT1, site);

                        //check
                        if(outputT1.crop.cycleCompletionPercentage>=100)
                        {
                            isPlanted = false;
                            isMatured = true;
                        }
                    }
                    else
                    {
                        isPlanted = false;
                        output = new outputs();
                        outputT1 = new outputs();
                    }

                    #region Objective function evaluation

                    // Decide what to include
                    bool includeCrop = string.Equals(calibrationVariable, "crop", StringComparison.OrdinalIgnoreCase)
                                       || string.Equals(calibrationVariable, "all", StringComparison.OrdinalIgnoreCase);

                    bool includeDisease = string.Equals(calibrationVariable, "disease", StringComparison.OrdinalIgnoreCase)
                                          || string.Equals(calibrationVariable, "all", StringComparison.OrdinalIgnoreCase);

                    if (hour.Hour == 0)
                    {
                        double totalError = 0.0;
                        bool hasReferenceData = false; // Track if any ref data was found

                        // ------- Crop -------
                        if (includeCrop)
                        {
                            double agbError = 0.0;
                            if (simulationUnit.referenceData.date_agb.TryGetValue(hour, out float agbRef))
                            {
                                double agbSim = outputT1.crop.agbAttainable;
                                agbError = Math.Pow((agbRef - agbSim) / 200.0, 2);
                                hasReferenceData = true;
                            }

                            double yieldError = 0.0;
                            if (simulationUnit.referenceData.date_yieldAttainable.TryGetValue(hour, out float yieldRef))
                            {
                                double yieldSim = outputT1.crop.yieldAttainable;
                                yieldError = Math.Pow((yieldRef - yieldSim) / 100.0, 2);
                                hasReferenceData = true;
                                if (yieldSim == 0 && yieldRef >0  &&
                                    isMatured)
                                {
                                    yieldError *= 1000;
                                }

                            }

                            double fintError = 0.0;
                            if (simulationUnit.referenceData.date_fint.TryGetValue(hour, out float fintRef))
                            {
                                double fintSim = outputT1.crop.lightInterceptionAttainable*100;
                                double fintDiff = Math.Abs(fintRef * 100 - fintSim);
                                fintError = Math.Pow(fintDiff, 2); // Already 0–1

                                if (fintSim < 0.1 && isPlanted)
                                {
                                    fintError *= 1000;
                                }
                                hasReferenceData = true;
                            }

                            totalError += agbError + yieldError + fintError;
                        }

                        // ------- Disease -------
                        if (includeDisease)
                        {
                            double diseaseError = 0.0;

                            if (simulationUnit.referenceData.disease_date_diseaseSev.TryGetValue(disease, out var byDate)
                                && byDate.TryGetValue(hour, out float disSevRefRaw))
                            {
                                double disSevRef = disSevRefRaw; // convert % → 0–1
                                double disSevSim = outputT1.disease.diseaseSeverity*100;

                                diseaseError = Math.Pow(disSevRef - disSevSim, 2);
                                hasReferenceData = true;
                            }
                            double yieldError = 0.0;

                            if (simulationUnit.referenceData.date_yieldActual.TryGetValue(hour, out float yieldRef))
                            {
                                double yieldSim = outputT1.crop.yieldActual;
                                yieldError = Math.Pow((yieldRef - yieldSim) / 100.0, 2);
                                if (yieldSim == 0 && yieldRef > 0 &&
                                    isMatured)
                                {
                                    yieldError *= 1000;
                                }


                                hasReferenceData = true;
                            }
                            

                            totalError += diseaseError + yieldError;
                        }

                        // One combined entry per hour
                        if (hasReferenceData)
                        {
                            errors.Add(totalError);
                        }
                    }

                    #endregion
                }
                else
                {
                    output = new outputs();
                    outputT1 = new outputs();
                }
            }
        }
        else
        {
            //Console.WriteLine("The weather file {0} has not been found. Check it!",
            //    weatherFile); 
        }
        #endregion

        //compute objective function
        objFun = Math.Round(Math.Sqrt(errors.Sum() / errors.Count), 3);


        // When updating:
        Console.Write("\r\u001b[K" +  // \r = return to line start, \u001b[K = clear to end of line (ANSI)
                      $"Root Mean Square Error = {objFun}");
        Console.Out.Flush();


        //return the objective function
        return objFun;
    }

    //this method is called in the validation run
    public void oneShot(Dictionary<string, float> paramValue, out Dictionary<DateTime, outputs> date_outputs)
    {
        //reinitialize the date_outputs object
        date_outputs = new Dictionary<DateTime, outputs>();
        Dictionary<DateTime, outputs> date_outputs_hourly = new Dictionary<DateTime, outputs>();

        #region assign parameters
        //instance of parameter class
        parameters parameters = new parameters();
        Type parametersTypeCalib = parameters.GetType();

        // === 1. Apply fixed parameters from nameParam ===
        foreach (var paramEntry in nameParam)
        {
            string paramName = paramEntry.Key;
            var paramSpec = paramEntry.Value;

            if (string.IsNullOrWhiteSpace(paramSpec.calibration))
            {
                // ❌ Not calibrated → skip, it will be handled later from param_outCalibration
                continue;
            }

            // ✅ Only now try to get the value from paramValue
            if (!paramValue.TryGetValue(paramName, out float rawValue))
            {
                //Console.WriteLine($"⚠️ Calibrated parameter '{paramName}' is missing in paramValue.");
                continue;
            }
        
            object valueToSet = rawValue;
            bool isSet = false;

            foreach (var classProp in parametersTypeCalib.GetProperties())
            {
                var classInstance = classProp.GetValue(parameters);
                if (classInstance == null) continue;

                var targetProp = classInstance.GetType().GetProperty(paramName.Split("_")[1]);
                if (targetProp?.CanWrite == true)
                {
                    var converted = Convert.ChangeType(valueToSet, targetProp.PropertyType);
                    targetProp.SetValue(classInstance, converted);
                    isSet = true;
                    break;
                }
            }

            if (!isSet)
            {
                Console.WriteLine($"⚠️ Could not set calibrated parameter '{paramName}' in any sub-object.");
            }
        }

        // === 2. Apply fixed parameters from param_outCalibration ===
        foreach (var param in param_outCalibration.Keys)
        {
            object valueToSet = param_outCalibration[param];
            bool isSet = false;

            foreach (var classProp in parametersTypeCalib.GetProperties())
            {
                var classInstance = classProp.GetValue(parameters);
                if (classInstance == null) continue;

                var targetProp = classInstance.GetType().GetProperty(param.Split("_")[1]);
                if (targetProp?.CanWrite == true)
                {
                    var converted = Convert.ChangeType(valueToSet, targetProp.PropertyType);
                    targetProp.SetValue(classInstance, converted);
                    isSet = true;
                    break;
                }
            }

            if (!isSet)
            {
                Console.WriteLine($"⚠️ Could not find fixed parameter '{param}' in any sub-object of 'parameters'");
            }
        }
        
        #endregion

        #region model execution
        //weather file
        string weatherFile = weatherDir + "//" + weatherTimeStep + "//" + site + ".csv";

        // Check if the weather file exists
        if (File.Exists(weatherFile) && availableSites.Contains(site))
        {
            //read weather data
            Dictionary<DateTime, inputsHourly> weatherData = new Dictionary<DateTime, inputsHourly>();

            if (weatherTimeStep == "hourly")
            {
                weatherData = weatherReader.readHourly(weatherFile, startYear, endYear, site);
            }
            else if (weatherTimeStep == "daily")
            {
                weatherData = weatherReader.readDaily(weatherFile, startYear, endYear);
            }

            //reinitialize the date_outputs object
            date_outputs = new Dictionary<DateTime, outputs>();
            date_outputs_hourly = new Dictionary<DateTime, outputs>();
            bool isPlanted = false;

            //reinstantiate output structures
            outputs output = new outputs();
            outputs outputT1 = new outputs();
            //reinstantiate model classes
            diseaseModel = new diseaseModel();
            cropModel = new cropModel();
            DateTime lastTreatmentDate = new DateTime(); ;

            #region model call
            //loop over dates
            foreach (var hour in weatherData.Keys)
            {
                //set the simulation period: 
                if (hour.Year >= startYear && hour.Year <= endYear)
                {
                   
                    weatherData[hour].date = hour;
                    weatherData[hour].latitude = simulationUnit.latitude;
                    if (simulationUnit.year_sowingDOY.ContainsKey(hour.Year) && 
                        hour.DayOfYear == simulationUnit.year_sowingDOY[hour.Year])
                    {
                        isPlanted = true;
                        output = new outputs();
                        outputT1 = new outputs();
                        diseaseModel = new diseaseModel();
                        outputT1.crop.growingSeason = hour.Year;
                    }

                    if (isPlanted)
                    {
                        // if today there is a treatment
                        if (simulationUnit.fungicideTreatmentSchedule.Treatments.Contains(hour.Date))
                        {
                            // update last treatment
                            lastTreatmentDate = hour.Date;
                        }

                        // feed the input with the last treatment
                        weatherData[hour].DateTreatmentLast = lastTreatmentDate;

                        //call the models
                        modelCall(weatherData[hour], parameters, ref output, ref outputT1, site);
                        if (outputT1.crop.cycleCompletionPercentage >= 100 ||
                            outputT1.crop.dayAfterSowing >= 11*30)//reinitialize after 11 months
                        {
                            isPlanted = false;
                        }
                        date_outputs_hourly.Add(hour, outputT1);
                        if (hour.Hour == 23)
                        {
                            //add the object to the output dictionary
                            date_outputs.Add(hour, outputT1);
                        }
                    }
                    else
                    {
                        isPlanted = false;
                        output = new outputs();
                        outputT1 = new outputs();
                    }
                }
                else
                {
                    output = new outputs();
                    outputT1 = new outputs();
                }
            }
            #endregion

            #region write outputs file

            //write the outputs from the calibration run
            writeOutputsCalibration(site, date_outputs, simulationUnit);
            #endregion
        }
        #endregion
    }

    // this method calls the models 
    public void modelCall(inputsHourly input, parameters parameters, ref outputs output, ref outputs outputT1, string site)
    {
        //add hourly variables to the list
        Temperatures.Add(input.airTemperature);
        Radiation.Add(input.rad);
        Precipitation.Add(input.precipitation);
        RelativeHumidities.Add(input.relativeHumidity);
        LeafWetness.Add(input.leafWetness);

        //run the disease model at hourly time step
        if (calibrationVariable != "crop" || !isCalibration)
        {
            diseaseModel.runHourly(input, parameters, output, outputT1);
        }

        //run the SEIR model, the crop model and eventually the fungicide model at the last hour of the day
        if (input.date.Hour == 23)
        {
            //pass the output from the previous time step
            output = outputT1;
            outputT1 = new outputs();
            outputT1.crop.growingSeason = output.crop.growingSeason;
            outputT1.crop.fIntPeak = output.crop.fIntPeak;
            outputT1.disease.isPrimaryInoculumStarted = output.disease.isPrimaryInoculumStarted;
            outputT1.disease.firstSeasonalInfection = output.disease.firstSeasonalInfection;
            outputT1.disease.cyclePercentageFirstInfection = output.disease.cyclePercentageFirstInfection;

            //daily weather input
            float Tmax = (float)Temperatures.Max();
            float Tmin = (float)Temperatures.Min();
            inputsDaily inputDaily = new inputsDaily();
            inputDaily.Tmax = Tmax;
            inputDaily.Tmin = Tmin;
            inputDaily.Rad = Radiation.Sum();
            inputDaily.Precipitation = Precipitation.Sum();
            inputDaily.RHx = RelativeHumidities.Max();
            inputDaily.RHn = RelativeHumidities.Min();
            inputDaily.LeafWetness = LeafWetness.Sum();
            //this is to adjust the treatment date
            inputDaily.date = input.date.AddHours(-23);
            if (input.DateTreatmentLast.Year > 1)
            {
                inputDaily.DateTreatmentLast = input.DateTreatmentLast;
            }
            //pass crop model data
            inputDaily.cropModelData = cropModelData;

            //call the models
            cropModel.run(inputDaily, parameters, output, outputT1);
            fungicideModel.fungicideRun(inputDaily, parameters, output, outputT1);  
            diseaseModel.runDaily(inputDaily, parameters, output, outputT1);

            //clean the hourly list
            Temperatures.Clear();
            Radiation.Clear();
            Precipitation.Clear();
            RelativeHumidities.Clear();
            LeafWetness.Clear();

            //pass the input data structure to outputs
            outputT1.inputsDaily = inputDaily;
        }     
    }

    #region write output files from calibration and validation
    //write outputs from the calibration run
    public void writeOutputsCalibration(string site,
        Dictionary<DateTime, outputs> date_outputs, simulationUnit simulationUnit)
    {

        #region write outputs
        //empty list to store outputs
        List<string> toWrite = new List<string>();

        //define the file header
        string[] headerFields = {
            // Base inputs
            "Site", "Variety", "Date", "Year", "Doy", "GrowingSeason", "DaysAfterSowing",
            "Tmax", "Tmin", "RHmax", "RHmin", "LW", "Prec", "Rad",
            // Phenology model outputs
            "GrowingDegreeDays", "PhenoCode", "CyclePercentage", "LightInterception", "LightInterceptionRef",
            // Crop production
            "AGBattainable", "AGBRef", "YieldAttainable", "YieldRef", "HTtimeRinoculum", "HTtimeSinoculum",
            // Disease progression
            "OuterInoculum, FirstInfection, HTtimeRinfection, Susceptible", "Latent", "Sporulating", "Dead", "Affected", "DiseaseSeverity", "DiseaseSeverityRef",
            // Healthy plant metrics
            "LightIntHealthy", "AGBactual", "YieldActual", "YieldActualRef",
            // Stress factors
            "LightStealers", "SenAccelerator", "RUEReducers", "AssimilateSappers",
            //fungicide outputs
             "FungicidePotentialDegradation,FungicideActualDegradation,FungicideTenacityFunction," +
             "FungicideConcentrationFactor,FungicideEfficacy"
        };

        string header = string.Join(",", headerFields);


        //add the header to the list
        toWrite.Add(header);

        //loop over days
        foreach (var date in date_outputs.Keys)
        {
            //empty string to store outputs
            string line = "";

            //populate this line
            line += site + ",";
            line += variety + ",";
            line += date.Date.ToShortDateString() + ",";
            line += date.Date.Year + ",";
            line += date.Date.DayOfYear + ",";
            line += date_outputs[date].crop.growingSeason + ",";
            line += date_outputs[date].crop.dayAfterSowing + ",";
            line += date_outputs[date].inputsDaily.Tmax + ",";
            line += date_outputs[date].inputsDaily.Tmin + ",";
            line += date_outputs[date].inputsDaily.RHx + ",";
            line += date_outputs[date].inputsDaily.RHn + ",";
            line += date_outputs[date].inputsDaily.LeafWetness + ",";
            line += date_outputs[date].inputsDaily.Precipitation + ",";
            line += date_outputs[date].inputsDaily.Rad + ",";
            //phenology
            line += date_outputs[date].crop.growingDegreeDays + ",";
            line += date_outputs[date].crop.phenoCode + ",";
            line += date_outputs[date].crop.cycleCompletionPercentage + ",";
            line += date_outputs[date].crop.lightInterceptionAttainable + ",";
            if (simulationUnit.referenceData.date_fint.ContainsKey(date.AddHours(-23)))
            {
                line += simulationUnit.referenceData.date_fint[date.AddHours(-23)] + ",";
            }
            else
            {
                line += ",";
            }
            line += date_outputs[date].crop.agbAttainable + ",";
            if (simulationUnit.referenceData.date_agb.ContainsKey(date.AddHours(-23)))
            {
                line += simulationUnit.referenceData.date_agb[date.AddHours(-23)] + ",";
            }
            else
            {
                line += ",";
            }
            line += date_outputs[date].crop.yieldAttainable + ",";
            if (simulationUnit.referenceData.date_yieldAttainable.ContainsKey(date.AddHours(-23)))
            {
                line += simulationUnit.referenceData.date_yieldAttainable[date.AddHours(-23)] + ",";
            }
            else
            {
                line += ",";
            }
            line += date_outputs[date].disease.hydroThermalTimeRate + ",";
            line += date_outputs[date].disease.hydroThermalTimeState + ",";
            line += date_outputs[date].disease.outerInoculum + ",";
            if (date_outputs[date].disease.firstSeasonalInfection.Year == 1)
            {
                line += 0 + ",";
            }
            else
            {
                line += date_outputs[date].disease.firstSeasonalInfection + ",";
            }
            line += date_outputs[date].disease.hydroThermalTimeInfection + ",";
            line += date_outputs[date].disease.susceptibleFraction + ",";
            line += date_outputs[date].disease.latentSum + ",";
            line += date_outputs[date].disease.sporulatingSum + ",";
            line += date_outputs[date].disease.deadSum + ",";
            line += date_outputs[date].disease.affectedSum + ",";
            line += date_outputs[date].disease.diseaseSeverity + ",";
            if (!String.IsNullOrEmpty(disease))
            {
                if (simulationUnit.referenceData.disease_date_diseaseSev.ContainsKey(disease) &&
                    simulationUnit.referenceData.disease_date_diseaseSev[disease].ContainsKey(date.AddHours(-23)))
                {
                    line += simulationUnit.referenceData.disease_date_diseaseSev[disease][date.AddHours(-23)] / 100 + ",";
                }
                else
                {
                    line += ",";
                }
            }
            else
            {
                line += ",";
            }
            line += date_outputs[date].crop.lightInterceptionActual + ",";
            line += date_outputs[date].crop.agbActual + ",";
            line += date_outputs[date].crop.yieldActual + ",";
            if (simulationUnit.referenceData.date_yieldActual.ContainsKey(date.AddHours(-23)))
            {
                line += simulationUnit.referenceData.date_yieldActual[date.AddHours(-23)] + ",";
            }
            else
            {
                line += ",";
            }
            line += date_outputs[date].disease.damageMechanisms.lightStealers + ",";
            line += date_outputs[date].disease.damageMechanisms.senescenceAccelerators + ",";
            line += date_outputs[date].disease.damageMechanisms.RUEreducers + ",";
            line += date_outputs[date].disease.damageMechanisms.assimilateSappers + ",";
            line += date_outputs[date].fungicide.FungicidePotentialDegradation + ",";
            line += date_outputs[date].fungicide.FungicideActualDegradation + ",";
            line += date_outputs[date].fungicide.FungicideTenacityFunction + ",";
            line += date_outputs[date].fungicide.FungicideConcentrationFactor + ",";
            line += date_outputs[date].fungicide.FungicideEfficacy;

            //add the line to the list
            toWrite.Add(line);
        }

        //save the file
        // Path assoluto della cartella dove sta l'eseguibile
        var exeDir = AppDomain.CurrentDomain.BaseDirectory;

        // "outputs" dentro alla cartella dell'eseguibile
        var dir = Path.Combine(exeDir, "outputs");
        Directory.CreateDirectory(dir); // crea se non esiste

        // percorso completo del file
        var filePath = Path.Combine(dir, site + "_" + variety + ".csv");
        File.WriteAllLines(filePath, toWrite);


        #endregion
    }

    #endregion
}


