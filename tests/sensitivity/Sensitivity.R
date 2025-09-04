# Remove objects from the Global Environment----
rm(list=ls())

#remove.packages('FraNchEstYN')


#libraries
library(sensitivity) #for doing sensitivity
library(tidyverse)
#devtools::install_github("GeoModelLab/FraNchEstYN", force = TRUE, build_vignettes = FALSE)
library(FraNchEstYN)

#set this directory as the working directory
thisDir<-getwd()

##----------------------------------------------------------------
##generate sensitivity experiment
##ONLY ONCE BEFORE SENSITIVITY SIMULATIONS
##----------------------------------------------------------------
options(scipen=999)

#1. calibrate crop parameters

## mgt data
site_sowing_dates <- list(
  FIJO0QXX = 127,
  ININ0QXX = 299,
  ITPO0QXX = 323,
  TRIZ0QXX = 320,
  UKRO0QXX = 289,
  USMA0QXX = 360
)

# Convert the named list to a data frame
sowing_df <- data.frame(
  site = names(site_sowing_dates),
  sowingDOY = unlist(site_sowing_dates),
  stringsAsFactors = FALSE
)

# Add other columns
management_data <- data.frame(
  site        = sowing_df$site,
  crop        = "wheat",
  variety     = "Generic",
  sowingDOY   = sowing_df$sowingDOY,
  year        = "All",
  stringsAsFactors = FALSE
)

## weather data
weather_list <- list.files(paste0(getwd(),'//weatherData'),full.names = T)

# Function to read a CSV and add site column
read_weather_with_site <- function(file_path) {
  # Extract just the file name without path or extension
  site_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the CSV and add the site column
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  df$site <- site_name
  
  return(df)
}

# Read and combine all files into one data frame
weather_data <- do.call(rbind, lapply(weather_list, read_weather_with_site)) |> 
  rename(rad=SRAD, Year=YYYY, Month = MM, Day=DD)


## reference data
reference_list <- list.files(paste0(getwd(),'//referenceData'),full.names = T)
reference_data <- do.call(rbind, lapply(reference_list, read_weather_with_site)) |> 
  rename(YieldAttainable = WGRN, AGB = WTOP, Year = Pyear) |> 
  mutate(YieldAttainable = YieldAttainable*10,
         AGB = AGB*10) |> 
  select(site,Year,DOY,FINT,YieldAttainable,AGB)

#1. crop calibration
sites<-unique(weather_data$site)

start_end<-c(1980,2011)

s<-sites[2]

# Initialize empty lists to store results
all_parameters <- list()
all_outputs <- list()
all_metrics <- list()

library(FraNchEstYN)
#adjust param bounds
thisCropParam <- cropParameters$wheat
thisCropParam$RadiationUseEfficiency$calibration<-T
thisCropParam$CycleLength$max<-3500
thisCropParam$CycleLength$min<-1500
thisCropParam$FloweringStart$min<-40
thisCropParam$FloweringStart$max<-65
thisCropParam$RadiationUseEfficiency$min<-2
thisCropParam$RadiationUseEfficiency$max<-3.8
thisCropParam$HalfIntGrowth$min<-12
thisCropParam$HalfIntGrowth$max<-25
thisCropParam$PartitioningMaximum$min<-.8
thisCropParam$PartitioningMaximum$calibration<-T
thisCropParam$ToptCrop$calibration<-T
thisCropParam$ToptCrop$max<-25

s<-sites[2]
for(s in sites)
{
  thisWeather <- weather_data |> 
    filter(site == s)
  
  thisReference <- reference_data |> 
    mutate(Disease=NA) |> 
    filter(site == s)
  
  thisManagement <- management_data |> 
    filter(site == s)
  
  df<-franchestyn(weather_data = thisWeather,
                  management_data = thisManagement,
                  reference_data = thisReference,
                  cropParameters = thisCropParam,
                  diseaseParameters = diseaseParameters$black_rust,
                  start_end = start_end,
                  calibration = 'crop',
                  iterations=999)
  
  # Save crop parameters as RDS
  saveRDS(df$parameters$crop, file = paste0("cropparameters/", s, "_parameters.rds"))
  # Save parameters, outputs, and metrics
  all_outputs[[s]]    <- df$outputs$simulation
  all_metrics[[s]]    <- df$diagnostics$metrics
}

#combine outputs
combined_outputs <- bind_rows(all_outputs, .id = "site")
combined_metrics <- bind_rows(all_metrics, .id = "site")

saveRDS(combined_outputs,'cropCalibration_outputs.rds')
saveRDS(combined_metrics,'cropCalibration_metrics.rds')

combined_outputs<-readRDS('cropCalibration_outputs.rds')

unique(combined_outputs$site)

#plots
ggplot(combined_outputs, aes(x=DaysAfterSowing)) + 
  stat_summary(geom='line',aes(y=YieldAttainable/20000),col='red')+
  stat_summary(aes(y=YieldRef/20000),col='red')+
  stat_summary(geom='line',aes(y=LightInterception),col='green2')+
  stat_summary(aes(y=LightInterceptionRef),col='green2')+
  stat_summary(geom='line',aes(y=AGBattainable/20000),col='blue')+
  stat_summary(aes(y=AGBRef/20000),col='blue')+
  facet_wrap(~site)



paramITPO<-readRDS(paste0("cropparameters/ITPO0QXX_parameters.rds"))
dfITPO<-parameters_to_df(paramITPO)
paramUSMA<-readRDS(paste0("cropparameters/USMA0QXX_parameters.rds"))
dfUSMA<-parameters_to_df(paramUSMA)
paramTRIZ<-readRDS(paste0("cropparameters/TRIZ0QXX_parameters.rds"))
dfTRIZ<-parameters_to_df(paramTRIZ)
paramININ<-readRDS(paste0("cropparameters/ININ0QXX_parameters.rds"))
dfININ<-parameters_to_df(paramININ)
paramFIJO<-readRDS(paste0("cropparameters/FIJO0QXX_parameters.rds"))
dfFIJO<-parameters_to_df(paramFIJO)
paramUKRO<-readRDS(paste0("cropparameters/UKRO0QXX_parameters.rds"))
dfUKRO<-parameters_to_df(paramUKRO)



# read crop parameters
paramSensitivity <- read.csv('franchestynParameters.csv') |> 
  dplyr::filter(Model=='disease') |> 
  group_by(Parameter) |> 
  slice_head()

#create two sets of parameters for two different pathogens
#set 1 - rust-type pathogen
set1<-paramSensitivity
set1$set<-'set_1'
set1$value[set1$Parameter == "IsSplashBorne"] <- 0

set1$value[set1$Parameter == "Tmin"] <- 2
set1$min[set1$Parameter == "Tmin"] <- 0
set1$max[set1$Parameter == "Tmin"] <- 4

set1$value[set1$Parameter == "Topt"] <- 20
set1$min[set1$Parameter == "Topt"] <- 18
set1$max[set1$Parameter == "Topt"] <- 22

set1$value[set1$Parameter == "Tmax"] <- 30
set1$min[set1$Parameter == "Tmax"] <- 28
set1$max[set1$Parameter == "Tmax"] <- 32

set1$value[set1$Parameter == "WetnessDurationMinimum"] <- 2
set1$min[set1$Parameter == "WetnessDurationMinimum"] <- 0
set1$max[set1$Parameter == "WetnessDurationMinimum"] <- 4

set1$value[set1$Parameter == "WetnessDurationOptimum"] <- 6
set1$min[set1$Parameter == "WetnessDurationOptimum"] <- 4
set1$max[set1$Parameter == "WetnessDurationOptimum"] <- 8

set1$value[set1$Parameter == "DryCriticalInterruption"] <- 2
set1$min[set1$Parameter == "DryCriticalInterruption"] <- 0
set1$max[set1$Parameter == "DryCriticalInterruption"] <- 4

set1$value[set1$Parameter == "LatencyDuration"] <- 5
set1$min[set1$Parameter == "LatencyDuration"] <- 3
set1$max[set1$Parameter == "LatencyDuration"] <- 7

set1$value[set1$Parameter == "SporulationDuration"] <- 10
set1$min[set1$Parameter == "SporulationDuration"] <- 8
set1$max[set1$Parameter == "SporulationDuration"] <- 12

set1$value[set1$Parameter == "RelativeHumidityCritical"] <- 70
set1$min[set1$Parameter == "RelativeHumidityCritical"] <- 65
set1$max[set1$Parameter == "RelativeHumidityCritical"] <- 75

set1$value[set1$Parameter == "RelativeHumidityNotLimiting"] <- 90
set1$min[set1$Parameter == "RelativeHumidityNotLimiting"] <- 85
set1$max[set1$Parameter == "RelativeHumidityNotLimiting"] <- 95

set1$value[set1$Parameter == "SenescenceAcceleratorDamage"] <- .1
set1$min[set1$Parameter == "SenescenceAcceleratorDamage"] <- .05
set1$max[set1$Parameter == "SenescenceAcceleratorDamage"] <- .15

set1$value[set1$Parameter == "AssimilateSappersDamage"] <- 30
set1$min[set1$Parameter == "AssimilateSappersDamage"] <- 20
set1$max[set1$Parameter == "AssimilateSappersDamage"] <- 40

set1$value[set1$Parameter == "RUEreducerDamage"] <- 1
set1$min[set1$Parameter == "RUEreducerDamage"] <- .5
set1$max[set1$Parameter == "RUEreducerDamage"] <- 1.5

set1$value[set1$Parameter == "LightStealerDamage"] <- 1
set1$min[set1$Parameter == "LightStealerDamage"] <- .5
set1$max[set1$Parameter == "LightStealerDamage"] <- 1.5

set1$value[set1$Parameter == "Rain50Detachment"] <- 5
set1$min[set1$Parameter == "Rain50Detachment"] <- 3
set1$max[set1$Parameter == "Rain50Detachment"] <- 7

set1$value[set1$Parameter == "OuterInoculumMax"] <- 0.03
set1$min[set1$Parameter == "OuterInoculumMax"] <- 0.02
set1$max[set1$Parameter == "OuterInoculumMax"] <- 0.04

set1$value[set1$Parameter == "OuterInoculumShapeRelease"] <- 0

set1$value[set1$Parameter == "PathogenSpread"] <- 0.1
set1$min[set1$Parameter == "PathogenSpread"] <- 0.05
set1$max[set1$Parameter == "PathogenSpread"] <- 0.15

set1$value[set1$Parameter == "HydroThermalTimeOnset"] <- 10
set1$min[set1$Parameter == "HydroThermalTimeOnset"] <- 8
set1$max[set1$Parameter == "HydroThermalTimeOnset"] <- 12

set1$value[set1$Parameter == "CyclePercentageOnset"] <- 30
set1$min[set1$Parameter == "CyclePercentageOnset"] <- 25
set1$max[set1$Parameter == "CyclePercentageOnset"] <- 35


#set 2 - blast-type pathogen
set2<-paramSensitivity
set2$set<-'set_2'
set2$value[set1$Parameter == "IsSplashBorne"] <- 1

set2$value[set1$Parameter == "Tmin"] <- 11
set2$min[set1$Parameter == "Tmin"] <- 9
set2$max[set1$Parameter == "Tmin"] <- 13

set2$value[set1$Parameter == "Topt"] <- 26
set2$min[set1$Parameter == "Topt"] <- 24
set2$max[set1$Parameter == "Topt"] <- 28

set2$value[set1$Parameter == "Tmax"] <- 36
set2$min[set1$Parameter == "Tmax"] <- 34
set2$max[set1$Parameter == "Tmax"] <- 38

set2$value[set1$Parameter == "WetnessDurationMinimum"] <- 5
set2$min[set1$Parameter == "WetnessDurationMinimum"] <- 3
set2$max[set1$Parameter == "WetnessDurationMinimum"] <- 7

set2$value[set1$Parameter == "WetnessDurationOptimum"] <- 12
set2$min[set1$Parameter == "WetnessDurationOptimum"] <- 10
set2$max[set1$Parameter == "WetnessDurationOptimum"] <- 14

set2$value[set1$Parameter == "DryCriticalInterruption"] <- 20
set2$min[set1$Parameter == "DryCriticalInterruption"] <- 18
set2$max[set1$Parameter == "DryCriticalInterruption"] <- 22

set2$value[set1$Parameter == "LatencyDuration"] <- 8
set2$min[set1$Parameter == "LatencyDuration"] <- 6
set2$max[set1$Parameter == "LatencyDuration"] <- 10

set2$value[set1$Parameter == "SporulationDuration"] <- 16
set2$min[set1$Parameter == "SporulationDuration"] <- 14
set2$max[set1$Parameter == "SporulationDuration"] <- 18

set2$value[set1$Parameter == "RelativeHumidityCritical"] <- 60
set2$min[set1$Parameter == "RelativeHumidityCritical"] <- 55
set2$max[set1$Parameter == "RelativeHumidityCritical"] <- 65

set2$value[set1$Parameter == "RelativeHumidityNotLimiting"] <- 80
set2$min[set1$Parameter == "RelativeHumidityNotLimiting"] <- 75
set2$max[set1$Parameter == "RelativeHumidityNotLimiting"] <- 85

set2$value[set1$Parameter == "SenescenceAcceleratorDamage"] <- .1
set2$min[set1$Parameter == "SenescenceAcceleratorDamage"] <- .05
set2$max[set1$Parameter == "SenescenceAcceleratorDamage"] <- .15

set2$value[set1$Parameter == "AssimilateSappersDamage"] <- 30
set2$min[set1$Parameter == "AssimilateSappersDamage"] <- 20
set2$max[set1$Parameter == "AssimilateSappersDamage"] <- 40

set2$value[set1$Parameter == "RUEreducerDamage"] <- 1
set2$min[set1$Parameter == "RUEreducerDamage"] <- .5
set2$max[set1$Parameter == "RUEreducerDamage"] <- 1.5

set2$value[set1$Parameter == "LightStealerDamage"] <- 1
set2$min[set1$Parameter == "LightStealerDamage"] <- .5
set2$max[set1$Parameter == "LightStealerDamage"] <- 1.5

set2$value[set1$Parameter == "Rain50Detachment"] <- 5
set2$min[set1$Parameter == "Rain50Detachment"] <- 3
set2$max[set1$Parameter == "Rain50Detachment"] <- 7

set2$value[set1$Parameter == "OuterInoculumMax"] <- 0.1
set2$min[set1$Parameter == "OuterInoculumMax"] <- 0.09
set2$max[set1$Parameter == "OuterInoculumMax"] <- 0.11

set2$value[set1$Parameter == "OuterInoculumShapeRelease"] <- 0

set2$value[set1$Parameter == "PathogenSpread"] <- 0.1
set2$min[set1$Parameter == "PathogenSpread"] <- 0.05
set2$max[set1$Parameter == "PathogenSpread"] <- 0.15

set2$value[set1$Parameter == "HydroThermalTimeOnset"] <- 10
set2$min[set1$Parameter == "HydroThermalTimeOnset"] <- 8
set2$max[set1$Parameter == "HydroThermalTimeOnset"] <- 12

set2$value[set1$Parameter == "CyclePercentageOnset"] <- 30
set2$min[set1$Parameter == "CyclePercentageOnset"] <- 25
set2$max[set1$Parameter == "CyclePercentageOnset"] <- 35

param_sets<-rbind(set1,set2) |> 
  dplyr::filter(!Parameter%in%c('IsSplashBorne','OuterInoculumShapeRelease'))


#set boundaries at +- 15%
# sensitivity analysis
m_levels=6
m_grid.jump = 3

parSets<-unique(param_sets$set)

parSet<-parSets[1]
for(parSet in parSets)
{
  thisParSet<-param_sets |> 
    dplyr::filter(set==parSet)
  
  ###morris design
  morris_df = sensitivity::morris(model = NULL, factors = thisParSet$Parameter, r = 30,
                  design = list(type = "oat", levels = m_levels, grid.jump = m_grid.jump),
                  binf = thisParSet$min, bsup = thisParSet$max, scale = T)
  
  #save RDS file primary
  saveRDS(morris_df, paste0(thisDir, paste0("\\parametersDesign", parSet, ".rds")))

  paramsSA<-as.data.frame(morris_df$X) |> 
     dplyr::mutate(Model = "franchestyn",
          ID_run = 1:n()) |> 
     pivot_longer(-c(Model,ID_run),names_to="parameter",values_to="value")
  write.csv(paramsSA, paste0("morris_", parSet, ".csv"),row.names=F)
  
  outDfs<-list(list())
  outYieldLoss_mean<-list(list())
  outYieldLoss_sd<-list(list())
  outDisSev_mean<-list(list())
  outDisSev_sd<-list(list())
  outMorrisYieldLoss_mean<-list()
  outMorrisYieldLoss_sd<-list()
  outMorrisDisSev_mean<-list()
  outMorrisDisSev_sd<-list()
  outDfsUncertainty<-list()

  s<-sites[1]
  #loop over sites
  for(s in sites)
  {
    thisWeather <- weather_data |> 
      filter(site == s)
    
    thisReference <- reference_data |> 
      filter(site == s) |> 
      mutate(Disease=NA) 
    
    thisManagement <- management_data |> 
      filter(site == s)
    
    #loop over parsets
    par<-1
    for(par in 1:length(unique(paramsSA$ID_run)))
    {
        thisPar<- paramsSA |> filter(ID_run == par) %>%
          select(-Model,ID_run)
        
        #assign crop parameters  
        paramCrop<-readRDS(paste0(getwd(),"//cropParameters//",s,'_parameters.rds'))
        
        paramDisease<-FraNchEstYN::df_to_parameters(thisPar)
        if(parSet=='set_1'){
          paramDisease$IsSplashBorne$value<-0
          paramDisease$OuterInoculumShapeRelease$value<-0
          
        } else {
          paramDisease$IsSplashBorne$value<-1
          paramDisease$OuterInoculumShapeRelease$value<-0
        }
        
        # Step 3: Perform a run with optimized parameters
        df<-FraNchEstYN::franchestyn(weather_data = thisWeather,
                        management_data = thisManagement,
                        reference_data = thisReference,
                        cropParameters = paramCrop,
                        diseaseParameters = paramDisease,
                        start_end = start_end,
                        calibration = 'none')
        
        outputs<-df$outputs$simulation
        summary<-df$outputs$summary |> 
          summarise(DiseaseSeverity_mean = mean(DiseaseSeverity,na.rm=T),
                    DiseaseSeverity_sd = sd(DiseaseSeverity,na.rm=T),
                    YieldLoss_mean = mean(YieldLossPerc,na.rm=T),
                    YieldLoss_sd = sd(YieldLossPerc,na.rm=T))
      
        # Extract yield gap 
        outYieldLoss_mean[[par]] <- summary$YieldLoss_mean
        outYieldLoss_sd[[par]] <- summary$YieldLoss_mean
        outDisSev_mean[[par]] <- summary$DiseaseSeverity_mean
        outDisSev_sd[[par]] <- summary$DiseaseSeverity_sd
        
        dailyOut<-outputs |> 
          select(Site,DaysAfterSowing,
                 LightInterception,LightInterceptionRef,LightIntHealthy,AGBattainable,YieldAttainable,YieldRef,
                 HTtimeSinoculum,HTtimeRinfection,Susceptible,Latent,Sporulating,Dead,Affected,DiseaseSeverity,
                 AGBactual,YieldActual) |> 
          group_by(DaysAfterSowing) |> 
          summarise(across(
            where(is.numeric),
            \(x) mean(x, na.rm = TRUE)
          ), .groups = "drop")
        
        outDfs[[par]] <- dailyOut
    }
    
    # Process results for this experiment 
    
    outVarYieldLoss_mean <- unlist(outYieldLoss_mean, use.names = FALSE)
    outVarYieldLoss_sd <-  unlist(outYieldLoss_sd, use.names = FALSE)
    outVarDisSev_mean <- unlist(outDisSev_mean, use.names = FALSE)
    outVarDisSev_sd <- unlist(outDisSev_sd, use.names = FALSE)
    
    # Compute sensitivity indices for fruits
    resultsYieldLoss_mean<- tell(morris_df, as.numeric(outVarYieldLoss_mean))
    outMorrisYieldLoss_mean[[s]] <- resultsYieldLoss_mean
    resultsYieldLoss_sd<- tell(morris_df, as.numeric(outVarYieldLoss_sd))
    outMorrisYieldLoss_sd[[s]] <- resultsYieldLoss_sd
    resultsDisSev_mean<- tell(morris_df, as.numeric(outVarDisSev_mean))
    outMorrisDisSev_mean[[s]] <- resultsDisSev_mean
    resultsDisSev_sd<- tell(morris_df, as.numeric(outVarYieldLoss_mean))
    outMorrisDisSev_sd[[s]] <- resultsDisSev_sd
        
    # Save uncertainty outputs for this soil and experiment
    outDfsUncertainty[[s]] <- outDfs
  }
  
  ####save RDS objects outMorris outDfsUncertainty
  saveRDS(outMorrisYieldLoss_mean, paste0("out//",parSet, "_outMorrisYieldLoss_mean.rds"))
  saveRDS(outMorrisYieldLoss_sd, paste0("out//",parSet,'outMorrisYieldLoss_sd.rds'))
  saveRDS(outMorrisDisSev_mean, paste0("out//",parSet,'outMorrisDisSev_mean.rds'))
  saveRDS(outMorrisDisSev_sd, paste0("out//",parSet,'outMorrisDisSev_sd.rds'))

  saveRDS(outDfsUncertainty, paste0("out//",parSet,'outDfsUncertainty.rds'))

  
  exp<-1
  results_yieldLoss_mean_list<-list()
  results_yieldLoss_sd_list<-list()
  results_disSev_mean_list<-list()
  results_disSev_sd_list<-list()
  
  for(exp in 1:length(outMorrisYieldLoss_mean))
  {
    #dfs
    dfmu_yieldLoss_mean<-as.data.frame(apply(outMorrisYieldLoss_mean[exp][[1]][[11]], 2, mean))
    dfmu_yieldLoss_sd<-as.data.frame(apply(outMorrisYieldLoss_sd[exp][[1]][[11]], 2, mean))
    dfmu_disSev_mean<-as.data.frame(apply(outMorrisDisSev_mean[exp][[1]][[11]], 2, mean))
    dfmu_disSev_sd<-as.data.frame(apply(outMorrisDisSev_sd[exp][[1]][[11]], 2, mean))
 
    # Rename the single column that was generated to "mean_values"
    colnames(dfmu_yieldLoss_mean) <- "mu"
    colnames(dfmu_yieldLoss_sd) <- "mu"
    colnames(dfmu_disSev_mean) <- "mu"
    colnames(dfmu_disSev_sd) <- "mu"
    
    #complete the dfs
    #yield loss mean
    dfmu_yieldLoss_mean$parameter <- rownames(dfmu_yieldLoss_mean)
    dfmu_yieldLoss_mean$experiment <- exp
    dfmu_yieldLoss_mean$muStar<-as.data.frame(apply(outMorrisYieldLoss_mean[exp][[1]][[11]], 2, 
                                             function(x) mean(abs(x))))[,1]
    dfmu_yieldLoss_mean$sigma<-as.data.frame(apply(outMorrisYieldLoss_mean[exp][[1]][[11]], 2, sd))[,1]
    
    #yield loss sd
    dfmu_yieldLoss_sd$parameter <- rownames(dfmu_yieldLoss_sd)
    dfmu_yieldLoss_sd$experiment <- exp
    dfmu_yieldLoss_sd$muStar<-as.data.frame(apply(outMorrisYieldLoss_sd[exp][[1]][[11]], 2, 
                                                    function(x) mean(abs(x))))[,1]
    dfmu_yieldLoss_sd$sigma<-as.data.frame(apply(outMorrisYieldLoss_sd[exp][[1]][[11]], 2, sd))[,1]
    
    #dissev mean
    dfmu_disSev_mean$parameter <- rownames(dfmu_disSev_mean)
    dfmu_disSev_mean$experiment <- exp
    dfmu_disSev_mean$muStar<-as.data.frame(apply(outMorrisDisSev_mean[exp][[1]][[11]], 2, 
                                                    function(x) mean(abs(x))))[,1]
    dfmu_disSev_mean$sigma<-as.data.frame(apply(outMorrisDisSev_mean[exp][[1]][[11]], 2, sd))[,1]
    
    #dissev sd
    dfmu_disSev_sd$parameter <- rownames(dfmu_disSev_sd)
    dfmu_disSev_sd$experiment <- exp
    dfmu_disSev_sd$muStar<-as.data.frame(apply(outMorrisDisSev_sd[exp][[1]][[11]], 2, 
                                                    function(x) mean(abs(x))))[,1]
    dfmu_disSev_sd$sigma<-as.data.frame(apply(outMorrisDisSev_sd[exp][[1]][[11]], 2, sd))[,1]
    
    # Reset row names to ensure they are not included in the final result
    rownames(dfmu_yieldLoss_mean) <- NULL
    rownames(dfmu_yieldLoss_sd) <- NULL
    rownames(dfmu_disSev_mean) <- NULL
    rownames(dfmu_disSev_sd) <- NULL
    
    
    # Rearrange the columns into the desired order
    new_order <- c("experiment", "parameter", "mu", "muStar", "sigma")
    dfmu_yieldLoss_mean <- dfmu_yieldLoss_mean[, new_order]
    dfmu_yieldLoss_sd <- dfmu_yieldLoss_sd[, new_order]
    dfmu_disSev_mean <- dfmu_disSev_mean[, new_order]
    dfmu_disSev_sd <- dfmu_disSev_sd[, new_order]
    
    # Store results in the predefined list
    results_yieldLoss_mean_list[[exp]] <- list(dfmu_yieldLoss_mean)
    results_yieldLoss_sd_list[[exp]] <- list(dfmu_yieldLoss_sd)
    results_disSev_mean_list[[exp]] <- list(dfmu_disSev_mean)
    results_disSev_sd_list[[exp]] <- list(dfmu_disSev_sd)
    
  }

  # Combine all data frames into one
  all_yieldLoss_mean_Experiments <- do.call(rbind, lapply(results_yieldLoss_mean_list, function(x) {
    do.call(rbind, x)  # Combine sand, loam, clay within each experiment
  }))
  all_yieldLoss_sd_Experiments <- do.call(rbind, lapply(results_yieldLoss_sd_list, function(x) {
    do.call(rbind, x)  # Combine sand, loam, clay within each experiment
  }))
  all_disSev_mean_Experiments <- do.call(rbind, lapply(results_disSev_mean_list, function(x) {
    do.call(rbind, x)  # Combine sand, loam, clay within each experiment
  }))
  all_disSev_sd_Experiments <- do.call(rbind, lapply(results_disSev_sd_list, function(x) {
    do.call(rbind, x)  # Combine sand, loam, clay within each experiment
  }))
  
  #acronyms for parameters
  new_acronyms <- c(
    "OuterInoculumMax"              = "OutI",
    "PathogenSpread"                = "PatSp",
    "WetnessDurationOptimum"       = "WDopt",
    "WetnessDurationMinimum"       = "WDmin",
    "DryCriticalInterruption"      = "D50",
    "Tmin"                          = "Tmin",
    "Topt"                          = "Topt",
    "Tmax"                          = "Tmax",
    "RelativeHumidityCritical"     = "RHcr",
    "Rain50Detachment"             = "R50d",
    "RelativeHumidityNotLimiting"  = "RHnl",
    "HydroThermalTimeOnset"        = "HTons",
    "CyclePercentageOnset"         = "CYons",
    "LatencyDuration"              = "LatD",
    "SporulationDuration"          = "SpoD",
    "LightStealerDamage"           = "LSdmg",
    "RUEreducerDamage"             = "RUEdmg",
    "SenescenceAcceleratorDamage" = "SENdmg",
    "AssimilateSappersDamage"      = "ASSdmg",
    "OuterInoculumShapeParameter"  = "Ishape"
  )
  
  # Substituting the parameter column in the data frame
  all_yieldLoss_mean_Experiments$parameter <- new_acronyms[all_yieldLoss_mean_Experiments$parameter]
  all_yieldLoss_sd_Experiments$parameter <- new_acronyms[all_yieldLoss_sd_Experiments$parameter]
  all_disSev_mean_Experiments$parameter <- new_acronyms[all_disSev_mean_Experiments$parameter]
  all_disSev_sd_Experiments$parameter <- new_acronyms[all_disSev_sd_Experiments$parameter]
  
  saveRDS(all_yieldLoss_mean_Experiments, paste0("out//",parSet, 'all_yieldLoss_mean_Experiments.rds'))
  saveRDS(all_yieldLoss_sd_Experiments, paste0("out//",parSet, 'all_yieldLoss_sd_Experiments.rds'))
  saveRDS(all_disSev_mean_Experiments, paste0("out//",parSet, 'all_disSev_mean_Experiments.rds'))
  saveRDS(all_disSev_sd_Experiments, paste0("out//",parSet, 'all_disSev_sd_Experiments.rds'))
  
}

disSev_set1<-readRDS('out//set_1all_disSev_mean_Experiments.rds') |> 
  mutate(set='set_1')
disSev_set2<-readRDS('out//set_2all_disSev_mean_Experiments.rds')|> 
  mutate(set='set_2')

disSev_morris<-rbind(disSev_set1,disSev_set2) %>%
  mutate(site_name = sites[experiment],
         site =substr(site_name,1,4))

ggplot(disSev_morris |> 
         filter(!parameter%in%c('Ishape')) |> 
         mutate(facet_strip=paste0(site,"_",set)),aes(x=reorder(parameter,-muStar)))+
  geom_col(aes(y=muStar,fill=set),position='dodge')+
  #coord_flip()+
  lemon::facet_rep_wrap(~facet_strip,scales='free_y',ncol=4,repeat.tick.labels = T)+
  theme_classic()+
  theme(legend.position='none',
        axis.text.x = element_text(angle=90,vjust=0,hjust=1,size=10))+
  xlab('')

yieldLoss_set1<-readRDS('out//set_1all_yieldLoss_mean_Experiments.rds') |> 
  mutate(set='set_1')
yieldLoss_set2<-readRDS('out//set_2all_yieldLoss_mean_Experiments.rds')|> 
  mutate(set='set_2')

yieldLoss_morris<-rbind(yieldLoss_set1,yieldLoss_set2) %>%
  mutate(site_name = sites[experiment],
         site =substr(site_name,1,4))

ggplot(yieldLoss_morris |> 
         filter(!parameter%in%c('Ishape')) |> 
         mutate(facet_strip=paste0(site,"_",set)),aes(x=reorder(parameter,-muStar)))+
  geom_col(aes(y=muStar,fill=set),position='dodge')+
  #coord_flip()+
  lemon::facet_rep_wrap(~facet_strip,scales='free_y',ncol=4,repeat.tick.labels = T)+
  theme_classic()+
  theme(legend.position='none',
        axis.text.x = element_text(angle=90,vjust=0,hjust=1,size=10))+
  xlab('')


#TODO: ANALYSIS

#compute mean and sd of the normalized metrics
diSev_mean_synth <- disSev_morris |> 
    ungroup() |> 
    group_by(parameter) |> 
    dplyr::summarise(mu=mean(muStar,na.rm=T),
              muSD=sd(muStar,na.rm=T),
              sig=mean(sigma,na.rm=T),
              sigSD=sd(sigma,na.rm=T))

allIndices<-rbind(disSev_morris |> mutate(variable='DisSev'),
                  yieldLoss_morris |> mutate(variable='YieldLoss'))

library(ggrepel)

# Step 1: Find top 7 parameters per site × variable
top7_labels <- allIndices %>%
  mutate(strip_label=paste0(site,"_",set,"_",variable)) |> 
  filter(!parameter %in% c("Ishape")) %>%
  group_by(strip_label) %>%
  slice_max(muStar, n = 7, with_ties = FALSE) %>%
  ungroup()

# Step 4: Plot
ggplot(allIndices %>% 
         mutate(strip_label=paste0(site,"_",set,"_",variable)) |> 
         filter(!parameter %in% c('Ishape'))) + 
  geom_point(aes(x = muStar, y = sigma, color = set), size = 0.5) +
  geom_text_repel(data = top7_labels,
                  aes(x = muStar, y = sigma, label = parameter,color=set),
                  size = 3,
            max.overlaps = 100,
            box.padding = 0.1,
            point.padding = 0.1,
            segment.size = 0.1) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Mean of muStar",
    y = "Mean of sigma",
    title = "FraNchEstYN - Morris sensitivity analysis"
  ) +
  facet_wrap(~ strip_label, scales = 'free',ncol=6)



# Read the nested lists
outDfs_set1 <- readRDS('out//set_1outDfsUncertainty.rds')
outDfs_set2 <- readRDS('out//set_2outDfsUncertainty.rds')

# Function to add 'set' and flatten inner list of dfs with identifiers
flatten_and_tag <- function(nested_list, set_name) {
  # nested_list is a list of lists of data.frames
  
  # Use map2 to get names of outer list as experiment ids
  map2_dfr(nested_list, names(nested_list), function(inner_list, experiment_name) {
    # inner_list: list of dfs per experiment
    # experiment_name: outer list name
    
    # For each run/dataframe, add run and experiment info + set
    map2_dfr(inner_list, seq_along(inner_list), function(df, run_number) {
      df %>%
        mutate(set = set_name,
               experiment = experiment_name,
               run = run_number)
    })
  })
}

# Apply to both sets
df_set1 <- flatten_and_tag(outDfs_set1, "set_1")
df_set2 <- flatten_and_tag(outDfs_set2, "set_2")

# Combine into one dataframe
outDfs <- bind_rows(df_set1, df_set2)

#takes 16 secs
outDfs_synth <- outDfs %>%
  group_by(experiment, set, DaysAfterSowing) %>%
  summarise(across(where(is.numeric),
                   list(
                     q25 = ~quantile(., 0.25, na.rm = TRUE),
                     q40 = ~quantile(., 0.40, na.rm = TRUE),
                     median = ~quantile(., 0.50, na.rm = TRUE),
                     q60 = ~quantile(., 0.60, na.rm = TRUE),
                     q75 = ~quantile(., 0.75, na.rm = TRUE)
                   ),
                   .names = "{col}_{fn}"),
            .groups = "drop") 
  


outDfs_synth_plot<-outDfs_synth |> 
  mutate(site=substr(experiment,1,4)) |> 
  filter((site=='FIJO'&DaysAfterSowing<140)|
           (site=='ININ'&DaysAfterSowing<150)|
           (site=='ITPO'&DaysAfterSowing<210&DaysAfterSowing>70)|
           (site=='TRIZ'&DaysAfterSowing<200&DaysAfterSowing>70)|
           (site=='UKRO'&DaysAfterSowing<310&DaysAfterSowing>180)|
           (site=='USMA'&DaysAfterSowing<150&DaysAfterSowing>55))
           

# Plot all variables with ribbons for the IQR and lines for the median
ggplot(outDfs_synth_plot |> 
         filter(DaysAfterSowing>50) |> 
         mutate(stripLabel=paste0(site,"_",set)), aes(x = DaysAfterSowing)) +
  #geom_line(aes(y = LightInterception_median)) +
  geom_area(aes(y = HTtimeRinfection_median),fill='green2',linewidth=.5,alpha=.3) +
  geom_ribbon(aes(ymin = DiseaseSeverity_q40, ymax = DiseaseSeverity_q60), alpha = 0.4,fill='darkslateblue') +
  geom_line(aes(y = DiseaseSeverity_median),col='darkslateblue',linewidth=.9) +
  geom_ribbon(aes(ymin = Latent_q40, ymax = Latent_q60), alpha = 0.7,fill='orange') +
  geom_line(aes(y = Latent_median),col='orange',linewidth=.5) +
  geom_ribbon(aes(ymin = Sporulating_q40, ymax = Sporulating_q60), alpha = 0.7,fill='orange4') +
  geom_line(aes(y = Sporulating_median),col='orange4',linewidth=.5) +
 # geom_ribbon(aes(ymin = Susceptible_q40, ymax = Susceptible_q60), alpha = 0.4,fill='green2') +
  #geom_line(aes(y = Susceptible_median),col='green2',linewidth=.5) +
  #geom_line(aes(y=Dead_median),fill='red4',alpha=.3)+

  facet_wrap(~stripLabel,ncol=4,scales='free') +
  theme_classic() +
  theme(legend.position = "none")+
  ylab('Disease severity, latent and infectious tissue')+
  xlab('Days after sowing')
  


site_sowing_dates <- c(
  FIJO = 127,
  ININ = 299,
  ITPO = 323,
  TRIZ = 320,
  UKRO = 289,
  USMA = 360
)


ref_data_avg <- reference_data |> 
  mutate(
    sowing_doy = site_sowing_dates[site],
    DaysAfterSowing = if_else(
      site != "FIJO",
      if_else(DOY < sowing_doy, DOY + (365-sowing_doy), DOY - sowing_doy),
      DOY - sowing_doy  # For FIJO, just use DOY - sowingDOY
    )
  ) |> 
  ungroup() |> 
  group_by(site, DaysAfterSowing) |> 
  summarise(across(where(is.numeric),
                   list(median = ~quantile(., 0.50, na.rm = TRUE)),
                   .names = "{col}_{fn}"),
            .groups = "drop") |> 
  as.data.frame()



outDfs_yield_plot<-outDfs_synth |> 
  mutate(site=substr(experiment,1,4)) |> 
  left_join(ref_data_avg,by=c('DaysAfterSowing',"site")) |> 
  filter((site=='FIJO'&DaysAfterSowing<120)|
           (site=='ININ'&DaysAfterSowing<150)|
           (site=='ITPO'&DaysAfterSowing<220)|
           (site=='TRIZ'&DaysAfterSowing<210)|
           (site=='UKRO'&DaysAfterSowing<310)|
           (site=='USMA'&DaysAfterSowing<150)) 

  

ggplot(outDfs_yield_plot |> mutate(stripLabel=paste0(site,"_",set)) , aes(x = DaysAfterSowing)) +
  #geom_area(aes(y = HTtimeRinfection_median), alpha = .5,fill='blue') +
  geom_line(aes(y = LightInterception_median), col = 'black', size = .5,linetype=3) +

   geom_line(aes(y = Susceptible_median),col='green4',linewidth=.5) +


  #geom_point(aes(y = YieldAttainable_median.y/10000), fill = 'black', size = .3,alpha = .2) +
  geom_area(aes(y = LightIntHealthy_median), fill = 'green', size = .3,alpha = .5) +
  geom_ribbon(aes(ymin = Susceptible_q40, ymax = Susceptible_q60), alpha = .5,fill='green4') +
  #geom_line(aes(y = DiseaseSeverity_median), alpha = .2, fill = 'slateblue') +
  geom_line(aes(y = YieldAttainable_median.x / 10000),linetype=2) +
  geom_ribbon(aes(ymin = YieldActual_q40 / 10000, ymax = YieldActual_q60 / 10000), fill = 'red', alpha = 0.8) +
 # geom_ribbon(aes(ymin = YieldActual_q25 / 10000, ymax = YieldActual_q75 / 10000), fill = 'darkslateblue', alpha = 0.4) +
  geom_line(aes(y = YieldActual_median / 10000), col = 'red3', linewidth = 1) +
  facet_wrap(~ stripLabel, ncol = 4, scales = 'free_x') +
  #geom_ribbon(aes(ymin = HTtimeRinfection_q40, ymax = HTtimeRinfection_q60), alpha = .4,fill='blue') +
  
  ylim(0, 1)+
  scale_y_continuous(
    name = "Light interception, Susceptible tissue, Pathogen suitability",
    sec.axis = sec_axis(~ . * 10, name = "Yield (Mg/ha)")
  ) +
  theme_classic() +
  theme(legend.position = "none") 

