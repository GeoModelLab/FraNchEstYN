# Remove objects from the Global Environment----
rm(list=ls())

#libraries
library(sensitivity) #for doing sensitivity
library(tidyverse)
#devtools::install_github("GeoModelLab/FraNchEstYN")
library(FraNchEstYN)

#set this directory as the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
thisDir<-getwd()

##----------------------------------------------------------------
##generate sensitivity experiment
##ONLY ONCE BEFORE SENSITIVITY SIMULATIONS
##----------------------------------------------------------------
options(scipen=999)
param<-read.csv('franchestynParameters.csv') 

#1. calibrate crop parameters

## mgt data
site_sowing_dates <- list(
  FIJO = 127,
  ININ = 299,
  ITPO = 323,
  TRIZ = 320,
  UKRO = 289,
  USMA = 360
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
weather_data <- do.call(rbind, lapply(weather_list, read_weather_with_site)) 
  #|> 
  #rename(rad=SRAD, Year=YYYY, Month = MM, Day=DD)


## reference data
reference_list <- list.files(paste0(getwd(),'//referenceData'),full.names = T)
reference_data <- do.call(rbind, lapply(reference_list, read.csv)) |> 
  rename(site = sName, YieldAttainable = WGRN, AGB = WTOP, Year = Pyear) |> 
  mutate(YieldAttainable = YieldAttainable*10,
         AGB = AGB*10)

#1. crop calibration
sites<-unique(weather_data$site)

start_end<-c(1980,2011)

s<-sites[2]

# Initialize empty lists to store results
all_parameters <- list()
all_outputs <- list()
all_metrics <- list()

#adjust param bounds
thisCropParam <- cropParameters$wheat
thisCropParam$RadiationUseEfficiency$calibration<-T
thisCropParam$CycleLength$max<-3500
thisCropParam$HalfIntGrowth$min<-15
thisCropParam$PartitioningMaximum$calibration<-F

for(s in sites)
{
  thisWeather <- weather_data |> 
    filter(site == s)
  
  thisReference <- reference_data |> 
    filter(site == substr(s, 1, 4)) |> 
    mutate(Disease=NA) 
  
  thisManagement <- management_data |> 
    filter(site == substr(s, 1, 4))
  
  df<-franchestyn(weather_data = thisWeather,
                  management_data = thisManagement,
                  reference_data = thisReference,
                  cropParameters = cropParameters$wheat,
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

#plots
ggplot(combined_outputs |> filter(GrowingSeason<1990), aes(x=DaysAfterSowing)) + 
  geom_line(aes(y=YieldAttainable/20000),col='red')+
  geom_point(aes(y=YieldRef/20000),col='red')+
  geom_line(aes(y=LightInterception),col='green2')+
  geom_point(aes(y=LightInterceptionRef),col='green2')+
  geom_line(aes(y=AGBattainable/20000),col='blue')+
  geom_point(aes(y=AGBRef/20000),col='blue')+
  facet_grid(site~GrowingSeason)

param<-readRDS(paste0("cropparameters/ITPO0QXX_parameters.rds"))
df<-parameters_to_df(param)

##
m_levels=4
m_grid.jump = m_levels/2
## 
#length(unique(param$parameter))

###morris design
morris_df = morris(model = NULL, factors = param$parameter, r = 300,
                  design = list(type = "oat", levels = m_levels,
                                grid.jump = m_grid.jump),
                  binf = param$min, bsup = param$max, scale = FALSE)

dfParsets<-morris_df$X 
#save RDS file primary
saveRDS(morris_df, paste0(thisDir, "\\parametersDesign.rds"))

toWrite<-as.data.frame(morris_df$X) %>%
   dplyr::mutate(Model = "franchestyn",
          ID_run = 1:n()) %>%
   pivot_longer(-c(Model,ID_run),names_to="parameter",values_to="value")
# 
# 
write.csv(toWrite, paste0("testMorris.csv"),row.names=F)
morris_df<-readRDS(paste0(getwd(), "\\parametersDesign.rds"))
##
# this path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load_all("..\\..\\R\\.") # Working directory should be in the package SCC_R_package

#devtools::install('..//..//FRaNchEstYn')
#devtools::document('..//..//FRaNchEstYn')
library('fRanchestyn')

# this path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# list of output files 
filesOuputs<- list.files("..//AgMIP", 
                         pattern = '.AgMIP',
                         full.names = T)
# load only header
header <- as.character(read.table(filesOuputs[1], skip =4, header =F, nrows=1))
# create a single dataframe
weather<-lapply(filesOuputs, function(x) 
{
  # Read the content of the file
  file_content <- readLines(filesOuputs[[1]])
  # Extract the LAT value from the specific line (it’s on the second line in your example)
  lat_line <- file_content[4]  # Assuming LAT is on the second line
  lat_value <- as.numeric(strsplit(lat_line, "\\s+")[[1]][3])  # Extract the LAT value, assuming it's the second value
  
  out <- tryCatch(read.table(x, 
                             header = F, 
                             skip = 5), error = function(e) NULL)
  if (!is.null(out)) {
    out$source_file <- x
    out$lat<-lat_value
  }
  return(out)
})
#assuming the same header/columns for all files
weather = do.call("rbind", weather) 
colnames(weather) = c(header, 'file')
colnames(weather)[[length(colnames(weather))]] = "LAT"
weather$Site <- gsub("Meteo/|\\.AgMIP", "", weather$file)
colnames(weather)[[1]]<-"DATE"
# Extract year and day of the year
year <- floor(weather$DATE / 1000)  # First 4 digits (year)
day_of_year <- weather$DATE %% 1000  # Last 3 digits (day of the year)
# Convert to Date
weather$DATE <- as.Date(paste(year, day_of_year, sep = "-"), format = "%Y-%j")

#load optimized crop parameters
cropCalibration<-list.files('..//cropCalibration',full.names = T)
# Read and combine all CSV files into one data frame
cropCalibration <- do.call(rbind, lapply(cropCalibration, read.csv))


paramsSA<-read.csv('testMorris.csv')

i<-1




outDfs<-list(list())
outYieldGap<-list(list())
outDisSev<-list(list())
outMorrisYieldGap<-list()
outMorrisDisSev<-list()
outDfsUncertainty<-list()

site<-1
for(site in 1:length(unique(weather$Site)))
{
  thisWeather <- weather |> 
    filter(Site == unique(weather$Site)[site])
  
  location <- sub(".*//.*?/([A-Z]{4}).*", "\\1", unique(weather$Site)[site])
  
  par<-1
  for(par in 1:length(unique(paramsSA$ID_run)))
  #for(par in 1:5)  
  {
      thisPar<- paramsSA |> filter(ID_run == par) %>%
        select(-Model,ID_run)
      
      #assign crop parameters  
      paramCrop<-cropCalibration |> 
        filter(site == location)
      
      paramDisease<-paramsSA |> 
        filter(ID_run==par) |> 
        select(-c(Model,ID_run)) |> 
        pivot_wider(names_from = parameter, values_from = c(value))
      
      # Step 3: Perform a run with optimized parameters
      optimizedSimulation <- fRanchestyn_daily(
        thisWeather|> filter(YYYY>=2004 & YYYY<=2009),
        #weather |> filter(YYYY>=2004 & YYYY<=2009, Site==unique(thisSiteParams$Location)),
        paramCrop, 
        paramDisease, 
        estimateRad = F,
        sowingDate = site_sowing_dates[[site]], 
        leafWetnessRH = 89
      ) |> 
        filter(season!=0)
      
      # Extract yield gap 
      yieldGap <- optimizedSimulation |> 
        group_by(season) |> 
        slice_tail() |> 
        mutate(yieldGap = (yieldPotState-yieldDisState)/yieldPotState) |> 
        ungroup() |> 
        summarise(yieldGap=mean(yieldGap))
      outYieldGap[[par]] <- yieldGap
      
      # extract dis sev
      DisSev <- optimizedSimulation |> 
        group_by(season) |> 
        slice_tail() |> 
        ungroup() |> 
        summarise(DisSev=mean(HTaffected))
      outDisSev[[par]]<-DisSev
      
      dailyOut<-optimizedSimulation |> 
        select(site,season,daysAfterSowing,
               fIntPotState,bioPotState,yieldPotState,
               HTtime,HTlatent,HTsporulating,HTdead,HTaffected,
               fIntDisState,bioDisState,yieldDisState)
      outDfs[[par]] <- dailyOut
  }
  
      # Process fruits results for this experiment and soil
      outVarYieldGap <- lapply(outYieldGap, "[[", "error") |> 
        unlist(use.names = FALSE)
      # Compute sensitivity indices for fruits
      resultsYieldGap <- tell(morris_df, as.numeric(outVarYieldGap))
      outMorrisYieldGap[[location]] <- resultsYieldGap
      
      # Process fruits results for this experiment and soil
      outVarDisSev <- lapply(outDisSev, "[[", "error") |> 
        unlist(use.names = FALSE)
      # Compute sensitivity indices for fruits
      resultsDisSev <- tell(morris_df, as.numeric(outVarDisSev))
      outMorrisYieldGap[[location]] <- resultsDisSev
      
      # Save uncertainty outputs for this soil and experiment
      outDfsUncertainty[[location]] <- outDfs
}

####save RDS objects outMorris outDfsUncertainty
saveRDS(outMorrisYieldGap, 'outMorrisYieldGap.rds')
saveRDS(outMorrisDisSev, 'outMorrisDisSev.rds')

saveRDS(outDfsUncertainty, 'outDfsUncertainty.rds')

outMorris <- readRDS('outMorris.rds')

exp<-1
results_Err_list<-list()
for(exp in 1:length(outMorris))
{
  #fruits
  dfErrors<-as.data.frame(apply(outMorris[exp][[1]][[11]], 2, mean))
 
  # Rename the single column that was generated to "mean_values"
  colnames(dfErrors) <- "mu"
 
  #Fruits dataset
  dfErrors$parameter <- rownames(dfErrors)
  dfErrors$experiment <- exp
  dfErrors$muStar<-as.data.frame(apply(outMorris[exp][[1]][[11]], 2, 
                                           function(x) mean(abs(x))))[,1]
  dfErrors$sigma<-as.data.frame(apply(outMorris[exp][[1]][[11]], 2, sd))[,1]
  
  # Reset row names to ensure they are not included in the final result
  rownames(dfErrors) <- NULL
  
  # Rearrange the columns into the desired order
  new_order <- c("experiment", "parameter", "mu", "muStar", "sigma")
  dfErrors <- dfErrors[, new_order]
 
  
  # Store results in the predefined list
  results_Err_list[[exp]] <- list(dfErrors)
  
 
}



# Combine all data frames into one
allErrorsExperiments <- do.call(rbind, lapply(results_Err_list, function(x) {
  do.call(rbind, x)  # Combine sand, loam, clay within each experiment
}))


# Step 1: Extract row names
rownames_col <- rownames(allErrorsExperiments)


# Step 4: Remove the row names by resetting them to numeric indices
rownames(allErrorsExperiments) <- NULL

allErrorsExperiments <- allErrorsExperiments[, new_order]

unique(allErrorsExperiments$parameter)

#TODO: ANALYSIS!!!!
new_acronyms <- c(
  "parDormancy_limitingPhotoperiod" = "DorLP", 
  "parDormancy_notLimitingPhotoperiod" = "DorNLP", 
  "parDormancy_limitingTemperature" = "DorLT", 
  "parDormancy_notLimitingTemperature" = "DorNLT", 
  "parDormancy_photoThermalThreshold" = "DorPT", 
  "parEndodormancy_limitingLowerTemperature" = "EndoLLT", 
  "parEndodormancy_notLimitingLowerTemperature" = "EndoNLLT", 
  "parEndodormancy_notLimitingUpperTemperature" = "EndoNLUT", 
  "parEndodormancy_limitingUpperTemperature" = "EndoLUT", 
  "parEndodormancy_chillingThreshold" = "EndoTh", 
  "parEcodormancy_notLimitingTemperature" = "EcoNLT", 
  "parEcodormancy_notLimitingPhotoperiod" = "EcoNLP", 
  "parEcodormancy_photoThermalThreshold" = "EcoTh", 
  "parGrowth_minimumTemperature" = "GroTn", 
  "parGrowth_optimumTemperature" = "GroTo", 
  "parGrowth_maximumTemperature" = "GroTx", 
  "parGrowth_thermalThreshold" = "GroTh", 
  "parGreendown_thermalThreshold" = "GreTh", 
  "parSenescence_limitingPhotoperiod" = "SenLP", 
  "parSenescence_notLimitingPhotoperiod" = "SenNLP", 
  "parSenescence_limitingTemperature" = "SenLT", 
  "parSenescence_notLimitingTemperature" = "SenNLT", 
  "parSenescence_photoThermalThreshold" = "SenTh", 
  "parVegetationIndex_nNDVIGrowth" = "GroVI", 
  "parVegetationIndex_nNDVISenescence" = "SenVI", 
  "parVegetationIndex_nNDVIGreendown" = "GreVI", 
  "parVegetationIndex_nNDVIEcodormancy" = "EcoVI", 
  "parVegetationIndex_nNDVIEndodormancy" = "EndoVI", 
  "parVegetationIndex_pixelTemperatureShift" = "Tshift", 
  "parVegetationIndex_minimumNDVI" = "VIn", 
  "parVegetationIndex_maximumNDVI" = "VIx"
)

# Example of how to apply the mapping to a dataset column
# Substituting the parameter column in the data frame
allErrorsExperiments$parameter <- new_acronyms[allErrorsExperiments$parameter]

excludeParam<-c('GreVI','GroVI','VIx','VIn','EndoVI','EcoVI','SenVI')

#a simple boxplot with parameter relevance
ggplot(allErrorsExperiments|> 
         filter(!parameter %in% excludeParam )) + 
  geom_col(aes(x=reorder(factor(parameter),-muStar), y = muStar),
               outlier.size=.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "top")+
  xlab("")
ggsave("muStarErrors.png",height=5,width=9)


#mu_sigma plot

#compute mean and sd of the normalized msoil_type#compute mean and sd of the normalized metrics
df_Errors_normalized_synth <- allErrorsExperiments |> 
  group_by(parameter) |> 
  summarise(muStar=mean(muStar),
            muStarSD=sd(muStar),
            sigma=mean(sigma),
            sigmaSD=sd(sigma))
unique(df_Errors_normalized_synth$parameter)


main_plot<-ggplot(df_Errors_normalized_synth |> 
                    filter(!parameter %in% excludeParam )) + 
  geom_point(aes(x=muStar, y = sigma, color = parameter),size=0.5) +
  geom_errorbarh(aes(xmin = muStar - muStarSD, xmax = muStar + muStarSD, 
                     y = sigma, height = 0,color=parameter),linewidth=0.3) +
  geom_errorbar(aes(ymin = sigma - sigmaSD, ymax = sigma + sigmaSD, 
                    x = muStar, width = 0,color=parameter),linewidth=0.3) +
  geom_text(aes(x = muStar, y = sigma, label = parameter,color=parameter), 
            vjust = 1, hjust = 1, size = 3.5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "none", color = NA)+
  labs(
    x = "Mean of muStar (Normalized)",
    y = "Mean of sigma (Normalized)",
    title = "SWELL - sensitivity analysis with Morris method",
    subtitle = "horizontal and vertical error bars are mean +- sd"
  )

main_plot
ggsave("mu_sigma_Fruits.png",height = 5, width=10)

#empty list
uncertainty_list<-list()

#general counter
counter<-1

exp<-1
#TAKES ONE MINUTE!
#loop on the list to build a single df 
for(exp in 1:length(outDfsUncertainty))
{
  #select one experiment
  thisExperimentList<-df_uncertainty[exp][[1]]
  
  #loop on the runs in this experiment
  for(run in 1:length(thisExperimentList))
  {
    #take the data as dataframe
    dfOutputs<-as.data.frame(thisExperimentList[run])
    dfOutputs$run<-run
    
    #assign the df to the list
    uncertainty_list[[counter]]<-dfOutputs
    
    #increase the counter
    counter<-counter+1
  }
}



#read rds
df_uncertainty <- readRDS('outDfsUncertainty.rds')
# Initialize an empty list to store the combined DataFrames
all_final_dfs <- list()

# Loop through each of the six elements in the main list
for (i in seq_along(df_uncertainty)) {
  # Combine all data frames within the current element (list of lists)
  combined_df <- do.call(rbind, df_uncertainty[[i]])
  
  # Add a column with the list index
  combined_df$list_id <- i
  
  # Add the modified DataFrame to the results list
  all_final_dfs[[i]] <- combined_df
}

# Combine all the final DataFrames into one
final_combined_df <- do.call(rbind, all_final_dfs) |> 
  mutate(site = str_extract(site, "(?<=/)[A-Z]{4}"))


#compute synthetic measures
library(data.table)

testData<-final_combined_df |> filter(list_id<400,season<2009)

summary(testData)

# List of variables to include in the summary
variables <- c("fIntPotState", "fIntDisState", "HTlatent", "HTsporulating", "HTdead", "HTaffected",
               "bioDisState","yieldDisState",'bioPotState','yieldPotState')

# Compute 25th, 50th (median), and 75th percentiles for all variables
testData_summary <- testData %>%
  filter(list_id < 400, season < 2009) %>%
  group_by(daysAfterSowing, site, season) %>%
  summarize(
    across(
      all_of(variables),
      list(
        median = ~median(.x, na.rm = TRUE),
        lower = ~quantile(.x, 0.40, na.rm = TRUE),
        upper = ~quantile(.x, 0.60, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# Plot all variables with ribbons for the IQR and lines for the median
ggplot(testData_summary |> filter(site=='UKRO'), aes(x = daysAfterSowing)) +
  geom_line(aes(y = fIntPotState_median), col = 'darkgreen') +
  geom_ribbon(aes(ymin = fIntDisState_lower, ymax = fIntDisState_upper), fill = 'green', alpha = 0.3) +
  geom_ribbon(aes(ymin = HTlatent_lower, ymax = HTlatent_upper), fill = 'green3', alpha = 1) +
  geom_ribbon(aes(ymin = HTsporulating_lower, ymax = HTsporulating_upper), fill = 'grey66', alpha = 0.75) +
  geom_ribbon(aes(ymin = HTdead_lower, ymax = HTdead_upper), fill = 'black', alpha = 0.75) +
  geom_ribbon(aes(ymin = HTaffected_lower, ymax = HTaffected_upper), fill = 'red3', alpha = 0.75) +
  facet_wrap(site ~ season, nrow = 2) +
  theme_classic() +
  theme(legend.position = 'none')

# Plot all variables with ribbons for the IQR and lines for the median
ggplot(testData_summary, aes(x = daysAfterSowing)) +
  geom_line(aes(y = fIntPotState_median*20000), col = 'darkgreen') +
  geom_line(aes(y = yieldPotState_median), col = 'red') +
  geom_line(aes(y = bioPotState_median), col = 'blue') +
  geom_ribbon(aes(ymin = fIntDisState_lower*20000, ymax = fIntDisState_upper*20000), fill = 'green', alpha = 0.3) +
  geom_ribbon(aes(ymin = fIntDisState_lower*20000, ymax = fIntDisState_upper*20000), fill = 'green', alpha = 0.3) +
  geom_ribbon(aes(ymin = yieldDisState_lower, 
                  ymax = yieldDisState_upper), fill = 'red', alpha = 0.7) +
  geom_ribbon(aes(ymin = bioDisState_lower, 
                  ymax = bioDisState_upper), fill = 'blue', alpha = 0.7) +
  facet_wrap(site ~ season, nrow = 6) +
  theme_classic() +
  theme(legend.position = 'none')













# Function to calculate the distance between two points (Haversine formula)
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  rad <- pi / 180
  lat1 <- lat1 * rad
  lon1 <- lon1 * rad
  lat2 <- lat2 * rad
  lon2 <- lon2 * rad
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371 # Earth's radius in kilometers
  
  return(R * c) # Distance in kilometers
}

# Function to find the closest file
find_closest_file <- function(target_lat, target_lon, folder_path) {
  # Get a list of files in the folder
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize variables to store the closest file and the minimum distance
  closest_file <- NULL
  min_distance <- Inf
  
  # Loop through each file
  for (file in files) {
    # Extract latitude and longitude from the filename
    file_name <- basename(file)
    coords <- strsplit(file_name, "_|\\.csv")[[1]]
    file_lat <- as.numeric(coords[1])
    file_lon <- as.numeric(coords[2])
    
    # Calculate the distance
    distance <- calculate_distance(target_lat, target_lon, file_lat, file_lon)
    
    # Update the closest file if the current distance is smaller
    if (distance < min_distance) {
      min_distance <- distance
      closest_file <- file
    }
  }
  
  # Return the closest file
  return(closest_file)
}

##
###create an empty list to store the results of the sensitivity runs
#outDfsExperiments<-list(list())
#outFruitsExperiments<- list(list())
#outBrixExperiments<-list(list())
#outMorris<-list()
#outDfsUncertainty<-list()
##
###for debugging
##parset<-1
##idExp<-1
##
##read the design parametersDesign.rds
#morris <- readRDS("parametersDesign.rds")
#
##select experiment
#irrigation_df<-irrigation_df
#
#idExp<-1
#
##set soils for sensitivity analysis
## Pre-allocate the lists with soil names
#soil_names <- c("sand", "loam", "clay")
##sand, silt, clay
#fc <- c(.12,.25,.36)
#wp <- c(.045,.12,.22)
#bd<-c(1.65,1.5,1.35)
#
#toWriteTest<-toWrite %>% 
#  filter(ID_run<4)
#
## Helper function to dynamically ensure a nested list structure
#ensure_nested_list <- function(list_obj, outer_index, inner_index) {
#  # Ensure the outer index exists
#  if (is.null(list_obj[[outer_index]])) {
#    list_obj[[outer_index]] <- list()
#  }
#  # Ensure the inner index exists
#  if (is.null(list_obj[[outer_index]][[inner_index]])) {
#    list_obj[[outer_index]][[inner_index]] <- list()
#  }
#  return(list_obj)
#}
#
## Pre-allocate nested structures explicitly
#outMorrisFruits <- vector("list", length(fc))  # One list per soil
#names(outMorrisFruits) <- soil_names
#
#outMorrisBrix <- vector("list", length(fc))    # One list per soil
#names(outMorrisBrix) <- soil_names
#
#outDfsUncertainty <- vector("list", length(fc))  # One list per soil
#names(outDfsUncertainty) <- soil_names
#
## Initialize nested lists for each soil
#for (soil in seq_along(fc)) {
#  outMorrisFruits[[soil]] <- vector("list", length(unique(irrigation_df$ID)))
#  outMorrisBrix[[soil]] <- vector("list", length(unique(irrigation_df$ID)))
#  outDfsUncertainty[[soil]] <- vector("list", length(unique(irrigation_df$ID)))
#}
#
#
## Track total start time
#start_time <- Sys.time()
#
## Main computation loop
#for (soil in seq_along(fc)) {
#  
#  # Initialize experiments lists for this soil
#  outFruitsExperiments <- vector("list", length(unique(toWrite$ID_run)))
#  outBrixExperiments <- vector("list", length(unique(toWrite$ID_run)))
#  outDfsExperiments <- vector("list", length(unique(toWrite$ID_run)))
#  
#  # Loop over experiments
#  for (idExpIndex in seq_along(unique(irrigation_df$ID))) {
#    idExp <- unique(irrigation_df$ID)[idExpIndex]
#    
#    # Retrieve current experiment's irrigation data
#    thisIDirrigation <- irrigation_df |> filter(ID == idExp)
#    
#    # Loop over parameter sets
#    for (parset in unique(toWrite$ID_run)) {
#      
#      # Get a single parameter set
#      thisParset <- toWrite |> filter(ID_run == parset)
#      
#      # Build the param object
#      param <- thisParset |> 
#        select(-c(ID_run, Model)) |> 
#        pivot_wider(names_from = parameter, values_from = value)
#      
#      # Add soil-specific parameters
#      param$FieldCapacity <- fc[[soil]]
#      param$WiltingPoint <- wp[[soil]]
#      param$BulkDensity <- bd[[soil]]
#      
#      # Call the cumba_experiment function
#      outputs <- cumba_experiment(
#        weather |> 
#          mutate(year = year(Date)) |> 
#          filter(year == unique(thisIDirrigation$YEAR)),
#        param,
#        estimateRad = TRUE, 
#        estimateET0 = TRUE, 
#        thisIDirrigation
#      )
#      
#      outputs$parset <- parset
#      
#      # Extract agbFruits and brix outputs
#      agbFruits <- outputs |> 
#        slice_tail(n = 1) |> 
#        select(experiment, fruitsStateAct)
#      
#      brix <- outputs |> 
#        slice_tail(n = 1) |> 
#        select(experiment, brixAct)
#      
#      # Save outputs for this parameter set
#      outFruitsExperiments[[parset]] <- agbFruits
#      outBrixExperiments[[parset]] <- brix
#      outDfsExperiments[[parset]] <- outputs
#    }
#    
#    # Aggregate results for sensitivity analysis
#    
#    # Process fruits results for this experiment and soil
#    outVarFruits <- lapply(outFruitsExperiments, "[[", "fruitsStateAct") |> 
#      unlist(use.names = FALSE)
#    
#    # Compute sensitivity indices for fruits
#    resultsFruits <- tell(morris_df, as.numeric(outVarFruits))
#    outMorrisFruits[[soil]][[idExpIndex]] <- resultsFruits
#    
#    # Process brix results for this experiment and soil
#    outVarBrix <- lapply(outBrixExperiments, "[[", "brixAct") |> 
#      unlist(use.names = FALSE)
#    
#    # Compute sensitivity indices for brix
#    resultsBrix <- tell(morris_df, as.numeric(outVarBrix))
#    outMorrisBrix[[soil]][[idExpIndex]] <- resultsBrix
#    
#    # Save uncertainty outputs for this soil and experiment
#    outDfsUncertainty[[soil]][[idExpIndex]] <- outDfsExperiments
#  }
#}
##
### Calculate and print the total elapsed time
##total_elapsed_time <- Sys.time() - start_time
##cat("Total elapsed time:", total_elapsed_time, "\n")
##
####save RDS objects outMorris outDfsUncertainty
#saveRDS(outMorrisFruits, 'outMorrisFruits.rds')
#saveRDS(outMorrisBrix, 'outMorrisBrix.rds')
##
#saveRDS(outDfsUncertainty, 'outDfsUncertainty.rds')
#takes around 2 minutes
outDfsUncertainty<-readRDS('outDfsUncertainty.rds')
outMorrisFruits <- readRDS('outMorrisFruits.rds')
outMorrisBrix <- readRDS('outMorrisBrix.rds')

#fill the dataframe
exp<-1
results_Fruits_list<-list(list())
results_Brix_list<-list(list())


#fruits
thisoutMorrisFruitsSand<-outMorrisFruits$sand
thisoutMorrisFruitsLoam<-outMorrisFruits$loam
thisoutMorrisFruitsClay<-outMorrisFruits$clay
#brix
thisoutMorrisBrixSand<-outMorrisBrix$sand
thisoutMorrisBrixLoam<-outMorrisBrix$loam
thisoutMorrisBrixClay<-outMorrisBrix$clay

for(exp in 1:length(thisoutMorrisFruitsSand))
{
  #fruits
  dfFruitsSand<-as.data.frame(apply(thisoutMorrisFruitsSand[exp][[1]][[11]], 2, mean))
  dfFruitsLoam<-as.data.frame(apply(thisoutMorrisFruitsLoam[exp][[1]][[11]], 2, mean))
  dfFruitsClay<-as.data.frame(apply(thisoutMorrisFruitsClay[exp][[1]][[11]], 2, mean))
  #brix
  dfBrixSand<-as.data.frame(apply(thisoutMorrisBrixSand[exp][[1]][[11]], 2, mean))
  dfBrixLoam<-as.data.frame(apply(thisoutMorrisBrixLoam[exp][[1]][[11]], 2, mean))
  dfBrixClay<-as.data.frame(apply(thisoutMorrisBrixClay[exp][[1]][[11]], 2, mean))
  
  # Rename the single column that was generated to "mean_values"
  colnames(dfFruitsSand) <- "mu"
  colnames(dfFruitsLoam) <- "mu"
  colnames(dfFruitsClay) <- "mu"
  colnames(dfBrixSand) <- "mu"
  colnames(dfBrixLoam) <- "mu"
  colnames(dfBrixClay) <- "mu"
  
  #Fruits dataset
  dfFruitsSand$parameter <- rownames(dfFruitsSand)
  dfFruitsSand$experiment <- exp
  dfFruitsSand$muStar<-as.data.frame(apply(thisoutMorrisFruitsSand[exp][[1]][[11]], 2, 
                                   function(x) mean(abs(x))))[,1]
  dfFruitsSand$sigma<-as.data.frame(apply(thisoutMorrisFruitsSand[exp][[1]][[11]], 2, sd))[,1]
  
  dfFruitsLoam$parameter <- rownames(dfFruitsLoam)
  dfFruitsLoam$experiment <- exp
  dfFruitsLoam$muStar<-as.data.frame(apply(thisoutMorrisFruitsLoam[exp][[1]][[11]], 2, 
                                 function(x) mean(abs(x))))[,1]
  dfFruitsLoam$sigma<-as.data.frame(apply(thisoutMorrisFruitsLoam[exp][[1]][[11]], 2, sd))[,1]
  
  dfFruitsClay$parameter <- rownames(dfFruitsClay)
  dfFruitsClay$experiment <- exp
  dfFruitsClay$muStar<-as.data.frame(apply(thisoutMorrisFruitsClay[exp][[1]][[11]], 2, 
                                     function(x) mean(abs(x))))[,1]
  dfFruitsClay$sigma<-as.data.frame(apply(thisoutMorrisFruitsClay[exp][[1]][[11]], 2, sd))[,1]
  
  #Brix dataset
  dfBrixSand$parameter <- rownames(dfBrixSand)
  dfBrixSand$experiment <- exp
  dfBrixSand$muStar<-as.data.frame(apply(thisoutMorrisBrixSand[exp][[1]][[11]], 2, 
                                   function(x) mean(abs(x))))[,1]
  dfBrixSand$sigma<-as.data.frame(apply(thisoutMorrisBrixSand[exp][[1]][[11]], 2, sd))[,1]
  
  dfBrixLoam$parameter <- rownames(dfBrixLoam)
  dfBrixLoam$experiment <- exp
  dfBrixLoam$muStar<-as.data.frame(apply(thisoutMorrisBrixLoam[exp][[1]][[11]], 2, 
                                 function(x) mean(abs(x))))[,1]
  dfBrixLoam$sigma<-as.data.frame(apply(thisoutMorrisBrixLoam[exp][[1]][[11]], 2, sd))[,1]
  
  dfBrixClay$parameter <- rownames(dfBrixClay)
  dfBrixClay$experiment <- exp
  dfBrixClay$muStar<-as.data.frame(apply(thisoutMorrisBrixClay[exp][[1]][[11]], 2, 
                                     function(x) mean(abs(x))))[,1]
  dfBrixClay$sigma<-as.data.frame(apply(thisoutMorrisBrixClay[exp][[1]][[11]], 2, sd))[,1]
  
  # Reset row names to ensure they are not included in the final result
  rownames(dfFruitsSand) <- NULL
  rownames(dfFruitsLoam) <- NULL
  rownames(dfFruitsClay) <- NULL
  rownames(dfBrixSand) <- NULL
  rownames(dfBrixLoam) <- NULL
  rownames(dfBrixClay) <- NULL
  
  # Rearrange the columns into the desired order
  new_order <- c("experiment", "parameter", "mu", "muStar", "sigma")
  dfFruitsSand <- dfFruitsSand[, new_order]
  dfFruitsLoam <- dfFruitsLoam[, new_order]
  dfFruitsClay <- dfFruitsClay[, new_order]
  dfBrixSand <- dfBrixSand[, new_order]
  dfBrixLoam <- dfBrixLoam[, new_order]
  dfBrixClay <- dfBrixClay[, new_order]
  
  # Store results in the predefined list
  results_Fruits_list[[exp]] <- list(
    sand = dfFruitsSand,
    loam = dfFruitsLoam,
    clay = dfFruitsClay
  )
  
  # Store results in the predefined list
  results_Brix_list[[exp]] <- list(
    sand = dfBrixSand,
    loam = dfBrixLoam,
    clay = dfBrixClay
  )
}


# Combine all data frames into one
allFruitsExperiments <- do.call(rbind, lapply(results_Fruits_list, function(x) {
  do.call(rbind, x)  # Combine sand, loam, clay within each experiment
}))
allBrixExperiments <- do.call(rbind, lapply(results_Brix_list, function(x) {
  do.call(rbind, x)  # Combine sand, loam, clay within each experiment
}))


# Assuming `allExperiments` is your combined data frame
# Step 1: Extract row names
rownames_col <- rownames(allFruitsExperiments)

# Step 2: Split the row names to extract the soil type (before the dot)
soil_type_col <- sub("\\..*", "", rownames_col)

# Step 3: Add the soil type as a new column
allFruitsExperiments$soil_type <- soil_type_col
allBrixExperiments$soil_type <- soil_type_col


# Step 4: Remove the row names by resetting them to numeric indices
rownames(allFruitsExperiments) <- NULL
rownames(allBrixExperiments) <- NULL


# Step 5: Reorder the columns to place `soil_type` as the first column
new_order <- c("soil_type", setdiff(names(allFruitsExperiments), "soil_type"))
allFruitsExperiments <- allFruitsExperiments[, new_order]
allBrixExperiments<-allBrixExperiments[, new_order]

# Original names and improved acronyms
new_acronyms <- c(
  "Tbase" = "Tb",              # Base temperature
  "Topt" = "Topt",               # Optimal temperature
  "Tmax" = "Tmax",               # Maximum temperature
  "Theat" = "Theat",              # Heat threshold
  "Tcold" = "Tcold",              # Cold threshold
  "FIntMax" = "IntMax",           # Fruit intensity max
  "CycleLength" = "CycleL",        # Cycle length
  "TransplantingLag" = "TraLag", # Transplanting lag
  "FloweringLag" = "FloLag",     # Flowering lag
  "HalfIntGrowth" = "SloGro",     # Half intensity growth
  "HalfIntSenescence" = "SloSen", # Half intensity senescence
  "InitialInt" = "IntTra",      # Initial intensity
  "RUE" = "RUE",               # Radiation use efficiency (unchanged)
  "KcIni" = "KcIni",             # Initial crop coefficient
  "KcMax" = "KcMax",             # Maximum crop coefficient
  "RootIncrease" = "RootInc",       # Root increase
  "RootDepthMax" = "RootMax",      # Root depth max
  "RootDepthInitial" = "RootIni",  # Root depth initial
  "WaterStressSensitivity" = "WSsens", # Water stress sensitivity
  "FloweringSlope" = "SloFlo",     # Flowering slope
  "FloweringMax" = "MaxFlo",       # Flowering max
  "k0" = "k0",                 # k0 (unchanged)
  "FruitWaterContentMin" = "FruWCmin", # Fruit water content min
  "FruitWaterContentMax" = "FruWCmax", # Fruit water content max
  "FruitWaterContentInc" = "FruWCinc", # Fruit water content increase
  "FruitWaterContentDecreaseMax" = "FruDec" # Fruit water content decrease max
)

# Substituting the parameter column in the data frame
allFruitsExperiments$parameter <- new_acronyms[allFruitsExperiments$parameter]
allBrixExperiments$parameter <- new_acronyms[allBrixExperiments$parameter]


#a simple boxplot with parameter relevance
ggplot(allFruitsExperiments) + 
  geom_boxplot(aes(x=reorder(factor(parameter),-muStar), y = muStar,fill=soil_type),
               outlier.size=.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "top")+
  xlab("")+
  scale_fill_manual(values=c("gold","brown","brown4"))
ggsave("muStarFruits.png",height=5,width=9)

#a simple boxplot with parameter relevance
ggplot(allBrixExperiments) + 
  geom_boxplot(aes(x=reorder(factor(parameter),-muStar), 
                   y = muStar,fill=soil_type),
               outlier.size=.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "top")+
  xlab("")+
  scale_fill_manual(values=c("gold","brown","brown4"))
ggsave("muStarBrix.png",height=5,width=9)

# Normalize the senstivity metrics by `experiment` for better comparison
df_Fruits_normalized <- allFruitsExperiments %>%
  group_by(experiment,soil_type) %>%
  mutate(
    mu_norm = (mu - min(mu)) / (max(mu) - min(mu)),
    muStar_norm = (muStar - min(muStar)) / (max(muStar) - min(muStar)),
    sigma_norm = (sigma - min(sigma)) / (max(sigma) - min(sigma))
  ) %>%
  ungroup()

#plot the normalized sensitivity metrics
ggplot(df_Fruits_normalized) + 
  geom_boxplot(aes(x=reorder(factor(parameter),-muStar_norm), 
                   y = muStar_norm,fill=soil_type))+
  theme(axis.text.x = element_text(angle=-90))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "top")+
  xlab("")+
  scale_fill_manual(values=c("gold","brown","brown4"))
ggsave("muStarFruitsNormalized.png",height=5,width=9)


# Normalize the senstivity metrics by `experiment` for better comparison
df_Brix_normalized <- allBrixExperiments %>%
  group_by(experiment,soil_type) %>%
  mutate(
    mu_norm = (mu - min(mu)) / (max(mu) - min(mu)),
    muStar_norm = (muStar - min(muStar)) / (max(muStar) - min(muStar)),
    sigma_norm = (sigma - min(sigma)) / (max(sigma) - min(sigma))
  ) %>%
  ungroup()

#plot the normalized sensitivity metrics
ggplot(df_Brix_normalized) + 
  geom_boxplot(aes(x=reorder(factor(parameter),-muStar_norm), 
                   y = muStar_norm,fill=soil_type))+
  theme(axis.text.x = element_text(angle=-90))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "top")+
  xlab("")+
  scale_fill_manual(values=c("gold","brown","brown4"))
ggsave("muStarBrixNormalized.png",height=5,width=9)


#compute mean and sd of the normalized msoil_type#compute mean and sd of the normalized metrics
df_Fruits_normalized_synth <- df_Fruits_normalized |> 
  group_by(parameter,soil_type) |> 
  summarise(muStar=mean(muStar_norm),
            muStarSD=sd(muStar_norm),
            sigma=mean(sigma_norm),
            sigmaSD=sd(sigma_norm))

#main sensitivity plot
# Define a custom color palette for soil types
soil_colors <- c("sand" = "#F4A460", "loam" = "#8B4513", "clay" = "#D2691E") 

main_plot<-ggplot(df_Fruits_normalized_synth) + 
  geom_point(aes(x=muStar, y = sigma, color = parameter),size=0.5) +
  geom_errorbarh(aes(xmin = muStar - muStarSD, xmax = muStar + muStarSD, 
                     y = sigma, height = 0,color=parameter),linewidth=0.3) +
  geom_errorbar(aes(ymin = sigma - sigmaSD, ymax = sigma + sigmaSD, 
                    x = muStar, width = 0,color=parameter),linewidth=0.3) +
  geom_text(aes(x = muStar, y = sigma, label = parameter,color=parameter), 
            vjust = 1, hjust = 1, size = 3.5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "none", color = NA)+
  labs(
    x = "Mean of muStar (Normalized)",
    y = "Mean of sigma (Normalized)",
    title = "cumbà - sensitivity analysis with Morris method",
    subtitle = "horizontal and vertical error bars are mean +- sd"
  )+
  facet_wrap(~soil_type,nrow=1)

main_plot
ggsave("mu_sigma_Fruits.png",height = 5, width=10)


#compute mean and sd of the normalized msoil_type
df_Brix_normalized_synth <- df_Brix_normalized |> 
  group_by(parameter,soil_type) |> 
  summarise(muStar=mean(muStar_norm),
            muStarSD=sd(muStar_norm),
            sigma=mean(sigma_norm),
            sigmaSD=sd(sigma_norm))

#main sensitivity plot
#Define a custom color palette for soil types
soil_colors <- c("sand" = "#F4A460", "loam" = "#8B4513", "clay" = "#D2691E") 

main_plot<-ggplot(df_Brix_normalized_synth) + 
  geom_point(aes(x=muStar, y = sigma, color = parameter),size=0.5) +
  geom_errorbarh(aes(xmin = muStar - muStarSD, xmax = muStar + muStarSD, 
                     y = sigma, height = 0,color=parameter),linewidth=0.3) +
  geom_errorbar(aes(ymin = sigma - sigmaSD, ymax = sigma + sigmaSD, 
                    x = muStar, width = 0,color=parameter),linewidth=0.3) +
  geom_text(aes(x = muStar, y = sigma, label = parameter,color=parameter), 
            vjust = 1, hjust = 1, size = 3.5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=-90),
        legend.position = "none", color = NA)+
  labs(
    x = "Mean of muStar (Normalized)",
    y = "Mean of sigma (Normalized)",
    title = "cumbà - sensitivity analysis with Morris method",
    subtitle = "horizontal and vertical error bars are mean +- sd"
  )+
  facet_wrap(~soil_type,nrow=1)

main_plot
ggsave("mu_sigma_Brix.png",height = 5, width=10)

  # Create inset plot to better see differences in parameters relevance
#inset_plot <- ggplot(df_normalized_synth) + 
#  geom_point(aes(x=muStar, y = sigma))+
#  geom_errorbarh(aes(xmin = muStar - muStarSD, xmax = muStar + muStarSD, 
#                     y = sigma, height = 0)) +
#  geom_errorbar(aes(ymin = sigma - sigmaSD, ymax = sigma + sigmaSD, 
#                    x = muStar, width = 0)) +
#  geom_text(aes(x = muStar, y = sigma, label = parameter), 
#            vjust = 1, hjust = 1, size = 2.5, color = "black") +
#  coord_cartesian(xlim = c(0, .17), ylim = c(0, .17)) +
#  theme_bw() +
#  facet_wrap(~soil_type)
#
## Combine the plots with patchwork
#(main_plot +
#    inset_element(inset_plot, 
#                  left = 0, right = 0.4, top = 1, bottom = 0.4))
#
#TAKES SOME MINUTES!!!
#uncertainty analysis
#df_uncertainty <- readRDS('outDfsUncertainty.rds')

#empty list
uncertainty_listSand<-list()
uncertainty_listLoam<-list()
uncertainty_listClay<-list()

#general counter
counter<-1
outDfsUncertaintySand<-outDfsUncertainty$sand
outDfsUncertaintyLoam<-outDfsUncertainty$loam
outDfsUncertaintyClay<-outDfsUncertainty$clay

#TAKES ONE MINUTE!
#loop on the list to build a single df 
for(exp in 1:length(outDfsUncertaintySand))
{
  #select one experiment
  thisExperimentSandList<-outDfsUncertaintySand[exp][[1]]
  thisExperimentLoamList<-outDfsUncertaintyLoam[exp][[1]]
  thisExperimentClayList<-outDfsUncertaintyClay[exp][[1]]
  
  #loop on the runs in this experiment
  for(run in 1:length(thisExperimentSandList))
  {
    #take the data as dataframe
    dfSand<-as.data.frame(thisExperimentSandList[run])
    dfLoam<-as.data.frame(thisExperimentLoamList[run])
    dfClay<-as.data.frame(thisExperimentClayList[run])
    
    #assign the df to the list
    uncertainty_listSand[[counter]]<-dfSand
    uncertainty_listLoam[[counter]]<-dfLoam
    uncertainty_listClay[[counter]]<-dfClay
    
    #increase the counter
    counter<-counter+1
  }
}

#TAKES ONE MINUTE!!!!
# Combine all data frames into one
allExperimentsUncertaintySand <- do.call(rbind, uncertainty_listSand)
allExperimentsUncertaintyLoam <- do.call(rbind, uncertainty_listLoam)
allExperimentsUncertaintyClay <- do.call(rbind, uncertainty_listClay)

saveRDS(allExperimentsUncertaintySand, "testUncertaintySand.rds")
saveRDS(allExperimentsUncertaintyLoam, "testUncertaintyLoam.rds")
saveRDS(allExperimentsUncertaintyClay, "testUncertaintyClay.rds")

# Define the desired percentiles
percentiles <- c(0.10, 0.25, 0.40, 0.5, 0.60, 0.75, 0.90)

#TAKES SEVERAL MINUTES!!!!!!
#compute synthetic measures
library(data.table)

# Convert to data.table
dtSand <- as.data.table(allExperimentsUncertaintySand)
dtLoam <- as.data.table(allExperimentsUncertaintyLoam)
dtClay <- as.data.table(allExperimentsUncertaintyClay)

library(future)
library(furrr)

# Set up parallel processing
plan(multisession) # Automatically uses available cores

# Function to compute quantiles for a group
compute_quantiles <- function(data, percentiles) {
  lapply(data, function(col) {
    if (is.numeric(col)) {
      sapply(percentiles, function(p) quantile(col, probs = p, na.rm = TRUE))
    } else {
      NULL
    }
  })
}

# Ensure proper types for grouping variables
dtSand[, experiment := as.character(experiment)]
dtSand[, doy := as.character(doy)]

# Specify numeric columns for .SD
numeric_cols <- names(dtSand)[sapply(dtSand, is.numeric)]

# Compute quantiles
resultSand <- dtSand[, lapply(.SD, function(col) {
  if (is.numeric(col)) {
    # Generate quantiles for each percentile
    quantiles <- sapply(percentiles, function(p) quantile(col, probs = p, na.rm = TRUE))
    
    # Create a data.table with the corresponding percentiles
    data.table(percentile = percentiles, quantile = quantiles)
  } else {
    NULL
  }
}), by = .(experiment, doy), .SDcols = numeric_cols]

# Flatten the list of data.tables into one data.table
resultSand <- rbindlist(resultSand)



#write the outputs
write_rds(resultSand,'uncertainty_synth_out_sand.rds')

df_synth<-readRDS('uncertainty_synth_out_sand.rds')
head(df_synth)
df_synth$doy<-as.numeric(as.character(df_synth$doy))

ggplot(df_synth,aes(x=doy))+
  geom_col(aes(y=1000,fill=cycleCompletion),width=2,alpha=.5)+
  #geom_col(aes(y=-400,fill=cycleCompletion),width=2,alpha=.5)+
  stat_summary(geom='ribbon',aes(y=carbonStateAct),
              fill='red',alpha=.5)+
  stat_summary(geom='ribbon',aes(y=fIntAct*100),
              fill='green4',alpha=.4)+
  #geom_ribbon(aes(ymin=-rootState*5,ymax=-rootState*5),
  #            fill='brown')+
  #geom_ribbon(aes(ymin=300-(wc2mm+wc1mm+wc3mm)/3*10,
  #                ymax=300-(wc2mm+wc1mm+wc3mm)/3*10),
  #            fill='blue',alpha=.2) +
  #geom_ribbon(aes(ymin=-100+wc1mm,ymax=-100+wc1mm),
  #            fill='cyan')+
  #geom_ribbon(aes(ymin=waterStress*100 ,ymax=waterStress*100),
  #            fill='gold')+
  #geom_ribbon(aes(ymin=floweringRateAct*100 ,
  #                ymax=floweringRateAct*100),
  #            fill='pink')+
  #stat_summary(aes(y=floweringRateIde*100),
  #          col='pink4')+
  #geom_ribbon(aes(ymin=fruitsStateAct,
  #                ymax=fruitsStateAct),
  #            fill='black')+
  geom_col(aes(y=irrigation),fill='blue')+
  geom_col(aes(y=p),fill='cyan4')+
  facet_wrap(~experiment)+
  scale_fill_gradient(low = "white", high = "green")  +
  theme_classic()+
  theme(legend.position = 'none')








#Loam
# Compute quantiles in parallel for each numeric column
resultLoam <- dtLoam[, lapply(.SD, function(col) {
  if (is.numeric(col)) {
    sapply(percentiles, function(p) quantile(col, probs = p, na.rm = TRUE))
  }
}), by = .(experiment, doy)]

# Add column names
setnames(resultLoam, old = names(result)[-c(1, 2)], 
         new = paste0(rep(names(dtLoam)[sapply(dtLoam, is.numeric)], each = length(percentiles)),
                      "_p", rep(percentiles * 100, length.out = sum(sapply(dtLoam, is.numeric)))))

#Clay
# Compute quantiles in parallel for each numeric column
resultClay <- dtClay[, lapply(.SD, function(col) {
  if (is.numeric(col)) {
    sapply(percentiles, function(p) quantile(col, probs = p, na.rm = TRUE))
  }
}), by = .(experiment, doy)]

# Add column names
setnames(resultClay, old = names(result)[-c(1, 2)], 
         new = paste0(rep(names(dtClay)[sapply(dtClay, is.numeric)], each = length(percentiles)),
                      "_p", rep(percentiles * 100, length.out = sum(sapply(dtClay, is.numeric)))))


