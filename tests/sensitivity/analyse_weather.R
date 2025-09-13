# Remove objects from the Global Environment----
rm(list=ls())

#this directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(tidyverse)
library(FraNchEstYN)

#calibrated parameters
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

sites<-c("FIJO0QXX", "ININ0QXX", "ITPO0QXX", "TRIZ0QXX", "UKRO0QXX", "USMA0QXX")


site_sowing_dates <- c(
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

s<-sites[1]

start_end<-c(1980,2011)

weather_conditions <- list()

for(s in sites){

  thisWeather <- weather_data |>
    filter(site == s)

  thisManagement <- management_data |>
    filter(site == s)

  #assign crop parameters
  paramCrop<-readRDS(paste0(getwd(),"//cropParameters//",s,'_parameters.rds'))

  # Step 3: Perform a run with optimized parameters
  df<-FraNchEstYN::franchestyn(weather_data = thisWeather,
                             management_data = thisManagement,
                             cropParameters = paramCrop,
                             diseaseParameters = diseaseParameters$brown_rust,
                             start_end = start_end,
                             calibration = 'none')

  weather_conditions[[s]]<-df$outputs$summary

}

merged_df <- bind_rows(weather_conditions, .id = "source") |>
  group_by(Site) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") |>
  mutate(RHave = (AveRHx + AveRHn)*.5)


