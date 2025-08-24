


jobs <- list(
  list(weather_data = weather,
       management_data = mgmt,
       reference_data = ref,
       cropParameters = cropParameters$wheat,
       diseaseParameters = diseaseParameters$septoria,
       start_end = c(2000,2000),
       calibration = "none"),
  list(weather_data = weather,
       management_data = mgmt,
       reference_data = ref,
       cropParameters = cropParameters$wheat,
       diseaseParameters = diseaseParameters$septoria,
       start_end = c(2000,2000),
       calibration = "none")
)

res_batch <- franchestyn_batch(jobs, workers = 2)
length(res_batch)




cat("Current working directory:", getwd(), "\n")

# --- Clean environment ----
rm(list = ls())

# --- Libraries ----
suppressPackageStartupMessages({
  library(FraNchEstYN)
  library(data.table)
  library(dplyr)
})

# --- Arguments from batch ----
args <- commandArgs(trailingOnly = TRUE)

startIdx <- ifelse(length(args) >= 1, as.numeric(args[1]), 1)
endIdx   <- ifelse(length(args) >= 2, as.numeric(args[2]), 5)

cat("▶ Running batch test from job", startIdx, "to", endIdx, "\n")

# --- Fake weather dataset (just one site) ----
set.seed(42)
thisWeather <- data.frame(
  site = "TestSite",
  year = rep(2000, 10),
  month = rep(1, 10),
  day = 1:10,
  Lat = 45,
  Lon = 9,
  Tmin = runif(10, 5, 10),
  Tmax = runif(10, 15, 25),
  rain = runif(10, 0, 5)
)

# --- Management and reference ----
thisManagement <- data.frame(
  site = "TestSite",
  crop = "wheat",
  variety = "Generic",
  sowingDOY = 300,
  year = "All"
)

thisReference <- data.frame(
  site = "TestSite",
  Disease = "septoria",
  DOY = 180,
  year = 2000,
  YieldAttainable = 5000
)

# --- Jobs for franchestyn_batch ----
jobs <- list()
for (i in startIdx:endIdx) {
  jobs[[i]] <- list(
    weather_data      = thisWeather,
    management_data   = thisManagement,
    reference_data    = thisReference,
    cropParameters    = cropParameters$wheat,
    diseaseParameters = diseaseParameters$septoria,
    start_end         = c(2000, 2000),
    calibration       = "none"
  )
}

cat("Total jobs to run:", length(jobs), "\n")

# --- Run in parallel ----
res <- franchestyn_batch(jobs, workers = 2, cleanup_outputs = TRUE)


cat("✅ franchestyn_batch test finished. Results length:", length(res), "\n")
