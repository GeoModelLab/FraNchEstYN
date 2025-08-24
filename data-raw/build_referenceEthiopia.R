# data-raw/reference_egypt.R
library(usethis)
library(readr)
library(dplyr)

weather_ethiopia<-do.call(rbind,lapply(list.files("tests/fungicide/weather",full.names=T),read.csv)) |>
  select(-X) |>
  select(Site,lat,YEAR,MO,DY,HR,T2M:PRECTOTCORR)

usethis::use_data(weather_ethiopia, overwrite = TRUE)
head(weather_ethiopia)

reference_ethiopia <- read.csv("tests/fungicide/referenceDataFungicide.csv") |>
  select(-FINT) |>
  rename(site = sName)
usethis::use_data(reference_ethiopia, overwrite = TRUE)
head(reference_ethiopia)

management_ethiopia <- read.csv("tests/fungicide/managementData.csv") |>
  rename(site = sName)
usethis::use_data(management_ethiopia, overwrite = TRUE)
head(management_ethiopia)
