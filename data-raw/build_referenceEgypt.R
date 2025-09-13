# data-raw/reference_egypt.R
library(usethis)
library(readr)
library(dplyr)

reference_egypt <- read_csv("data-raw/reference_egypt.csv") %>%
  mutate(
    site    = "Sharkiya",
    variety = as.character(variety)
  )

usethis::use_data(reference_egypt, overwrite = TRUE)

weather_egypt<-read.csv('tests/egypt/SharkiyaWeatherDaily.csv') |>
  mutate(lat = 30.6150167) |>
  select(-X)
usethis::use_data(weather_egypt, overwrite = TRUE)
head(weather_egypt)
