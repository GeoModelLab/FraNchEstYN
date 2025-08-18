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
