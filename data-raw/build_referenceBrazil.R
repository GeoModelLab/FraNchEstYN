# data-raw/reference_egypt.R
library(usethis)
library(readr)
library(dplyr)

reference_brazil <- read_csv("data-raw/data_epamig.csv") |>
  group_by(year,sowing_date,cultivar,planting_period) |>
  summarise(head = mean(heading_days),
            harv= mean(harvest_date),
            dhs=mean(dhs),
            yield=mean(yield)) |>
  filter(cultivar=='BRILHANTE') |>
  ungroup() |>
  rename(yieldActual = yield,
         variety = cultivar,
         Disease=dhs) |>
  mutate(site="Sertaozinho") |>
  mutate(doy=yday(ymd_hms(sowing_date))+harv,
         fint=.1)

reference_brazil <- reference_data %>%
  bind_rows(
    reference_data %>%
      mutate(
        doy         = yday(ymd_hms(sowing_date)) + head,
        fint        = 1,
        Disease     = NA_real_,
        yieldActual = NA_real_
      )
  ) %>%
  arrange(site, variety, year, doy) |>
  select(-c(sowing_date,head,harv))

usethis::use_data(reference_brazil, overwrite = TRUE)


#management data

epamig_data<-read.csv('data-raw//data_epamig.csv') |>
  group_by(year,sowing_date,cultivar,planting_period) |>
  summarise(head = mean(heading_days),
            harv= mean(harvest_date),
            dhs=mean(dhs),
            yield=mean(yield)) |>
  filter(cultivar=='BRILHANTE')
management_brazil<-epamig_data |>
  ungroup() |>
  mutate(crop = 'wheat',
         sowingDOY = yday(ymd_hms(sowing_date))) |>
  rename(variety=cultivar) |>
  select(crop,variety,sowingDOY,year,planting_period)

usethis::use_data(management_brazil, overwrite = TRUE)


cropParameters_brazil <- readRDS("data-raw/brazil_wheat_parameters.rds")

usethis::use_data(cropParameters_brazil, overwrite = TRUE)
