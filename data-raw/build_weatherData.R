library(dplyr)
library(usethis)

weather_indiana<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\daily\\Indiana.csv'))
weather_indiana$site <- 'Indiana'
weather_indiana<-weather_indiana |>
  select(site,year,month,day,tx,tn,p,rad)

usethis::use_data(weather_indiana, overwrite = TRUE)

reference_indiana<-read.csv(paste0(getwd(),"\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\reference\\Indiana.csv")) |>
  dplyr::rename(Disease = thisDisease)


reference_indiana<-reference_indiana |>
  rename(site = sName) |>
  filter(year>=1972) |>
  dplyr::select(site,year,DOY,FINT,Disease)

head(reference_indiana)
usethis::use_data(reference_indiana, overwrite = TRUE)



management_indiana <- read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\management\\mgt_indiana.csv'))
usethis::use_data(management_indiana, overwrite = TRUE)

