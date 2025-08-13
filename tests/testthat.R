# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(tidyverse)
#
# remove.packages('FraNchEstYN')
# devtools::document()
# devtools::install()
# library(FraNchEstYN)


weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\daily\\Indiana.csv'))
#weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\hourly\\Indiana.csv'))
weather_data$site <- 'Indiana'
# weather_data<-weather_data |>
#   mutate(PRECTOTCORR = ifelse(PRECTOTCORR>.5,PRECTOTCORR,0))


management_data <- read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\management\\sowing.csv')) |>
  dplyr::filter(site == "Indiana") |>
  select(-site)

reference_data<-read.csv(paste0(getwd(),"\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\reference\\Indiana.csv")) |>
  filter(FINT != 0.50 | Septoria>0) |>
  rename(Disease = Septoria)
reference_data$site <- 'Indiana'
reference_data$year <- reference_data$Pyear
#reference_data$yieldActual<-3000

cropParameters<-FraNchEstYN::cropParameters
diseaseParameters <- FraNchEstYN::diseaseParameters

start_year<-1960
end_year<-2022
iterations <- 1



thisCropParam <- cropParameters$wheat
thisCropParam$TbaseCrop$value<-1
thisCropParam$TbaseCrop$calibration<-T
thisCropParam$RadiationUseEfficiency$calibration<-F
thisCropParam$RadiationUseEfficiency$value<-3
thisCropParam$CycleLength$max<-3000
thisCropParam$CycleLength$value <- 3200
thisCropParam$CycleLength$calibration<-F


thisDiseaseParam<-diseaseParameters$septoria
thisDiseaseParam$IsSplashBorne$value<-1
thisDiseaseParam$SenescenceAccelerationMax$calibration<-T
thisDiseaseParam$virtualVisualLesion$calibration<-T
thisDiseaseParam$virtualVisualLesion$min<-0.5
thisDiseaseParam$virtualVisualLesion$max<-2
thisDiseaseParam$SenescenceAccelerationMax$value<-0
thisDiseaseParam$virtualVisualLesion$min<-.1
thisDiseaseParam$virtualVisualLesion$max<-2
thisDiseaseParam$HydroThermalTimeOnset$min<-4
thisDiseaseParam$HydroThermalTimeOnset$max<-20
thisDiseaseParam$CyclePercentageOnset$min<-20
thisDiseaseParam$CyclePercentageOnset$max<-60
thisDiseaseParam$Topt$min<-15
thisDiseaseParam$Tmax$min<-30
thisDiseaseParam$OuterInoculum$max<-0.14
thisDiseaseParam$OuterInoculum$min<-0.0001
thisDiseaseParam$Rain50Detachment$calibration<-T


# library(nasapower)
# hourly_ag <- get_power(
#   community = "ag",
#   lonlat = c(2, 45),
#   pars = c( "T2M","RH2M","PRECTOTCORR"),
#   dates = c("2002-01-01", "2010-12-31"),
#   temporal_api = "hourly"
# )
# write.csv(hourly_ag,'testWeatherHourly.csv')
#
# hourly_ag$datetime <- as.POSIXct(
#   sprintf("%04d-%02d-%02d %02d:00:00", hourly_ag$YEAR, hourly_ag$MO, hourly_ag$DY, hourly_ag$HR),
#   tz = "UTC"
# )
# ggplot(hourly_ag)+geom_line(aes(x=datetime,y=T2M))
#library(FraNchEstYN)
start_end = c(1950,2010)
timestep='daily'
thisMode = 'dsadsa'
calibration="crop"
source("R\\Main.R")

df<-franchestyn(weather_data = weather_data,
            management_data = management_data,
            reference_data = reference_data,
            cropParameters = cropParameters$wheat,
            diseaseParameters = thisDiseaseParam,
            calibration="all",
            start_end = start_end,
            iterations=300)

outputs<-df$outputs$simulation

ggplot(outputs,
       aes(x=DaysAfterSowing)) +
  geom_area(aes(y=Latent),fill='blue',alpha=.3)+
  geom_area(aes(y=Sporulating),fill='brown',alpha=.3)+
  #geom_col(aes(y=Prec/40))+
  geom_line(aes(y=LightInterception))+
  geom_line(aes(y=Susceptible),col='grey33',size=.5)+
  # geom_line(aes(y=Affected),col='black')+
  # #geom_area(aes(y=Dead),col='blue',alpha=3)+
  # #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
   geom_line(aes(y=LightIntHealthy),col='green3',size=1)+
  # geom_point(aes(y=LightInterceptionRef))+
  # #geom_line(aes(y=Yield),col='red')+
   geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  # geom_line(aes(y=YieldAttainable/6000),col='red')+
  # geom_line(aes(y=YieldActual/6000),col='red',linetype=2)+
  # geom_point(aes(y=YieldActualRef/6000),col='red',size=3)+
   geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6)+
  theme_bw()+
  xlim(0,320)

metrics <- df$diagnostics$metrics


#save calibrated parameters
calibCrop<-df$parameters$crop
calibCrop$HalfIntSenescence$value<-80

calibDisease<-df$parameters$disease

calibDisease$OuterInoculum$value<-.05
calibDisease$PathogenSpread$value<-0.01
calibDisease$IsSplashBorne$value<-0
calibDisease$WetnessDurationMinimum$value<-18
calibDisease$virtualVisualLesion$value<-0.9

management_data$resistance<-0

dfValid<-franchestyn(weather_data = weather_data,
                management_data = management_data,
                reference_data = reference_data,
                cropParameters = calibCrop,
                diseaseParameters = calibDisease,
                calibration="none",
                disease = "Septoria",
                start_end = start_end,
                iterations=300)

outputs<-dfValid$outputs$simulation
ggplot(outputs,
       aes(x=DaysAfterSowing)) +
   geom_area(aes(y=Latent),fill='red',alpha=.3)+
   geom_area(aes(y=Sporulating),fill='brown',alpha=.3)+
   geom_line(aes(y=LightInterception))+
  geom_line(aes(y=Susceptible),col='grey33',size=.5)+
  geom_line(aes(y=Affected),col='black')+
  geom_area(aes(y=Dead),col='blue',alpha=3)+
  #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
   geom_line(aes(y=LightIntHealthy),col='green3',size=1)+
     geom_point(aes(y=LightInterceptionRef))+
  #geom_line(aes(y=YieldAttainable/6000),col='red')+
  #geom_line(aes(y=YieldActual/6000),col='red',linetype=2)+
  #geom_point(aes(y=YieldActualRef/6000),col='red',size=3)+
   geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
   geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6)+
  theme_bw()+
  xlim(0,320)

summary<-df$outputs$summary
df$calibration$plots


outputs<-df$outputs$simulation


ggplot(outputs,
       aes(x=DaysAfterSowing)) +
  stat_summary(geom='area',aes(y=Latent),fill='red',alpha=.3)+
  stat_summary(geom='area',aes(y=Sporulating),fill='brown',alpha=.3)+
  stat_summary(geom='line',aes(y=LightInterception))+
  stat_summary(geom='line',aes(y=Susceptible),col='grey33',size=.5)+
  stat_summary(geom='line',aes(y=Affected),col='black')+
  stat_summary(geom='area',aes(y=Dead),col='blue',alpha=3)+
  #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
  stat_summary(geom='line',aes(y=LightIntHealthy),col='green3',size=1)+
  stat_summary(geom='point', aes(y=LightInterceptionRef))+
  #geom_line(aes(y=yield),col='red')+

  stat_summary(geom='line',aes(y=DisSev),col='blue',size=1)+
  stat_summary(geom='point', aes(y=as.numeric(DisSevRef)),col='blue',shape=21,size=3)+
  theme_bw()+
  xlim(0,320)
