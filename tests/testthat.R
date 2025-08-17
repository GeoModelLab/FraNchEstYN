# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(tidyverse)

setwd("C:\\GitHub\\FraNchEstYN")
# remove.packages('FraNchEstYN')
# devtools::document()
# devtools::install()


weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\daily\\Indiana.csv'))
#weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\hourly\\Indiana.csv'))
weather_data$site <- 'Indiana'
# weather_data<-weather_data |>
#   mutate(PRECTOTCORR = ifelse(PRECTOTCORR>.5,PRECTOTCORR,0))


management_data <- read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\management\\sowing.csv'))


reference_data<-read.csv(paste0(getwd(),"\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\reference\\Indiana.csv")) |>
  dplyr::filter(FINT != 0.50 | Septoria>0) |>
  dplyr::rename(Disease = Septoria)

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
thisCropParam$CycleLength$max<-2500
thisCropParam$CycleLength$min <- 1500
thisCropParam$CycleLength$calibration<-T


 thisDiseaseParam<-diseaseParameters$septoria
 thisDiseaseParam$IsSplashBorne$value<-1
 thisDiseaseParam$OuterInoculumShapeRelease$value<-1
 thisDiseaseParam$OuterInoculumMax$value<-.2
 thisDiseaseParam$RelativeHumidityCritical$min<-50
 thisDiseaseParam$RelativeHumidityNotLimiting$min<-80
 thisDiseaseParam$SporulationDuration$max<-35
 thisDiseaseParam$SporulationDuration$min<-15

# thisDiseaseParam$SenescenceAccelerationMax$calibration<-T
# thisDiseaseParam$virtualVisualLesion$calibration<-T
# thisDiseaseParam$virtualVisualLesion$min<-0.5
# thisDiseaseParam$virtualVisualLesion$max<-2
# thisDiseaseParam$SenescenceAccelerationMax$value<-0
# thisDiseaseParam$virtualVisualLesion$min<-.1
# thisDiseaseParam$virtualVisualLesion$max<-2
# thisDiseaseParam$HydroThermalTimeOnset$min<-4
# thisDiseaseParam$HydroThermalTimeOnset$max<-20
# thisDiseaseParam$CyclePercentageOnset$min<-20
# thisDiseaseParam$CyclePercentageOnset$max<-60
# thisDiseaseParam$Topt$min<-15
# thisDiseaseParam$Tmax$min<-30
# thisDiseaseParam$OuterInoculum$max<-0.14
# thisDiseaseParam$OuterInoculum$min<-0.0001
# thisDiseaseParam$Rain50Detachment$calibration<-T


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
#library(FraNchEstYN)

source("R\\Main.R")
apikey  <- ""


thisDiseaseParam$OuterInoculumShapeRelease<-1
thisDiseaseParam$Tmax$min<-27
thisDiseaseParam$Tmax$max<-36
thisDiseaseParam$Rain50Detachment$max<-25
thisDiseaseParam$LightStealerDamage$max<-1.5

thisDiseaseParam$OuterInoculumMax$max<-0.25
thisDiseaseParam$LatencyDuration$min<-5
thisDiseaseParam$CyclePercentageOnset$min<-5
thisDiseaseParam$WetnessDurationMinimum$min<-4
thisDiseaseParam$WetnessDurationOptimum$min<-18

weather_data<-weather_data |>
  dplyr::mutate(lat = 45)

management_data<-management_data |>
  dplyr::select(-site)


management_data$treatment<-list(c('12 Dec', "28 Feb"))

library(FraNchEstYN)

source("R\\Main.R")
df<-franchestyn(weather_data = weather_data,
            management_data = management_data,
            reference_data = reference_data,
            cropParameters = thisCropParam,
            diseaseParameters = thisDiseaseParam,
            fungicideParameters = FraNchEstYN::fungicideParameters,
            calibration="none", #'all', 'crop', 'disease'
            start_end = start_end,
            apikey = "sk-or-v1-",
            franchy_message = T,
            iterations=100)

df$diagnostics$calibration$plots
parameters<-df$diagnostics$calibration$plots
outputs<-df$outputs$simulation

library(ggplot2)
ggplot(outputs,
       aes(x=DaysAfterSowing)) +
  geom_area(aes(y=Latent),fill='blue',alpha=.3)+
  geom_area(aes(y=Sporulating),fill='brown',alpha=.3)+
  #geom_col(aes(y=Prec/40))+
  geom_line(aes(y=LightInterception))+
  geom_line(aes(y=Susceptible),col='grey33',size=.5)+
  #geom_line(aes(y=Affected),col='black',size=1)+
  geom_area(aes(y=Dead),fill='blue',alpha=.5)+
  # #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
   geom_line(aes(y=LightIntHealthy),col='green3',size=1)+
  # geom_point(aes(y=LightInterceptionRef))+
  # #geom_line(aes(y=Yield),col='red')+
   geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  geom_line(aes(y=FungicideEfficacy),col='blue',size=1)+
  # geom_line(aes(y=YieldAttainable/6000),col='red')+
  # geom_line(aes(y=YieldActual/6000),col='red',linetype=2)+
  # geom_point(aes(y=YieldActualRef/6000),col='red',size=3)+
   geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6)+
  theme_bw()+
  xlim(0,300)

  metrics <- df$diagnostics$metrics
summary<-df$outputs$summary



#save calibrated parameters
calibCrop<-df$parameters$crop
# calibCrop$HalfIntSenescence$value<-80
#
calibDisease<-df$parameters$disease
#
#calibDisease$OuterInoculum$value<-.03
 # calibDisease$PathogenSpread$value<-0.01
 # calibDisease$HydroThermalTimeOnset$value<-14
 # calibDisease$CyclePercentageOnset$value<-35
 # calibDisease$SporulationDuration$value<-6
 # calibDisease$LatencyDuration$value<-20
# calibDisease$IsSplashBorne$value<-0
# calibDisease$WetnessDurationMinimum$value<-18
# calibDisease$virtualVisualLesion$value<-0.9

management_data$treatment_1<-105
management_data$treatment_2<-160
management_data$treatment_3<-175
management_data$treatment_4<-190
management_data$treatment_5<-130
management_data$treatment_6<-120
management_data$treatment_7<-145
thisFungicide<-fungicideParameters
thisFungicide$protectant$DegradationRate$value<-0.02
thisFungicide$protectant$AShapeParameter$value<-4
thisFungicide$protectant$BShapeParameter$value<-8.5
thisFungicide$protectant$TenacityFactor$value<-0.02
# management_data<-management_data |>
#   select(-c(treatment_1:treatment_5))
dfValid<-franchestyn(weather_data = weather_data,
                management_data = management_data,
                reference_data = reference_data,
                cropParameters = calibCrop,
                diseaseParameters = calibDisease,
                fungicideParameters = thisFungicide,
                calibration="none",
                disease = "Septoria",
                start_end = start_end,
                iterations=300)

outputs<-dfValid$outputs$simulation
ggplot(outputs |> filter(GrowingSeason==1989),
       aes(x=DaysAfterSowing)) +
  geom_area(aes(y=Latent),fill='blue',alpha=.3)+
  geom_area(aes(y=Sporulating),fill='brown',alpha=.3)+
  #geom_col(aes(y=Prec/40))+
  #geom_line(aes(y=LightInterception))+
  #geom_line(aes(y=Susceptible),col='grey33',size=.5)+
  geom_line(aes(y=Affected),col='black',size=1)+
  geom_area(aes(y=Dead),fill='blue',alpha=.5)+
  #geom_area(aes(y=HTtimeR),fill='gold',alpha=.4)+
  # geom_line(aes(y=LightIntHealthy),col='green3',size=1)+
  # geom_point(aes(y=LightInterceptionRef))+
  # #geom_line(aes(y=Yield),col='red')+
   geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  geom_line(aes(y=FungicideEfficacy),col='gold',size=1)+
  # geom_line(aes(y=YieldAttainable/6000),col='red')+
  # geom_line(aes(y=YieldActual/6000),col='red',linetype=2)+
  # geom_point(aes(y=YieldActualRef/6000),col='red',size=3)+
   geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6)+
  theme_bw()+
  xlim(0,300)

summary<-df$outputs$summary
df$calibration$plots

#
# outputs<-df$outputs$simulation
#
#
# ggplot(outputs,
#        aes(x=DaysAfterSowing)) +
#   stat_summary(geom='area',aes(y=Latent),fill='red',alpha=.3)+
#   stat_summary(geom='area',aes(y=Sporulating),fill='brown',alpha=.3)+
#   stat_summary(geom='line',aes(y=LightInterception))+
#   stat_summary(geom='line',aes(y=Susceptible),col='grey33',size=.5)+
#   stat_summary(geom='line',aes(y=Affected),col='black')+
#   stat_summary(geom='area',aes(y=Dead),col='blue',alpha=3)+
#   #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
#   stat_summary(geom='line',aes(y=LightIntHealthy),col='green3',size=1)+
#   stat_summary(geom='point', aes(y=LightInterceptionRef))+
#   #geom_line(aes(y=yield),col='red')+
#
#   stat_summary(geom='line',aes(y=DisSev),col='blue',size=1)+
#   stat_summary(geom='point', aes(y=as.numeric(DisSevRef)),col='blue',shape=21,size=3)+
#   theme_bw()+
#   xlim(0,320)
