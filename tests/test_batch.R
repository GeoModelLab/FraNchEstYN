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
#remove.packages('FraNchEstYN')
# devtools::document()   # make sure roxygen → Rd is synced
# devtools::clean_dll()
# devtools::build(vignettes = FALSE)


weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\daily\\Indiana.csv'))
#weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\hourly\\Indiana.csv'))
weather_data$site <- 'Indiana'
# weather_data<-weather_data |>
#   mutate(PRECTOTCORR = ifelse(PRECTOTCORR>.5,PRECTOTCORR,0))


management_data <- read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\management\\mgt_indiana.csv'))


reference_data<-read.csv(paste0(getwd(),"\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\reference\\Indiana.csv")) |>
  dplyr::rename(Disease = thisDisease)

#reference_data$yieldActual<-3000

cropParameters<-FraNchEstYN::cropParameters
diseaseParameters <- FraNchEstYN::diseaseParameters

start_year<-1960
end_year<-2022
iterations <- 666



thisCropParam <- cropParameters$wheat

thisDiseaseParam<-diseaseParameters$septoria
start_end = c(1950,2010)

timestep='daily'

#source("R\\Main.R")
apikey  <- ""


#library(FraNchEstYN)

#source("R\\Main.R")
#load default parameters for wheat
thisCropParam<-cropParameters$wheat
# we do not need to calibrate the crop model here, but we have light interception data so we will disable all parameters related to cardinal temperatures and biomass and yield formation and we keep the ones regulating the shape of light interception (canopy dynamics)
thisCropParam<-disable_all_calibration(thisCropParam)
thisCropParam<-set_calibration_flags(thisCropParam,
                                     disable=c('PartitioningMaximum',
                                               'TbaseCrop', 'TmaxCrop', 'ToptCrop',
                                               'FloweringStart',
                                               'RadiationUseEfficiency'))

#load default parameters for septoria
thisDiseaseParam<-diseaseParameters$septoria
# we do not need to simulate yield losses here, just disease severity --> disable all parameters related to damage mechanisms
thisDiseaseParam<-set_calibration_flags(thisDiseaseParam,
                                        disable=c('RUEreducerDamage',
                                                  'LightStealerDamage',
                                                  'SenescenceAcceleratorDamage',
                                                  'AssimilateSappersDamage'))

start_end = c(1971,1992)
api_key = 'xxx' #not used here for LLM
franchy_message = F
iterations = 666 #the devil number!
calibration = 'all' #we calibrate the crop and disease model together

source("R\\franchestyn_batch.R")

franchestyn_batch(weather_data = weather_data,
                management_data = management_data,
                cropParameters = thisCropParam,
                diseaseParameters = thisDiseaseParam,
                start_end = start_end,
                iterations=1,
                outputDir = paste0(getwd(),"//tests//outBatch"))

df$diagnostics$calibration$plots
df$diagnostics$metrics |>
  filter(Variable=='DisSev')
df$parameters$crop
df$parameters$disease

summary<-df$outputs$summary
outputs<-df$outputs$simulation

ggplot(outputs, aes(x=as.Date(Date,format='%m/%d/%Y'))) +
  geom_area(aes(y=HTtimeRinoculum),fill='yellow4',size=.2,alpha=.3)+
  geom_area(aes(y=Latent),fill='gold',alpha=.7)+
  geom_area(aes(y=Sporulating),fill='orange',alpha=.7)+
  geom_line(aes(y=Susceptible),col='green4',size=.5)+
  geom_line(aes(y=Affected),col='red',size=1)+
  geom_line(aes(y=Dead),fill='red4',alpha=.5)+
  geom_line(aes(y=LightInterception),col='black',size=1)+
  geom_line(aes(y=LightIntHealthy),col='green4',size=1)+
  geom_point(aes(y=LightInterceptionRef),size=1)+
  geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6,scales='free_x')+
  theme_bw()+
  xlab('Date')




ggplot(outputs, aes(x=DaysAfterSowing)) +
  geom_area(aes(y=HTtimeRinfection),fill='yellow',size=.2,alpha=.3)+
  geom_area(aes(y=Latent),fill='orange',alpha=.3)+
  geom_area(aes(y=Sporulating),fill='orange3',alpha=.3)+
  geom_line(aes(y=Susceptible),col='green4',size=.5)+
  geom_line(aes(y=Affected),col='red',size=1)+
  geom_line(aes(y=Dead),fill='red4',alpha=.5)+
  geom_line(aes(y=LightIntHealthy),col='green4',size=1)+
  geom_point(aes(y=LightInterceptionRef),size=1)+
  # #geom_line(aes(y=Yield),col='red')+
  geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  # geom_line(aes(y=FungicideEfficacy),col='blue',size=1)+
  # geom_line(aes(y=YieldAttainable),col='red')+
  # geom_line(aes(y=YieldActual),col='red',linetype=2)+
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
