#libraries
library(tidyverse)
library(FraNchEstYN)

weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\daily\\Indiana.csv'))
#weather_data<-read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\weather\\hourly\\Indiana.csv'))
weather_data$site <- 'Indiana'

management_data <- read.csv(paste0(getwd(),'\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\management\\mgt_indiana.csv'))

reference_data<-read.csv(paste0(getwd(),"\\src_csharp\\FraNchEstYN\\FraNchEstYN\\files\\reference\\Indiana.csv")) |>
  dplyr::rename(Disease = thisDisease)

# we do not need to calibrate the crop model here, but we have light interception data so we will disable all parameters related to cardinal temperatures and biomass and yield formation and we keep the ones regulating the shape of light interception (canopy dynamics)
thisCropParam <- cropParameters$wheat
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
                                                  'SenescenceAcceleratorDamage',
                                                  'AssimilateSappersDamage'))

start_end = c(1971,1992)
calibration = 'all' #we calibrate the crop and disease model together

df<-franchestyn(weather_data = weather_data,
            management_data = management_data,
            reference_data = reference_data,
            cropParameters = thisCropParam,
            diseaseParameters = thisDiseaseParam,
            start_end = start_end,
            calibration = calibration,
            iterations=333)

df$diagnostics$calibration$plots
metrics<-df$diagnostics$metrics |>
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

#modify a parameter and see the impact
thisCropParam<-df$parameters$crop
thisDiseaseParam<-df$parameters$disease
thisDiseaseParam$PathogenSpread$value<-0.5
thisDiseaseParam$IsSplashBorne$value<-F


df<-franchestyn(weather_data = weather_data,
                management_data = management_data,
                reference_data = reference_data,
                cropParameters = thisCropParam,
                diseaseParameters = thisDiseaseParam,
                start_end = start_end,
                calibration = 'none')

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

