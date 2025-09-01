setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(FraNchEstYN)
library(tidyverse)

# to be replaced with the data released with the package
weather_data<-read.csv("ITPO0QXX.csv")
weather_data$site <- 'Policoro'

# no management data needed
management_data <- NULL

# to be replaced with the data released with the package
cropModel_data <-  read.csv("ITPO0QXX_ssm.csv")
reference_data<-NULL

cropParameters<-NULL
thisDiseaseParam <- FraNchEstYN::diseaseParameters$brown_rust

start_end = c(1980,1985)

# source("..//..//R//Main.R")

df<-franchestyn(weather_data = weather_data,
                cropModel_data = cropModel_data,
                diseaseParameters = thisDiseaseParam,
                start_end = start_end,
                calibration = "none")

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
  geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  facet_wrap(~GrowingSeason,ncol=6,scales='free_x')+
  theme_bw()+
  xlab('Date')

#change some disease parameters

thisDiseaseParam<-diseaseParameters$brown_rust
thisDiseaseParam$OuterInoculumMax$value<-0.08
thisDiseaseParam$OuterInoculumShapeRelease$value<-1
#change some damage mechanisms
thisDiseaseParam$RUEreducerDamage$value<-0.1
thisDiseaseParam$LightStealerDamage$value<-0.5
thisDiseaseParam$SenescenceAcceleratorDamage$value<-0.1
thisDiseaseParam$AssimilateSappersDamage$value<-0

dfValid<-franchestyn(weather_data = weather_data,
                     cropModel_data = cropModel_data,
                     management_data = management_data,
                     reference_data = reference_data,
                     #cropParameters = FraNchEstYN::cropParameters$wheat,
                     diseaseParameters = thisDiseaseParam,
                     #fungicideParameters = thisFungicide,
                     calibration="none",
                     start_end = start_end,
                     iterations=300,
                     apikey = "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",
                     franchy_message = F,
                     personality = 'farmer')

outputs<-dfValid$outputs$simulation
ggplot(outputs |> filter(GrowingSeason==1982), aes(x=as.Date(Date,format='%m/%d/%Y'))) +
  #geom_area(aes(y=HTtimeRinoculum),fill='yellow4',size=.2,alpha=.3)+
 # geom_area(aes(y=HTtimeSinoculum),fill='yellow4',size=.2,alpha=.3)+
    geom_area(aes(y=Latent),fill='gold',alpha=.7)+
    geom_area(aes(y=Sporulating),fill='orange',alpha=.2)+
    geom_line(aes(y=Susceptible),col='green4',size=.5)+
   geom_line(aes(y=Affected),col='red',size=1)+
    geom_line(aes(y=Dead),fill='red4',alpha=.5)+
   geom_line(aes(y=LightInterception),col='red',size=1)+
   geom_line(aes(y=LightIntHealthy),col='black',size=1)+
   geom_line(aes(y=YieldActual/8000),col='black',size=1)+
    geom_line(aes(y=YieldAttainable/8000),col='red',size=1)+
    geom_line(aes(y=AGBactual/10000),col='black',size=1)+
    geom_line(aes(y=AGBattainable/10000),col='red',size=1)+
    geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  # geom_line(aes(y=FungicideEfficacy),col='blue',size=1)+
  # geom_line(aes(y=OuterInoculum),col='blue',size=1)+
  facet_wrap(~GrowingSeason,ncol=3,scales='free_x')+
  theme_bw()+
  xlab('Date')






# add fungicide scheduling
management_data <- data.frame(
  site      = rep("Policoro"),
  crop      = rep("wheat"),
  variety = rep("Generic"),
  year      = 1981,
  #treatment = "",
  # treatment_2 = 70,
  # treatment_3 = 80,
  # treatment_4 = 90,
  # treatment_5 = 100,
  # treatment_6 = 110,
  # treatment_7 = 125,
  stringsAsFactors = FALSE
)



# add the message for
