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
library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

ggplot(outputs %>% filter(GrowingSeason == 1984),
       aes(x = as.Date(Date, format = "%m/%d/%Y"))) +

  # Inoculum pressure
  geom_area(aes(y = HTtimeRinoculum, fill = "Pathogen suitability"),
            alpha = 0.1, color = NA) +

  # Infection stages
  geom_area(aes(y = Latent, fill = "Latent"), alpha = 0.7) +
  geom_area(aes(y = Sporulating, fill = "Sporulating"), alpha = 0.5) +
  geom_area(aes(y = Dead, fill = "Dead"), alpha = 0.3) +

  # Dynamics (scaled 0–1)
  geom_line(aes(y = LightInterception, color = "Attainable Light Interception"),
            , linetype = "dashed",size = .8) +
  geom_line(aes(y = LightIntHealthy, color = "Actual Light Interception"), size = 1.2) +
  geom_line(aes(y = DiseaseSeverity, color = "Disease severity"), size = 1.2) +
  geom_line(aes(y = Susceptible, color = "Susceptible plants"), , linetype = "dotted",size = 1) +
  #geom_line(aes(y = Affected, color = "Affected plants"), size = 1) +

  # Yield & biomass scaled to [0–1] for plotting
  geom_line(aes(y = YieldActual/20000, color = "Actual yield"), size = 1.2) +
  geom_line(aes(y = YieldAttainable/20000, color = "Attainable yield"),
            size = .8, linetype = "dashed") +
  geom_line(aes(y = (AGBactual-2000)/20000, color = "Actual biomass"), size = 1.2) +
  geom_line(aes(y = (AGBattainable-2000)/20000, color = "Attainable biomass"),
            size =.8, linetype = "dashed") +

  # Facet by year
  #facet_wrap(~GrowingSeason, ncol = 3, scales = "free_x") +

  # Color scales
  scale_fill_manual(name = "Host tissue stages",
                    values = c("Pathogen suitability" = "yellow4",
                               "Latent" = "gold",
                               "Sporulating" = "orange",
                               "Dead" = "gold3")) +
  scale_color_manual(name = "",
                     values = c("Disease severity" = "red",
                                "Susceptible plants" = "green4",
                                "Affected plants" = "red",
                                "Actual yield" = "black",
                                "Attainable yield" = "darkgrey",
                                "Actual biomass" = "black",
                                "Attainable biomass" = "darkgrey",
                                "Attainable light int." = "darkgrey",
                                "Actual light int." = "black")) +

  # Axes
  scale_y_continuous(
    name = "Disease dynamics (0–1 scale)",
    limits = c(0, 1),
    sec.axis = sec_axis(~ . * 24000,
                        name = "Yield & Biomass (kg ha⁻¹)")
  ) +
  scale_x_date(
    name = "Date",
    date_breaks = "30 days",
    date_labels = "%b %d",
    limits = as.Date(c("1984-11-25", "1985-06-15"))
  ) +
  # Theme
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 14)
  ) +

  labs(x = "Date",
       title = "Disease Progression, Yield and Biomass")
