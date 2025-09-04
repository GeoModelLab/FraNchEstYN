library(tidyverse)
library(FraNchEstYN)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# locations<-c("Hossana","Agecha")
# lonlats<-list(c(37.8499966,7.5499978), c(38.4833,7.000))
# for(loc in 1:length(locations))
# {
#   weather_data <- nasapower::get_power(
#     community     = "ag",
#     lonlat        = lonlats[[loc]],
#     dates         = c(paste0(2012, '-01-01'),
#                       paste0(2012, '-12-31')),
#     temporal_api  = "hourly",
#     pars          = c("RH2M", "T2M", "PRECTOTCORR")
#   ) %>%
#     mutate(                # extraterrestrial radiation
#       Site = locations[loc],
#       lat = lonlats[[loc]][2]
#     ) %>%
#     select(Site, T2M, RH2M, PRECTOTCORR, YEAR, MO, DY,HR,lat)
#
#    write.csv(weather_data, file = paste0("weather//",locations[loc],".csv"))
# }


weather_data<-do.call(rbind,lapply(list.files("weather//",full.names = T),read.csv)) |>
  mutate(YEAR = ifelse(Site=='Hossana',YEAR+1,YEAR)) |>
  mutate(site='all') |>
  select(-Site)

reference_data<-read.csv("referenceDataFungicide.csv") |>
  rename(site = sName) |>
  mutate(year = ifelse(site=='Hossana',year+1,year),
         site='all') |>
  filter(variety =='Gambo', fungicide == 'no') |>
  select(site,year,variety,DOY:YieldAttainable)

thisCropParameters<-cropParameters$wheat
thisCropParameters  <- FraNchEstYN::disable_all_calibration(thisCropParameters)
thisCropParameters$TbaseCrop$value<-2
thisCropParameters$ToptCrop$value<-21
thisCropParameters$HalfIntSenescence$value<-90
thisCropParameters$CycleLength$calibration<-T
thisCropParameters$CycleLength$min<-2100
thisCropParameters$CycleLength$max<-2200
thisCropParameters$RadiationUseEfficiency$calibration<-T
thisCropParameters$FloweringStart$calibration<-T
thisCropParameters$FloweringStart$min<-45

start_end<-c(2012,2014)
thisDiseaseParam<-diseaseParameters$septoria
thisDiseaseParam$AssimilateSappersDamage$calibration<-F
thisDiseaseParam$AssimilateSappersDamage$value<-0
thisDiseaseParam$SenescenceAcceleratorDamage$calibration<-T
thisDiseaseParam$SenescenceAcceleratorDamage$max<-0.1
thisDiseaseParam$OuterInoculumMax$min<-0.06
thisDiseaseParam$Topt$min<-20
thisDiseaseParam$OuterInoculumShapeRelease$value<-1
thisDiseaseParam$LightStealerDamage$min<-0
thisDiseaseParam$LightStealerDamage$max<-.3
thisDiseaseParam$HydroThermalTimeOnset$max<-5
thisDiseaseParam$CyclePercentageOnset$max<-10
thisDiseaseParam$CyclePercentageOnset$min<-0
thisDiseaseParam$PathogenSpread$max<-0.3


# Add other columns
management_data <- read.csv("managementData.csv") |>
  mutate(year = "all",sName='all') |>
  rename(site = sName) |>
  slice_head() |>
  select(-c(treatment,fungicide,variety))

sites<-unique(reference_data$site)

weather_data<-weather_data |>
  filter(!(YEAR==2013&MO==2&DY==29))

df<-FraNchEstYN::franchestyn(weather_data = weather_data,
                             management_data = management_data,
                             reference_data = reference_data,
                             cropParameters = thisCropParameters,
                             diseaseParameters = thisDiseaseParam,
                             calibration="all", #'all', 'crop', 'disease'
                             start_end = start_end,
                             iterations=999)

df$diagnostics$calibration$plots
sim<-df$outputs$simulation

yield_scale<-4000

ggplot(sim, aes(x = as.Date(Date,format='%m/%d/%Y'))) +
  # healthy interception as area
  geom_area(aes(y = LightIntHealthy, fill = "Healthy interception"),
            alpha = 0.3, na.rm = TRUE) +
  # light interception
  geom_line(aes(y = LightInterception, colour = "Light interception"),
            linewidth = 0.5, na.rm = TRUE) +
  # disease severity
  geom_line(aes(y = DiseaseSeverity, colour = "Disease severity"),
            linewidth = 0.5, na.rm = TRUE) +
  geom_point(aes(y = DiseaseSeverityRef, fill = "Disease severity"),
             shape = 21, size = 2, stroke = 0.2, na.rm = TRUE) +
  # yield
  geom_line(aes(y = YieldActual / yield_scale, colour = "Yield"),
            linewidth = 0.4, na.rm = TRUE) +
  geom_point(aes(y = YieldActualRef / yield_scale, colour = "Yield"),
             shape = 22, size = 2, stroke = 0.2, na.rm = TRUE) +
  geom_line(aes(y = YieldAttainable / yield_scale, colour = "Yield"),
            linewidth = 0.4, na.rm = TRUE) +
  geom_point(aes(y = YieldRef / yield_scale, colour = "Yield"),
             shape = 22, size = 2, stroke = 0.2, na.rm = TRUE) +
  scale_y_continuous(
    name = "Disease severity / Light interception (0–1)",
    sec.axis = sec_axis(~ . * yield_scale, name = "Yield (kg/ha)")
  ) +
  # scale_x_continuous(
  #   breaks = seq(0, 366, by = 45),
  #   labels = function(x) month.abb[ceiling(x/45)],
  #   expand = c(0.1, 0.1)
  # ) +
  scale_colour_manual(
    name = NULL,
    values = c("Disease severity" = "blue",
               "Yield" = "black",
               "Light interception" = "darkgreen")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Disease severity" = "blue",
               "Yield" = "black",
               "Healthy interception" = "green3")
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey80"),
    strip.placement = "outside",
    strip.text = element_text(size = 10),
    legend.position = "top",
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_text(size = 8),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank()
  ) +
  xlab("")+
  facet_wrap(~GrowingSeason,scales='free_x')

 saveRDS(df$parameters$crop,'calibCrop_fungicide.rds')
 saveRDS(df$parameters$disease,'calibDisease_fungicide.rds')

calibCrop<-readRDS('calibCrop_fungicide.rds')
calibDisease<-readRDS('calibDisease_fungicide.rds')

weather_data<-do.call(rbind,lapply(list.files("weather//",full.names = T),read.csv))

reference_data<-read.csv("referenceDataFungicide.csv") |>
  rename(site = sName)

management_data <- read.csv("managementData.csv")

year<-2012


sites<-unique(weather_data$Site)
varieties<-unique(reference_data$variety)

calibCrop<-FraNchEstYN::disable_all_calibration(calibCrop)
calibDisease<-FraNchEstYN::disable_all_calibration(calibDisease)

outputsSim<-list()

thisVariety<-'Galama'
thisRow<-1

fungicideParam <- FraNchEstYN::disable_all_calibration(FraNchEstYN::fungicideParameters$protectant)
fungicideParam$TenacityFactor$value<-0.05
fungicideParam$DegradationRate$value<-0.02
fungicideParam$InitialEfficacy$value<-1
fungicideParam$AShapeParameter$value<-3
fungicideParam$BShapeParameter$value<-8

fungiParam<-list()
for(thisRow in 1:nrow(management_data)){

  thisScheduling <- management_data[thisRow,]

  for(thisVariety in varieties)
  {
    cat(paste0('site ', thisScheduling$sName, ' variety ', thisVariety))
    if(thisVariety == 'Gambo'){
      calibCrop$VarietalResistance$value <- 0
    }else if(thisVariety == 'Galama'){
      calibCrop$VarietalResistance$value<-.25
    }else if(thisVariety == 'Alidoro'){
      calibCrop$VarietalResistance$value<-.5
    }

    this_weather = weather_data |>
      filter(Site == thisScheduling$sName)

    this_management <- thisScheduling |>
      select(-variety)

    this_reference <- reference_data |>
      filter(site==thisScheduling$sName,variety==thisVariety, fungicide==thisScheduling$fungicide) |>
      select(-variety)

    df<-FraNchEstYN::franchestyn(weather_data = this_weather,
                                 management_data = this_management,
                                 reference_data = this_reference,
                                 cropParameters = calibCrop,
                                 diseaseParameters = calibDisease,
                                 fungicideParameters = fungicideParam,
                                 calibration="none", #'all', 'crop', 'disease'
                                 start_end = start_end,
                                 iterations=999)

    dfOut <- df$outputs$simulation
    dfOut$variety <- thisVariety
    dfOut$fungicide <- thisScheduling$fungicide
    dfOut$Site<-thisScheduling$sName
    outputsSim[[paste0(thisScheduling$sName,"_",thisScheduling$fungicide, "_", thisVariety)]]<-dfOut
  }
}

sim <-  bind_rows(outputsSim, .id = c("site"))

yield_scale<-4000

# Reorder and rename fungicide factor
sim$fungicide <- factor(
  sim$fungicide,
  levels = c("no", "2", "3", "7"),                  # desired order
  labels = c("no treat", "2 treat", "3 treat", "7 treat")  # new names
)

#ggplot(sim, aes(x = as.Date(Date,format='%m/%d/%Y'))) +
ggplot(sim, aes(x = DaysAfterSowing)) +
  # light interception
  geom_line(aes(y = LightIntHealthy, colour = "Light interception (healthy)"),
            alpha = 1, na.rm = TRUE) +
  geom_line(aes(y = LightInterception, colour = "Light interception (diseased)"),
            linewidth = 0.5, linetype = 2, na.rm = TRUE) +
  # disease severity
  geom_line(aes(y = FungicideEfficacy, colour = "Fungicide efficacy"),
            linewidth = 0.4, na.rm = TRUE) +
  geom_area(aes(y = FungicideEfficacy, fill = "Fungicide efficacy"),
            alpha = 0.5, na.rm = TRUE) +
  geom_line(aes(y = DiseaseSeverity, colour = "Disease severity"),
            linewidth = 0.8, na.rm = TRUE) +
  geom_point(aes(y = DiseaseSeverityRef, colour = "Disease severity"),
             shape = 21, size = 2.5, stroke = 0.2, na.rm = TRUE) +
  # yield
  geom_line(aes(y = YieldAttainable / yield_scale, colour = "Yield attainable"),
            linewidth = 0.4, linetype=2,na.rm = TRUE) +
  geom_point(aes(y = YieldRef / yield_scale, colour = "Yield attainable"),
             shape = 22, size = 2.3, stroke = 0.2, na.rm = TRUE) +
  geom_line(aes(y = YieldActual / yield_scale, colour = "Yield actual"),
            linewidth = 0.8, na.rm = TRUE) +
  geom_point(aes(y = YieldActualRef / yield_scale, colour = "Yield actual"),
             shape = 21, size = 2.3, alpha = 0.5, stroke = 0.2, na.rm = TRUE) +

  # axis scales
  scale_y_continuous(
    name = "Disease severity / Light interception (0–1)",
    sec.axis = sec_axis(~ . * yield_scale, name = "Yield (kg/ha)")
  ) +

  # manual legend
  scale_colour_manual(
    name = "Variables",
    values = c(
      "Light interception (healthy)" = "green",
      "Light interception (diseased)" = "black",
      "Fungicide efficacy" = "gold3",
      "Disease severity" = "darkslateblue",
      "Yield attainable" = "red",
      "Yield actual" = "red"
    )
  ) +
  scale_fill_manual(
    name = "Area variables",
    values = c("Fungicide efficacy" = "gold3")
  ) +

  facet_grid(variety ~ Site + fungicide, switch = "y") +  # strip labels on the right

  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey80"),
    strip.placement = "outside",
    strip.text = element_text(size = 10),
    strip.switch.pad.grid = unit(0.1, "lines"),
    legend.position = "top",
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_text(size = 8),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank()
  ) +
  xlab("")

