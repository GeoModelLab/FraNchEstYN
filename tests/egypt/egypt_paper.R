# Remove objects from the Global Environment----
rm(list=ls())

#libraries
library(tidyverse)
library(FraNchEstYN)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load weather data
weather_data<- weather_egypt
#build management data
management_data <- data.frame(
  crop        = c("wheat"),
  variety     = c("Generic"),
  resistance  = c(0),
  sowingDOY   = c(300),
  year        = c("All"),
  stringsAsFactors = FALSE
)

#load reference data and select the most susceptible variety
reference_data<-reference_egypt |>
  filter(variety == 'sids12')

#load default wheat parameters
thisCropParameters<-FraNchEstYN::cropParameters$wheat
#adjust cycle length limit
thisCropParameters$CycleLength$min<-3000
thisCropParameters$CycleLength$max<-4500
thisCropParameters$FloweringStart$max<-60

#load default yellow rust parameters
thisDiseaseParam <- FraNchEstYN::diseaseParameters$yellow_rust
#define sim start and end
start_end<-c(2012,2015)

#crop model calibration
df<-FraNchEstYN::franchestyn(weather_data = weather_data,
                management_data = management_data,
                reference_data = reference_data,
                cropParameters = thisCropParameters,
                diseaseParameters = thisDiseaseParam,
                calibration="crop", #'all', 'crop', 'disease'
                start_end = start_end,
                iterations=333)

df$diagnostics$calibration$plots
parameters<-df$diagnostics$calibration$plots
options(scipen=999)
outputs<-df$outputs$simulation
metrics<-df$diagnostics$metrics

ggplot(outputs,
       aes(x=as.Date(Date,format='%m/%d/%Y'))) +
  geom_area(aes(y=Latent),fill='blue',alpha=.3)+
  geom_area(aes(y=Sporulating),fill='brown',alpha=.3)+
  #geom_col(aes(y=Prec/40))+
  geom_line(aes(y=LightInterception))+
  geom_line(aes(y=Susceptible),col='grey33',size=.5)+
  #geom_line(aes(y=Affected),col='black',size=1)+
  geom_area(aes(y=Dead),fill='blue',alpha=.5)+
  # #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
  # geom_line(aes(y=LightIntHealthy),col='green3',size=1)+
   geom_point(aes(y=LightInterceptionRef))+
  # #geom_line(aes(y=Yield),col='red')+
  geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
   geom_line(aes(y=YieldAttainable/6000),col='gold3')+
  geom_point(aes(y=YieldRef/6000),col='gold3',size=3)+
   geom_line(aes(y=YieldActual/6000),col='red',linetype=2)+
   geom_point(aes(y=YieldActualRef/6000),col='red',size=3)+
  geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6,scales='free_x')+
  theme_bw()

metrics <- df$diagnostics$metrics
summary<-df$outputs$summary



#save calibrated parameters
calibCrop<-df$parameters$crop
#
calibDisease<-FraNchEstYN::diseaseParameters$yellow_rust
calibDisease$AssimilateSappersDamage$calibration<-F
calibDisease$AssimilateSappersDamage$value<-0
calibDisease$RUEreducerDamage$calibration<-F
calibDisease$RUEreducerDamage$value<-0
calibDisease$SenescenceAcceleratorDamage$calibration<-F
calibDisease$OuterInoculumShapeRelease$value<-1
calibDisease$WetnessDurationMinimum$min<-1
calibDisease$LightStealerDamage$min<-0.1
calibDisease$RelativeHumidityCritical$min<-45
calibDisease$HydroThermalTimeOnset$max<-50

dfValid<-FraNchEstYN::franchestyn(weather_data = weather_data,
                     management_data = management_data,
                     reference_data = thisReference_data,
                     cropParameters = calibCrop,
                     diseaseParameters = calibDisease,
                     calibration="disease",
                     start_end = start_end,
                     iterations=999)

parameters<-dfValid$diagnostics$calibration$plots
parameters
outputs<-dfValid$outputs$simulation
ggplot(outputs,
       aes(x=as.Date(Date,format='%m/%d/%Y'))) +
  geom_area(aes(y=Latent),fill='blue',alpha=.3)+
  geom_area(aes(y=Sporulating),fill='brown',alpha=.3)+
  #geom_col(aes(y=Prec/40))+
  geom_line(aes(y=LightInterception))+
  geom_line(aes(y=Susceptible),col='grey33',size=.5)+
  geom_line(aes(y=Affected),col='black',size=1)+
  geom_area(aes(y=Dead),fill='blue',alpha=.5)+
  # #geom_line(aes(y=htTimeS/60),col='red2',size=2)+
   geom_line(aes(y=LightIntHealthy),col='green3',size=1)+
   geom_point(aes(y=LightInterceptionRef))+
  # #geom_line(aes(y=Yield),col='red')+
  geom_line(aes(y=OuterInoculum),col='purple',size=1)+
  geom_line(aes(y=DiseaseSeverity),col='blue',size=1)+
  geom_line(aes(y=YieldAttainable/6000),col='gold3')+
  geom_point(aes(y=YieldRef/6000),col='gold3',size=3)+
  geom_line(aes(y=YieldActual/6000),col='red',linetype=2)+
  geom_point(aes(y=YieldActualRef/6000),col='red',size=3)+
  geom_point(aes(y=as.numeric(DiseaseSeverityRef)),col='blue',shape=21,size=3)+
  facet_wrap(~GrowingSeason,ncol=6,scales='free_x')+
  theme_bw()

saveRDS(calibCrop,file='egyptian_crop_params.rds')
saveRDS(dfValid$parameters$disease,file='egyptian_disease_params.rds')

calibCrop<-readRDS('egyptian_crop_params.rds')
calibDisease<-readRDS('egyptian_disease_params.rds')

# esempio d'uso
calibDisease <- FraNchEstYN::disable_all_calibration(calibDisease)
calibCrop <- FraNchEstYN::disable_all_calibration(calibCrop)

# contenitori
all_outputs   <- list()
all_params    <- list()
fail_log      <- character(0)

varieties<-unique(reference_data$variety)
par_df_dis<-parameters_to_df(calibDisease)
par_df_crop<-parameters_to_df(calibCrop)

calibCrop<-enable_calibration(calibCrop,
                              keys=c('RadiationUseEfficiency',
                                     'VarietalResistance'))
calibCrop_df<-parameters_to_df(calibCrop)

calibDisease_df<-parameters_to_df(calibDisease)

var <- varieties[[1]]
for (var in varieties) {
    message(sprintf("▶️ variety: %s", var))
    thisReference_data <- reference_data %>% filter(variety == var)

    res <- tryCatch(
      FraNchEstYN::franchestyn(
        weather_data      = weather_data,
        management_data   = management_data,
        reference_data    = thisReference_data,
        cropParameters    = calibCrop,
        diseaseParameters = calibDisease,
        calibration       = "all",
        start_end         = start_end,
        iterations        = 999
      ),
      error = function(e) {
        fail_log <<- c(fail_log, sprintf("%s: %s", var, e$message))
        return(NULL)
      }
    )

    if (is.null(res)) next

    # --- estrazione sicura ---
    out_df <- tryCatch(
      res$outputs$simulation %>% mutate(Variety = var),
      error = function(e) NULL
    )

    par_df <- tryCatch(
      res$diagnostics$calibration$plots %>% mutate(Variety = var),
      error = function(e) NULL
    )

    if (!is.null(out_df)) all_outputs[[var]] <- out_df
    if (!is.null(par_df)) all_params[[var]]  <- par_df
 }

  # bind finali (se liste vuote -> tibble con 0 righe)
  outputs_df <- if (length(all_outputs)) bind_rows(all_outputs) else tibble()
  params_df  <- if (length(all_params))  bind_rows(all_params)  else tibble()

  # opzionale: salva su disco
  # readr::write_csv(outputs_df, "outputs_by_variety.csv")
  # readr::write_csv(params_df,  "parameters_by_variety.csv")

  # opzionale: mostra eventuali fallimenti
  if (length(fail_log)) {
  warning("Alcune variety non hanno completato:\n - ", paste(fail_log, collapse = "\n - "))
}

  library(ggplot2)
  library(scales)

  ggplot(outputs_df, aes(x = as.Date(Date, format = "%m/%d/%Y"))) +
    # --- disease/canopy states ---
    geom_area(aes(y = Dead, fill = "Dead"), alpha = .5) +
    geom_area(aes(y = Sporulating, fill = "Sporulating"), alpha = .3) +
    geom_area(aes(y = Latent, fill = "Latent"), alpha = .8) +

    geom_line(aes(y = LightInterception, color = "Light interception"), size = 0.8) +
    geom_line(aes(y = Susceptible, color = "Susceptible canopy"), size = 0.5, linetype = 2) +
    geom_line(aes(y = LightIntHealthy, color = "Healthy light interception"), size = 1) +
    geom_point(aes(y = LightInterceptionRef, color = "Measured light interception"), shape = 4) +

    geom_line(aes(y = DiseaseSeverity, color = "Disease severity"), size = 1) +
    geom_point(aes(y = as.numeric(DiseaseSeverityRef), color = "Measured disease severity"),
               shape = 21, size = 2) +

    # --- yield on secondary axis ---
    geom_line(aes(y = YieldAttainable/6000, color = "Attainable yield (simulated)"), size = 1) +
    geom_point(aes(y = YieldRef/6000, color = "Measured attainable yield"),
               shape = 21, fill = "grey54", size = 2.5) +
    geom_line(aes(y = YieldActual/6000, color = "Actual yield (simulated)"), size = .6) +
    geom_point(aes(y = YieldActualRef/6000, color = "Measured actual yield"), size = 2.5) +

    facet_wrap(GrowingSeason ~ Variety, scales = "free_x", ncol = 7) +

    # --- axis labels and dual y-axis ---
    scale_y_continuous(
      name = "Disease severity / Canopy states (proportion)",
      sec.axis = sec_axis(~.*6000, name = "Yield (kg/ha)")
    ) +
    scale_x_date(date_breaks = "30 days", date_labels = "%b %d") +

    # --- legend mappings with explicit labels ---
    scale_fill_manual(
      name = "Canopy states",
      values = c("Dead" = "red4", "Sporulating" = "orange", "Latent" = "pink4")
    ) +
    scale_color_manual(
      name = "Variables",
      values = c(
        "Light interception"          = "black",
        "Susceptible canopy"          = "black",
        "Healthy light interception"  = "green4",
        "Measured light interception" = "green4",
        "Disease severity"            = "blue",
        "Measured disease severity"   = "blue",
        "Attainable yield (simulated)"= "grey53",
        "Measured attainable yield"   = "black",
        "Actual yield (simulated)"    = "black",
        "Measured actual yield"       = "black"
      )
    ) +

    labs(x = "Date", fill = "Canopy states", color = "Variables") +

    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "lines"),
      legend.spacing.x = unit(0.3, "cm"),
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.box.margin = margin(t = -5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      strip.text = element_text(face = "bold")
    ) +
    guides(
      fill = guide_legend(ncol = 3),
      color = guide_legend(ncol = 3)
    )


