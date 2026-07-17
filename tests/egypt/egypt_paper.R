# Remove objects from the Global Environment----
rm(list=ls())

#libraries
library(tidyverse)
library(FraNchEstYN)


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
thisCropParameters$CycleLength$min<-3500
thisCropParameters$CycleLength$max<-4000
thisCropParameters$SlopeGrowth$max<-.005
thisCropParameters$SlopeSenescence$max <- .02

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

calibDisease$Rain50Detachment$calibration<-F
calibDisease$AssimilateSappersDamage$calibration<-F
calibDisease$AssimilateSappersDamage$value<-0
calibDisease$RUEreducerDamage$calibration<-T
calibDisease$RUEreducerDamage$value<-0
calibDisease$RUEreducerDamage$max<-1
calibDisease$DryCriticalInterruption$min<-16
calibDisease$CyclePercentageOnset$min<-15
calibDisease$SenescenceAcceleratorDamage$calibration<-F
calibDisease$OuterInoculumShapeRelease$value<-1
calibDisease$WetnessDurationMinimum$min<-1
calibDisease$WetnessDurationOptimum$min<-15
calibDisease$WetnessDurationOptimum$max<-24
calibDisease$LightStealerDamage$min<-0.1
calibDisease$LightStealerDamage$max<-0.5
calibDisease$RelativeHumidityCritical$min<-45
calibDisease$HydroThermalTimeOnset$max<-50
calibDisease$PathogenSpread$min<-0.35

dfValid<-FraNchEstYN::franchestyn(weather_data = weather_data,
                     management_data = management_data,
                     reference_data = reference_data,
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
all_metrics <- list()
fail_log      <- character(0)

varieties<-unique(reference_egypt$variety)
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
    thisReference_data <- reference_egypt %>% filter(variety == var)

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
      res$diagnostics$calibration$tables$crop %>% mutate(Variety = var),
      error = function(e) NULL
    )

    metrics_df <- tryCatch(
      res$diagnostics$metrics %>% mutate(Variety = var),
      error = function(e) NULL
    )

    if (!is.null(out_df)) all_outputs[[var]] <- out_df
    if (!is.null(par_df)) all_params[[var]]  <- par_df
    if (!is.null(metrics_df)) all_metrics[[var]]  <- metrics_df
 }

  # bind finali (se liste vuote -> tibble con 0 righe)
  outputs_df <- if (length(all_outputs)) bind_rows(all_outputs) else tibble()
  params_df  <- if (length(all_params))  bind_rows(all_params)  else tibble()
  metrics_df <- if (length(all_metrics))  bind_rows(all_metrics)  else tibble()


  library(ggplot2)
  library(scales)

  # Suppose you want to reorder levels manually
  outputs_df$Variety <- factor(outputs_df$Variety,
                               levels = c("sids12", "gem11", "gem7","sids1","sids13",'gem9','gem10'))  # put your desired order here

  outputs_df$facet_strip <- paste0(outputs_df$Variety, "_", outputs_df$GrowingSeason)
  outputs_df$facet_strip <- factor(outputs_df$facet_strip,
                               levels = c("sids12_2013", "gem11_2013", "gem7_2013",
                                          "sids1_2013","sids13_2013",'gem9_2013','gem10_2013',
                                          "sids12_2014", "gem11_2014", "gem7_2014",
                                          "sids1_2014","sids13_2014",'gem9_2014','gem10_2014'))  # put your desired order here



  ggplot(outputs_df, aes(x = as.Date(Date, format = "%m/%d/%Y"))) +
    # --- disease/canopy states ---
    #geom_area(aes(y = Dead, fill = "Dead"), alpha = .5) +
    # geom_area(aes(y = Sporulating, fill = "Sporulating"), alpha = .3) +
    # geom_area(aes(y = Latent, fill = "Latent"), alpha = .8) +

    geom_line(aes(y = LightInterception, color = "Attainable light interception"), size = 0.5,linetype=2) +
    geom_line(aes(y = Susceptible, color = "Susceptible canopy"), size = 0.5, linetype = 1) +
    geom_line(aes(y = LightIntHealthy, color = "Actual light interception"), size = .5) +
    #geom_point(aes(y = LightInterceptionRef, color = "Reference light interception"), shape = 4) +

    geom_line(aes(y = DiseaseSeverity, color = "Disease severity"), size =.5) +
    geom_point(aes(y = as.numeric(DiseaseSeverityRef), color = "Reference disease severity"),
               shape = 21, size = 2) +

    # --- yield on secondary axis ---
    geom_line(aes(y = YieldAttainable/6000, color = "Simulated attainable yield"), size = .5,linetype=2) +
    geom_point(aes(y = YieldRef/6000, color = "Reference attainable yield"),
               shape = 4, size = 2) +
    geom_line(aes(y = YieldActual/6000, color = "Simulated actual yield"), size = .5) +
    geom_point(aes(y = YieldActualRef/6000, color = "Reference actual yield"), size = 2,
               shape = 4) +

    facet_wrap( ~ facet_strip, scales = "free_x", ncol = 7) +

    # --- axis labels and dual y-axis ---
    scale_y_continuous(
      name = "Disease severity",
      sec.axis = sec_axis(
        ~ . * 6000,
        name = expression("Yield (kg ha"^{-1}*")")
      )
    )+

    scale_x_date(date_breaks = "30 days", date_labels = "%b %d") +

    # --- legend mappings with explicit labels ---
    scale_fill_manual(
      name = "Canopy states",
      values = c("Dead" = "red4", "Sporulating" = "orange", "Latent" = "pink4")
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "Attainable light interception" = "green4",
        "Susceptible canopy"          = "black",
        "Actual light interception"  = "green4",
        #"Reference light interception" = "green4",
        "Disease severity"            = "blue",
        "Reference disease severity"   = "blue",
        "Simulated Attainable yield"= "grey53",
        "Reference attainable yield"   = "grey53",
        "Simulated actual yield"    = "red",
        "Reference actual yield"       = "red"
      )
    ) +

    labs(x = "", fill = "Canopy states", color = "Variables") +

    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 1),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.5, "lines"),
      legend.spacing.x = unit(0.3, "cm"),
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.box.margin = margin(t = -5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(face = "bold",size=10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 11),          # left axis title size
      axis.text.y = element_text(size = 10),           # left axis text size
      axis.title.y.right = element_text(size = 11,angle=90),    # right axis title size
      axis.text.y.right = element_text(size = 10),
      strip.text.x = element_text(size=10)# right axis text size
    ) +
    guides(
      fill = guide_legend(ncol = 3),
      color = guide_legend(ncol = 3)
    )
ggsave('tests//egypt//figure2.png',height=5,width=9)

