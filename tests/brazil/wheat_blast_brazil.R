library(tidyverse)
library(FraNchEstYN)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

epamig_data<-read.csv('..//..//data-raw//data_epamig.csv') |>
  group_by(year,sowing_date,cultivar,planting_period) |>
  summarise(head = mean(heading_days),
            harv= mean(harvest_date),
            dhs=mean(dhs),
            yield=mean(yield)) |>
  filter(cultivar=='BRILHANTE')

#   weather_data <- nasapower::get_power(
#     community     = "ag",
#     lonlat        = c(-46.4403, -18.5178),
#     dates         = c(paste0(2011, '-01-01'),
#                       paste0(2020, '-12-31')),
#     temporal_api  = "daily",
#     pars          = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR")
#   ) %>%
#     rename(TMAX = T2M_MAX, TMIN = T2M_MIN,
#            RAIN = PRECTOTCORR, DATE = YYYYMMDD) %>%
#     mutate(
#       year  = year(DATE),
#       month = month(DATE),
#       day   = day(DATE),                        # extraterrestrial radiation
#       Site = 'Sertaozinho'
#     ) %>%
#     select(Site, TMAX, TMIN, RAIN, year, month, day)
#
# write.csv(weather_data, file = "brazil//weather//SertaozinhoDaily.csv")

weather_data<-read.csv('weather//SertaozinhoDaily.csv') |>
  mutate(lat =-18.5178,
         RAIN = RAIN)

management_data<-epamig_data |>
  ungroup() |>
  mutate(crop = 'wheat',
         sowingDOY = yday(ymd_hms(sowing_date))) |>
  rename(variety=cultivar) |>
  select(crop,sowingDOY,year,planting_period)

reference_data<-epamig_data |>
  ungroup() |>
  rename(yieldActual = yield,
         variety = cultivar,
         Disease=dhs) |>
  mutate(site="Sertaozinho") |>
  mutate(doy=yday(ymd_hms(sowing_date))+harv,
         fint=.1)

reference_data_aug <- reference_data %>%
  bind_rows(
    reference_data %>%
      mutate(
        doy         = yday(ymd_hms(sowing_date)) + head,
        fint        = 1,
        Disease     = NA_real_,
        yieldActual = NA_real_
      )
  ) %>%
  arrange(site, variety, year, doy) |>
  select(-c(sowing_date,variety,head,harv))

#saveRDS(df$parameters$crop,'brazil_wheat_parameters.rds')

thisCropParameters<-readRDS('..//..//data-raw//brazil_wheat_parameters.rds')
thisCropParameters    <- FraNchEstYN::disable_all_calibration(thisCropParameters)
thisCropParameters$CycleLength$calibration<-T
thisCropParameters$CycleLength$min<-1500
thisCropParameters$CycleLength$max<-2500
thisCropParameters$RadiationUseEfficiency$calibration<-T
thisCropParameters$FloweringStart$calibration<-T

start_end<-c(2011,2020)
diseaseParameters
thisDiseaseParam<-diseaseParameters$wheat_blast
thisDiseaseParam$AssimilateSappersDamage$calibration<-F
thisDiseaseParam$AssimilateSappersDamage$value<-0
thisDiseaseParam$SenescenceAcceleratorDamage$calibration<-F
thisDiseaseParam$SenescenceAcceleratorDamage$value<-0
thisDiseaseParam$SenescenceAcceleratorDamage$max<-0.5
thisDiseaseParam$CyclePercentageOnset$min<-20
thisDiseaseParam$OuterInoculumShapeRelease$value<-1
thisDiseaseParam$LightStealerDamage$value<-0
thisDiseaseParam$LightStealerDamage$calibration<-F
thisDiseaseParam$AssimilateSappersDamage$calibration<-T
thisDiseaseParam$OuterInoculumShapeParameter$value<-0
thisDiseaseParam$IsSplashBorne$value<-0


pps <- unique(management_data$planting_period)
#for calibration, the others for validation
pps<-c(1,5,10)

all_parameters_disease <- list()
all_parameters_crop <- list()

for (pp in pps) {
  df<-FraNchEstYN::franchestyn(weather_data = weather_data,
                             management_data = management_data|>
                               filter(planting_period==pp),
                             reference_data = reference_data_aug |>
                               filter(planting_period==pp),
                             cropParameters = thisCropParameters,
                             diseaseParameters = thisDiseaseParam,
                             calibration="all", #'all', 'crop', 'disease'
                             start_end = start_end,
                             iterations=999)

  plot_df<-df$outputs$simulation

  # Flatten a "list of lists" or accept a data.frame as-is
  crop_df<- parameters_to_df(df$parameters$crop)
  disease_df <- parameters_to_df(df$parameters$disease)

  out_df <- disease_df %>%
    mutate(planting_period = pp)

  out_crop_df <- crop_df %>%
    mutate(planting_period = pp)

  # NOTE: index with all_parameters, not all_outputs
  all_parameters_disease[[length(all_parameters_disease) + 1]] <- out_df
  all_parameters_crop[[length(all_parameters_crop) + 1]] <- out_crop_df
}

parameters_df <- dplyr::bind_rows(all_parameters_disease, .id = "run_id")
parameters_df_crop <- dplyr::bind_rows(all_parameters_crop, .id = "run_id")

options(scipen = 999)
# 1) summary stats for all parameters
param_summary <- parameters_df %>%
  group_by(Parameter) %>%
  summarise(
    value = mean(as.numeric(Value), na.rm = TRUE),
    sd_value   = sd(as.numeric(Value),   na.rm = TRUE),
    n          = sum(!is.na(Value)),
    .groups    = "drop"
  )
param_summary_crop <- parameters_df_crop %>%
  group_by(Parameter) %>%
  summarise(
    value = mean(as.numeric(Value), na.rm = TRUE),
    sd_value   = sd(as.numeric(Value),   na.rm = TRUE),
    n          = sum(!is.na(Value)),
    .groups    = "drop"
  )

calibCrop<-FraNchEstYN::df_to_parameters(param_summary_crop,range_pct=0.2)

# Rebuild a list-of-lists with $value = mean using the custom function
calibDisease<-FraNchEstYN::df_to_parameters(param_summary,range_pct=0.2)

# 1. disable all calibration first
calibDisease <- FraNchEstYN::disable_all_calibration(calibDisease)

#adjust the parameters of the crop and disease model
thisCropParameters<-FraNchEstYN::disable_all_calibration(calibCrop)
# thisCropParameters$HalfIntSenescence$value<-75
 # thisCropParameters$RadiationUseEfficiency$value<-2
 # calibDisease$LightStealerDamage$value<-0
write_rds(thisCropParameters,'..//..//data-raw//brazil_wheat_parameters.rds')
write_rds(calibDisease, "brazil_disease_parameters.rds")

#thisCropParameters<-readRDS("..//..//data-raw//brazil_wheat_parameters.rds")
#calibDisease<-readRDS("..//brazil//brazil_disease_parameters.rds")

# containers for results
all_outputs <- list()

pps <- unique(epamig_data$planting_period)

for (pp in pps) {
  thisPlantingPeriod <- reference_data_aug %>%
    filter(planting_period == pp)

  thisManagementData<- management_data |>
    filter(planting_period == pp)

  message(sprintf("   ▶️ planting period: %s", pp))

  res <- FraNchEstYN::franchestyn(
    weather_data      = weather_data,
    management_data   = thisManagementData,
    reference_data    = thisPlantingPeriod,
    cropParameters    = thisCropParameters,
    diseaseParameters = calibDisease,
    calibration       = "none",
    start_end         = c(2011, 2020),
  )

  # --- extract outputs only if simulation slot exists and has rows ---
  sim_ok <- !is.null(res$outputs$simulation) &&
    is.data.frame(res$outputs$simulation) &&
    nrow(res$outputs$simulation) > 0

  if (sim_ok) {
    out_df <- res$outputs$simulation %>%
      mutate(planting_period = pp)

    all_outputs[[length(all_outputs) + 1]] <- out_df
  } else {
    message("⚠️ No simulation output for this combo.")
  }
}

# --- single combined data frame ---
final_outputs <- dplyr::bind_rows(all_outputs, .id = "run_id")

epamig_data_plot<-read.csv('..//..//data-raw//data_epamig.csv') |>
  filter(cultivar=='BRILHANTE') |>
  group_by(year,sowing_date,cultivar,planting_period) |>
  summarise(DisSev=mean(dhs),
            Yield=mean(yield),
            DisSev_sd = sd(dhs),
            Yield_sd = sd(yield))

final_outputs_joined <- final_outputs |>
  left_join(epamig_data_plot |> filter(cultivar=='BRILHANTE'),
                                         by=c("planting_period" = 'planting_period',
                                            "Year" = 'year'))
cols_to_keep_last <- c("DisSev", "Yield", "DisSev_sd", "Yield_sd")

final_outputs_joined <- final_outputs_joined %>%
  group_by(Year, planting_period) %>%
  # if you need a specific ordering, add arrange(...) here with .by_group = TRUE
  mutate(across(
    any_of(cols_to_keep_last),
    ~ replace(., row_number() != dplyr::n(), NA)
  )) %>%
  ungroup()

# scale factor to align Yield with the primary axis (tune as needed)
yield_scale <- 6000

plot_df <- final_outputs_joined %>%
  mutate(
    planting_period = as.integer(planting_period),
    date = mdy(Date),   # usa ymd(Date) se in formato "yyyy-mm-dd"
    anno_plantingExperiment = paste0(GrowingSeason, "_sow_", planting_period)
  ) %>%
  arrange(GrowingSeason, planting_period) %>%
  mutate(
    anno_plantingExperiment = fct_inorder(anno_plantingExperiment)
  )


ggplot(plot_df |> filter(planting_period%in%c(1,2,3,4,5,6,7,8,9,10,11,12)), aes(x = Doy)) +
  # healthy interception as area
  stat_summary(geom='area',aes(y = LightIntHealthy, fill = "Healthy interception"),
            alpha = 0.3, na.rm = TRUE) +
  # light interception
  geom_line(aes(y = LightInterception, colour = "Light interception"),linewidth = 0.5) +
  # disease severity
  geom_line(aes(y = DiseaseSeverity, colour = "Disease severity"),linewidth = 0.5) +
  geom_point(aes(y = DisSev/100, fill = "Disease severity"),
             shape = 21, size = 2, stroke = 0.2, na.rm = TRUE) +
  # yield
  geom_line(aes(y = YieldActual / yield_scale, colour = "Yield"),
            linewidth = 0.4, na.rm = TRUE) +
  geom_point(aes(y = Yield / yield_scale, colour = "Yield"),
             shape = 22, size = 2, stroke = 0.2, na.rm = TRUE) +
  facet_grid(GrowingSeason~ planting_period,
             scales = "free_x") +
  scale_y_continuous(
    name = "Disease severity / Light interception (0–1)",
    sec.axis = sec_axis(~ . * yield_scale, name = "Yield (kg/ha)")
  ) +
  scale_x_continuous(
    breaks = seq(0, 366, by = 45),
    labels = function(x) month.abb[ceiling(x/45)],
    expand = c(0.1, 0.1)
  ) +
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
  xlab("")


ggplot(plot_df |> filter(planting_period%in%c(1,2,3,4,5,6,7,8,9,10,11,12)), aes(x = Doy)) +
  # healthy interception as area
  stat_summary(geom='area',aes(y = LightIntHealthy, fill = "Healthy interception"),
               alpha = 0.3, na.rm = TRUE) +
  # light interception
  stat_summary(aes(y = LightInterception, colour = "Light interception"),linewidth = 0.5) +
  # disease severity
  stat_summary(geom='line',aes(y = DiseaseSeverity, colour = "Disease severity"),linewidth = 0.5) +
  stat_summary(geom='point',aes(y = DisSev/100, fill = "Disease severity"),
             shape = 21, size = 1, stroke = 0.2, na.rm = TRUE) +
  # yield
  stat_summary(geom='line',aes(y = YieldActual / yield_scale, colour = "Yield"),
            linewidth = 0.4, na.rm = TRUE) +
  stat_summary(geom='point',aes(y = Yield / yield_scale, colour = "Yield"),
             shape = 22, size = 1, stroke = 0.2, na.rm = TRUE) +
  facet_wrap(~ planting_period, ncol=4,
             scales = "free_x") +
  scale_y_continuous(
    name = "Disease severity / Light interception (0–1)",
    sec.axis = sec_axis(~ . * yield_scale, name = "Yield (kg/ha)")
  ) +
  scale_x_continuous(
    breaks = seq(0, 366, by = 45),
    labels = function(x) month.abb[ceiling(x/45)],
    expand = c(0.1, 0.1)
  ) +
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
  xlab("")
