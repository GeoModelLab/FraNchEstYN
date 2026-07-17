# Remove objects from the Global Environment----
rm(list=ls())

library(FraNchEstYN)
#this directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(tidyverse)


# Function to read a CSV and add site column
read_weather_with_site <- function(file_path) {
  # Extract just the file name without path or extension
  site_name <- tools::file_path_sans_ext(basename(file_path))

  # Read the CSV and add the site column
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  df$site <- site_name

  return(df)
}


combined_outputs<-readRDS('cropCalibration_outputs.rds') |>
  filter(!is.na(YieldRef)) |>
  filter(
    (site == "FIJO0QXX" & DaysAfterSowing < 120) |
      (site == "ININ0QXX" & DaysAfterSowing < 150) |
      (site == "ITPO0QXX" & DaysAfterSowing < 215) |
      (site == "TRIZ0QXX" & DaysAfterSowing < 210) |
      (site == "UKRO0QXX" & DaysAfterSowing < 310) |
      (site == "USMA0QXX" & DaysAfterSowing < 145)
  ) |>
  mutate(
    site = str_remove(site, "0QXX$")   # removes trailing "0QXX"
  )

#plots
library(ggplot2)
library(lemon)
library(hydroGOF)

yield_scale <- 20000  # convert kg/ha to the 0–1 primary axis


# 1) metrics (as before, using nRMSE % + EF)
metrics_by_site <- combined_outputs %>%
  group_by(site) %>%
  summarise(
    RMSE_LI = hydroGOF::rmse(LightInterception,LightInterceptionRef),
    R2_LI = hydroGOF::R2(LightInterception,LightInterceptionRef),
    RMSE_AGB = hydroGOF::rmse(AGBattainable,AGBRef),
    R2_AGB = hydroGOF::R2(AGBattainable,AGBRef),
    RMSE_YLD= hydroGOF::rmse(YieldAttainable,YieldRef),
    R2_YLD = hydroGOF::R2(YieldAttainable,YieldRef)
  )

#install.packages('ggtext')
library(ggtext)

# 2) build a strip label for each site (multi-line; short tags)
strip_labs <- setNames(
  paste0(
    metrics_by_site$site, "<br>",
    "LI&nbsp;RMSE ", round(metrics_by_site$RMSE_LI, 1), " | R² ", round(metrics_by_site$R2_LI,1), "<br>",
    "AGB RMSE ", round(metrics_by_site$RMSE_AGB, 0), " kg ha<sup>-1</sup> | R² ", round(metrics_by_site$R2_AGB, 1), "<br>",
    "Yld RMSE ", round(metrics_by_site$RMSE_YLD, 0), " kg ha<sup>-1</sup> | R² ", round(metrics_by_site$R2_YLD, 1)
  ),
  metrics_by_site$site
)

yield_scale <- 20000

ggplot(combined_outputs, aes(x = DaysAfterSowing)) +
  stat_summary(aes(y = LightInterception,     colour = "Light interception (sim)"),
               fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(aes(y = LightInterceptionRef,  colour = "Light interception (ref)"),
               fun = mean, geom = "line", linewidth = 0.6, linetype = 2) +
  stat_summary(aes(y = AGBattainable / yield_scale, colour = "Biomass (sim)"),
               fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(aes(y = AGBRef        / yield_scale, colour = "Biomass (ref)"),
               fun = mean, geom = "line", linewidth = 0.6, linetype = 2) +
  stat_summary(aes(y = YieldAttainable / yield_scale, colour = "Yield (sim)"),
               fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(aes(y = YieldRef        / yield_scale, colour = "Yield (ref)"),
               fun = mean, geom = "line", linewidth = 0.6, linetype = 2) +
  scale_y_continuous(
    name = "Light interception (0–1)",
    sec.axis = sec_axis(~ . * yield_scale, name = expression("Biomass & yield (kg ha"^{-1}*")"))
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Light interception (sim)" = "darkgreen",
      "Light interception (ref)" = "darkgreen",
      "Biomass (sim)"            = "steelblue",
      "Biomass (ref)"            = "steelblue",
      "Yield (sim)"              = "red3",
      "Yield (ref)"              = "red3"
    ),
    breaks = c(
      "Light interception (sim)", "Light interception (ref)",
      "Biomass (sim)", "Biomass (ref)",
      "Yield (sim)", "Yield (ref)"
    )
  ) +
  lemon::facet_rep_wrap(
    ~ site, scales = "free_x", repeat.tick.labels = TRUE,
    labeller = labeller(site = function(x) strip_labs[x])
  )+
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12),
    strip.text = element_markdown(face = "bold", size = 10, lineheight = 1.0),
    strip.background = element_rect(fill = "grey95", colour = "grey80"),

    # rotate secondary axis label
    axis.title.y.right = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  )+
  labs(x = "Days after sowing", y = NULL)

ggsave('ssm_calibration.png',height = 6, width = 9.5)




#calibrated parameters
paramITPO<-readRDS(paste0("cropparameters/ITPO0QXX_parameters.rds"))
dfITPO<-parameters_to_df(paramITPO)
paramUSMA<-readRDS(paste0("cropparameters/USMA0QXX_parameters.rds"))
dfUSMA<-parameters_to_df(paramUSMA)
paramTRIZ<-readRDS(paste0("cropparameters/TRIZ0QXX_parameters.rds"))
dfTRIZ<-parameters_to_df(paramTRIZ)
paramININ<-readRDS(paste0("cropparameters/ININ0QXX_parameters.rds"))
dfININ<-parameters_to_df(paramININ)
paramFIJO<-readRDS(paste0("cropparameters/FIJO0QXX_parameters.rds"))
dfFIJO<-parameters_to_df(paramFIJO)
paramUKRO<-readRDS(paste0("cropparameters/UKRO0QXX_parameters.rds"))
dfUKRO<-parameters_to_df(paramUKRO)

sites<-c("FIJO0QXX", "ININ0QXX", "ITPO0QXX", "TRIZ0QXX", "UKRO0QXX", "USMA0QXX")

# install.packages(c("dplyr","tibble")) # if needed
library(dplyr)
library(tibble)
library(ggplot2)
library(latex2exp)

df <- data.frame(
  x = factor(c("D_{50}", "I_{\\text{pri}}^{\\text{max}}", "RH_{\\text{crit}}")),
  y = 1:3
)

ggplot(df, aes(x, y)) +
  geom_point(size = 3) +
  scale_x_discrete(labels = function(x) TeX(x))

####save RDS objects outMorris outDfsUncertainty
disSev_set1<-readRDS('out//set_1all_disSev_mean_Experiments.rds') |>
  mutate(set='set_1')
disSev_set2<-readRDS('out//set_2all_disSev_mean_Experiments.rds')|>
  mutate(set='set_2')

disSev_morris<-rbind(disSev_set1,disSev_set2) %>%
  mutate(site_name = sites[experiment],
         site =substr(site_name,1,4))

yieldLoss_set1<-readRDS('out//set_1all_yieldLoss_mean_Experiments.rds') |>
  mutate(set='set_1')
yieldLoss_set2<-readRDS('out//set_2all_yieldLoss_mean_Experiments.rds')|>
  mutate(set='set_2')

yieldLoss_morris<-rbind(yieldLoss_set1,yieldLoss_set2) %>%
  mutate(site_name = sites[experiment],
         site =substr(site_name,1,4))

allIndices<-rbind(disSev_morris |> mutate(variable='DisSev'),
                  yieldLoss_morris |> mutate(variable='YieldLoss'))

library(dplyr)
library(forcats)
library(ggplot2)

# parameters to keep / order
vars <- c("ASSdmg","CYons","D50","HTons","LatD","LSdmg","OutI",
          "PatSp","RUEdmg","R50d","RHcr","RHnl","SENdmg","SpoD",
          "Tmax","Tmin","Topt","WDmin","WDopt")


# ---- Group assignment ----
group_vector <- c(
  "ASSdmg"="Damage mechanism", "LSdmg"="Damage mechanism",
  "RUEdmg"="Damage mechanism", "SENdmg"="Damage mechanism",
  "CYons"="Primary inoculum", "HTons"="Primary inoculum",
  "D50"="Wetness response", "R50d"="Secondary inoculum",
  "LatD"="Epidemic clocks", "SpoD"="Epidemic clocks",
  "OutI"="Primary inoculum", "PatSp"="Secondary inoculum",
  "RHcr"="Humidity response", "RHnl"="Humidity response",
  "Tmin"="Temperature response", "Topt"="Temperature response", "Tmax"="Temperature response",
  "WDmin"="Wetness response", "WDopt"="Wetness response"
)
group_order <- c("Damage mechanism","Primary inoculum","Epidemic clocks",
                 "Secondary inoculum","Humidity response","Temperature response","Wetness response")

# ---- Prepare data ----
df_norm <- allIndices %>%
  group_by(set, site, variable) %>%
  mutate(
    muStar_norm = muStar / sum(muStar, na.rm = TRUE),
    mu_norm     = mu     / sum(abs(mu), na.rm = TRUE),
    sigma_norm  = sigma  / sum(sigma, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(parameter %in% vars) %>%
  mutate(
    # assign groups
    param_group = dplyr::recode(parameter, !!!group_vector) %>%
      forcats::fct_relevel(!!!group_order),
    # now order parameters by their group, then by name
    parameter   = forcats::fct_reorder2(parameter, param_group, as.numeric(param_group))
  )

# build a named expression vector directly
rename_vector_expr <- setNames(
  c(
    expression(D[SAP]),
    expression(C[ons]),
    expression(D[50]),
    expression(HTT[ons]),
    expression(Dur[lat]),
    expression(D[LS]),
    expression(I[pri]^max),
    expression(I[sec]^max),
    expression(D[RUE]),
    expression(R[det]^50),
    expression(RH[crit]),
    expression(RH[NL]),
    expression(D[SEN]),
    expression(Dur[spo]),
    expression(T[max]),
    expression(T[min]),
    expression(T[opt]),
    expression(W[min]),
    expression(W[opt])
  ),
  c("ASSdmg","CYons","D50","HTons","LatD","LSdmg","OutI","PatSp",
    "RUEdmg","R50d","RHcr","RHnl","SENdmg","SpoD","Tmax","Tmin",
    "Topt","WDmin","WDopt")
)

my_colors <- c(
  "Damage mechanism"    = "#E41A1C",  # red
  "Primary inoculum"    = "#377EB8",  # blue
  "Epidemic clocks"     = "#4DAF4A",  # green
  "Secondary inoculum"  = "#984EA3",  # purple
  "Humidity response"   = "#FF7F00",  # orange
  "Temperature response"= "#A65628",  # brown
  "Wetness response"    = "#F781BF"   # pink
)

# ---- Plot ----
ggplot(df_norm |>
         mutate( set  = ifelse(set == "set_1", "rust type", "blast type"),
                 variable = ifelse(variable=='YieldLoss', 'Yield loss (%)', 'Disease severity (fraction)')),
       aes(x = parameter, y = muStar_norm)) +
  stat_summary(geom='col',aes(fill = param_group), outlier.shape = NA) +
  geom_point(aes(shape = site), size = 2, alpha = 0.85) +
  lemon::facet_rep_grid(set ~ variable, scales = "free_y", repeat.tick.labels = TRUE) +
  scale_x_discrete(labels = rename_vector_expr) +
  labs(
    x = "Parameter",
    y = "Normalized μ*",
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x     = element_text(angle = 90, hjust = 1, size = 14,vjust=0.1),
    axis.text.y     = element_text(size = 13,vjust=0),
    legend.box      = "vertical",
    axis.title = element_text(size=13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size=13),
    strip.text = element_markdown(face = "bold", size = 13, lineheight = 1.0),
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    legend.position = "bottom",
    legend.margin   = margin(t = -2, b = 2, unit = "pt"), # tighten top/bottom
    legend.spacing.y = unit(0.1, "cm")                    # less vertical gap
  ) +
  guides(
    shape = guide_legend(title = "Site", nrow = 1, byrow = TRUE),
    fill  = guide_legend(title = "Parameter group", nrow = 2, byrow = TRUE,
                         override.aes = list(shape = NA)) # prevent merging
  )+
  xlab('')+
  scale_fill_manual(values = my_colors)
ggsave('morris_mu.png', height = 7, width =12)

write.csv(df_norm,'df_norm.csv')



reference_list <- list.files(paste0(getwd(),'//referenceData'),full.names = T)
reference_data <- do.call(rbind, lapply(reference_list, read_weather_with_site)) |>
  rename(YieldAttainable = WGRN, AGB = WTOP, Year = Pyear) |>
  mutate(YieldAttainable = YieldAttainable*10,
         AGB = AGB*10) |>
  select(site,Year,DOY,FINT,YieldAttainable,AGB)




ggplot(allIndices |>
         filter(!parameter%in%c('Ishape')) |>
         mutate(facet_strip=paste0(site,"_",set)),aes(x=reorder(parameter,-muStar)))+
  geom_boxplot(aes(y=muStar,fill=set),position='dodge')+
  #coord_flip()+
  lemon::facet_rep_grid(variable~set,scales='free_y',repeat.tick.labels = T)+
  theme_classic()+
  theme(legend.position='none',
        axis.text.x = element_text(angle=90,vjust=0,hjust=1,size=10))+
  xlab('')
ggsave


# Step 1: Find top 7 parameters per site × variable
top7_labels <- allIndices %>%
  mutate(strip_label=paste0(site,"_",set,"_",variable)) |>
  filter(!parameter %in% c("Ishape")) %>%
  group_by(strip_label) %>%
  slice_max(muStar, n = 4, with_ties = FALSE) %>%
  ungroup()

# Step 4: Plot
ggplot(allIndices %>%
         mutate(strip_label=paste0(site,"_",set,"_",variable)) |>
         filter(!parameter %in% c('Ishape'))) +
  geom_point(aes(x = muStar, y = sigma, color = set), size = 0.5) +
  geom_text_repel(data = top7_labels,
                  aes(x = muStar, y = sigma, label = parameter,color=set),
                  size = 3,
                  max.overlaps = 100,
                  box.padding = 0.1,
                  point.padding = 0.1,
                  segment.size = 0.1) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Mean of muStar",
    y = "Mean of sigma",
    title = "FraNchEstYN - Morris sensitivity analysis"
  ) +
  facet_wrap(~ strip_label, scales = 'free',ncol=6)



# Read the nested lists
outDfs_set1 <- readRDS('out//set_1outDfsUncertainty.rds')
outDfs_set2 <- readRDS('out//set_2outDfsUncertainty.rds')

# Function to add 'set' and flatten inner list of dfs with identifiers
flatten_and_tag <- function(nested_list, set_name) {
  # nested_list is a list of lists of data.frames

  # Use map2 to get names of outer list as experiment ids
  map2_dfr(nested_list, names(nested_list), function(inner_list, experiment_name) {
    # inner_list: list of dfs per experiment
    # experiment_name: outer list name

    # For each run/dataframe, add run and experiment info + set
    map2_dfr(inner_list, seq_along(inner_list), function(df, run_number) {
      df %>%
        mutate(set = set_name,
               experiment = experiment_name,
               run = run_number)
    })
  })
}

# Apply to both sets
df_set1 <- flatten_and_tag(outDfs_set1, "set_1")
df_set2 <- flatten_and_tag(outDfs_set2, "set_2")

# Combine into one dataframe
outDfs <- bind_rows(df_set1, df_set2)

#takes 16 secs
outDfs_synth <- outDfs %>%
  group_by(experiment, set, DaysAfterSowing) %>%
  summarise(across(where(is.numeric),
                   list(
                     q25 = ~quantile(., 0.25, na.rm = TRUE),
                     q40 = ~quantile(., 0.40, na.rm = TRUE),
                     median = ~quantile(., 0.50, na.rm = TRUE),
                     q60 = ~quantile(., 0.60, na.rm = TRUE),
                     q75 = ~quantile(., 0.75, na.rm = TRUE)
                   ),
                   .names = "{col}_{fn}"),
            .groups = "drop")



outDfs_synth_plot<-outDfs_synth |>
  mutate(site=substr(experiment,1,4)) |>
  filter((site=='FIJO'&DaysAfterSowing<140)|
           (site=='ININ'&DaysAfterSowing<150)|
           (site=='ITPO'&DaysAfterSowing<210&DaysAfterSowing>70)|
           (site=='TRIZ'&DaysAfterSowing<200&DaysAfterSowing>70)|
           (site=='UKRO'&DaysAfterSowing<310&DaysAfterSowing>180)|
           (site=='USMA'&DaysAfterSowing<150&DaysAfterSowing>55))


# Plot all variables with ribbons for the IQR and lines for the median
library(dplyr)
library(ggplot2)
library(ggnewscale)

p_states <- outDfs_synth %>%
  mutate(
    site = substr(experiment, 1, 4),
    set  = ifelse(set == "set_1", "rust type", "blast type")
  ) %>%
  filter(
    (site == "FIJO" & DaysAfterSowing < 140) |
      (site == "ININ" & DaysAfterSowing < 150) |
      (site == "ITPO" & DaysAfterSowing < 210 & DaysAfterSowing > 70) |
      (site == "TRIZ" & DaysAfterSowing < 200 & DaysAfterSowing > 70) |
      (site == "UKRO" & DaysAfterSowing < 310 & DaysAfterSowing > 180) |
      (site == "USMA" & DaysAfterSowing < 150 & DaysAfterSowing > 55)
  ) %>%
  filter(DaysAfterSowing > 50) %>%
  ggplot(aes(x = DaysAfterSowing)) +

  # Pathogen suitability (area)
  geom_area(
    aes(y = HTtimeRinfection_median, fill = "Pathogen suitability"),
    alpha = 0.20, colour = NA
  ) +
  scale_fill_manual(
    values = c("Pathogen suitability" = "forestgreen"),
    name   = NULL
  ) +
  guides(
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.4))
  ) +

  # Epidemic states (Latent / Sporulating / Dead)
  geom_ribbon(aes(ymin = Latent_q40, ymax = Latent_q60),
              fill = "orange", alpha = 0.30, show.legend = FALSE) +
  geom_ribbon(aes(ymin = Sporulating_q40, ymax = Sporulating_q60),
              fill = "sienna3", alpha = 0.30, show.legend = FALSE) +

  geom_line(aes(y = Latent_median,      colour = "Latent"),      linewidth = 0.8) +
  geom_line(aes(y = Sporulating_median, colour = "Sporulating"), linewidth = 0.8) +
  geom_line(aes(y = Dead_median,        colour = "Dead"),        linewidth = 0.7, alpha = 0.8) +

  scale_colour_manual(
    name   = "Epidemic states",
    values = c(Latent = "orange", Sporulating = "sienna3", Dead = "firebrick4")
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1, byrow = TRUE)
  ) +

  # Severity (separate guide, same line as Pathogen suitability)
  ggnewscale::new_scale_color() +
  geom_ribbon(aes(ymin = DiseaseSeverity_q40, ymax = DiseaseSeverity_q60),
              fill = "darkslateblue", alpha = 0.25, show.legend = FALSE) +
  geom_line(aes(y = DiseaseSeverity_median, colour = "Disease severity"),
            linewidth = 1.0) +
  scale_color_manual(
    values = c("Disease severity" = "darkslateblue"),
    name   = NULL
  ) +
  guides(
    colour = guide_legend(order = 2,
                          override.aes = list(fill = "darkslateblue",
                                              alpha = 1,
                                              size = 1.0))
  ) +

  # Facets, axes, theme
  facet_grid(set ~ site, scales = "free_x") +
  labs(x = "Days after sowing", y = "Proportion (0–1)") +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = expansion(mult = c(0, 0.02))) +
  theme_classic(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.box        = "horizontal",  # put all legends in one row
    legend.text       = element_text(size = 11),
    legend.spacing.x  = unit(0.6, "cm"),
    strip.background  = element_rect(fill = "grey95", colour = "grey80"),
    strip.text        = element_text(size = 11, face = "bold")
  )

p_states


ggsave('diseaseSeverity.png',height=4.5,width=8.4)

# Build reference medians once
site_sowing_dates <- c(FIJO = 127, ININ = 299, ITPO = 323, TRIZ = 320, UKRO = 289, USMA = 360)

ref_data_avg <- reference_data |>
  dplyr::mutate(
    sowing_doy = site_sowing_dates[site],
    DaysAfterSowing = dplyr::if_else(
      site != "FIJO",
      dplyr::if_else(DOY < sowing_doy, DOY + (365 - sowing_doy), DOY - sowing_doy),
      DOY - sowing_doy
    )
  ) |>
  dplyr::group_by(site, DaysAfterSowing) |>
  dplyr::summarise(dplyr::across(where(is.numeric),
                                 ~ stats::quantile(., 0.50, na.rm = TRUE),
                                 .names = "{col}_median"),
                   .groups = "drop")

yield_scale <- 10  # 0–1 -> Mg/ha

synth_metrics<-outDfs_synth |>
  group_by(experiment,set) |>
  slice_tail() |>
  select(YieldAttainable_median,YieldActual_median) |>
  mutate(Yield_loss = (YieldAttainable_median-YieldActual_median)/YieldAttainable_median*100)



p_yield <- outDfs_synth |>
  dplyr::mutate(site = substr(experiment, 1, 4)) |>
  dplyr::left_join(ref_data_avg, by = c("DaysAfterSowing", "site")) |>
  dplyr::filter(
    (site == "FIJO" & DaysAfterSowing < 120) |
      (site == "ININ" & DaysAfterSowing < 150) |
      (site == "ITPO" & DaysAfterSowing < 220) |
      (site == "TRIZ" & DaysAfterSowing < 210) |
      (site == "UKRO" & DaysAfterSowing < 310) |
      (site == "USMA" & DaysAfterSowing < 150)
  ) |>
  dplyr::mutate(stripLabel = paste0(site, "_", set)) |>
  ggplot(aes(x = DaysAfterSowing)) +
  # Healthy interception (area, as elsewhere)
  geom_area(aes(y = LightIntHealthy_median),
            fill = "darkolivegreen3", alpha = 0.5) +
  # Attainable interception (dashed)
  geom_line(aes(y = LightInterception_median),
            colour = "darkgreen", linetype = 2, linewidth = 0.6) +
  # Susceptible fraction (IQR + median)
  geom_ribbon(aes(ymin = Susceptible_q40, ymax = Susceptible_q60),
              fill = "black", alpha = 0.15) +
  geom_line(aes(y = Susceptible_median),
            colour = "black", linewidth = 0.6) +
  # Yield: ribbon IQR + median (scaled to Mg/ha on sec axis)
  geom_ribbon(aes(ymin = YieldActual_q40 / (yield_scale * 1000),
                  ymax = YieldActual_q60 / (yield_scale * 1000)),
              fill = "red3", alpha = 0.25) +
  geom_line(aes(y = YieldActual_median / (yield_scale * 1000)),
            colour = "red3", linewidth = 0.9) +
  # Attainable yield (dashed)
  geom_line(aes(y = YieldAttainable_median.x / (yield_scale * 1000)),
            colour = "grey30", linetype = 2, linewidth = 0.6) +
  facet_grid(set ~ site, scales = "free_x") +
  scale_y_continuous(
    name = "Light interception / susceptible (0–1)",
    sec.axis = sec_axis(~ . * (yield_scale * 1000),
                        name = "Yield (kg/ha)")
  ) +
  labs(x = "Days after sowing") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white", colour = "grey80"),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 1))
p_yield

