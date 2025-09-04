# Remove objects from the Global Environment----
rm(list=ls())

#this directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(tidyverse)

combined_outputs<-readRDS('cropCalibration_outputs.rds')

#plots
ggplot(combined_outputs |> filter(GrowingSeason<1985), aes(x=DaysAfterSowing)) +
  stat_summary(geom='line',aes(y=YieldAttainable/20000),fill='red',alpha=.5)+
  stat_summary(aes(y=YieldRef/20000),col='red',alpha=.2)+
  stat_summary(geom='line',aes(y=LightInterception),fill='green2',alpha=.5)+
  stat_summary(aes(y=LightInterceptionRef),col='green2',alpha=.2)+
  stat_summary(geom='line',aes(y=AGBattainable/20000),fill='blue',alpha=.5)+
  stat_summary(aes(y=AGBRef/20000),col='blue',alpha=.2)+
  theme_classic()+
  lemon::facet_rep_grid(site~GrowingSeason,scales='free_x',
                        repeat.tick.labels=T)


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

  ####save RDS objects outMorris outDfsUncertainty
disSev_set1<-readRDS('out//set_1all_disSev_mean_Experiments.rds') |>
  mutate(set='set_1')
disSev_set2<-readRDS('out//set_2all_disSev_mean_Experiments.rds')|>
  mutate(set='set_2')

disSev_morris<-rbind(disSev_set1,disSev_set2) %>%
  mutate(site_name = sites[experiment],
         site =substr(site_name,1,4))

ggplot(disSev_morris |>
         filter(!parameter%in%c('Ishape')) |>
         mutate(facet_strip=paste0(site,"_",set)),aes(x=reorder(parameter,-muStar)))+
  geom_col(aes(y=muStar,fill=set),position='dodge')+
  #coord_flip()+
  lemon::facet_rep_wrap(~facet_strip,scales='free_y',ncol=4,repeat.tick.labels = T)+
  theme_classic()+
  theme(legend.position='none',
        axis.text.x = element_text(angle=90,vjust=0,hjust=1,size=10))+
  xlab('')

reference_list <- list.files(paste0(getwd(),'//referenceData'),full.names = T)
reference_data <- do.call(rbind, lapply(reference_list, read_weather_with_site)) |>
  rename(YieldAttainable = WGRN, AGB = WTOP, Year = Pyear) |>
  mutate(YieldAttainable = YieldAttainable*10,
         AGB = AGB*10) |>
  select(site,Year,DOY,FINT,YieldAttainable,AGB)

yieldLoss_set1<-readRDS('out//set_1all_yieldLoss_mean_Experiments.rds') |>
  mutate(set='set_1')
yieldLoss_set2<-readRDS('out//set_2all_yieldLoss_mean_Experiments.rds')|>
  mutate(set='set_2')

yieldLoss_morris<-rbind(yieldLoss_set1,yieldLoss_set2) %>%
  mutate(site_name = sites[experiment],
         site =substr(site_name,1,4))

ggplot(yieldLoss_morris |>
         filter(!parameter%in%c('Ishape')) |>
         mutate(facet_strip=paste0(site,"_",set)),aes(x=reorder(parameter,-muStar)))+
  geom_col(aes(y=muStar,fill=set),position='dodge')+
  #coord_flip()+
  lemon::facet_rep_wrap(~facet_strip,scales='free_y',ncol=4,repeat.tick.labels = T)+
  theme_classic()+
  theme(legend.position='none',
        axis.text.x = element_text(angle=90,vjust=0,hjust=1,size=10))+
  xlab('')


#TODO: ANALYSIS

#compute mean and sd of the normalized metrics
diSev_mean_synth <- disSev_morris |>
  ungroup() |>
  group_by(parameter) |>
  dplyr::summarise(mu=mean(muStar,na.rm=T),
                   muSD=sd(muStar,na.rm=T),
                   sig=mean(sigma,na.rm=T),
                   sigSD=sd(sigma,na.rm=T))

allIndices<-rbind(disSev_morris |> mutate(variable='DisSev'),
                  yieldLoss_morris |> mutate(variable='YieldLoss'))

library(ggrepel)

# Step 1: Find top 7 parameters per site × variable
top7_labels <- allIndices %>%
  mutate(strip_label=paste0(site,"_",set,"_",variable)) |>
  filter(!parameter %in% c("Ishape")) %>%
  group_by(strip_label) %>%
  slice_max(muStar, n = 7, with_ties = FALSE) %>%
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
ggplot(outDfs_synth_plot |>
         filter(DaysAfterSowing>50) |>
         mutate(stripLabel=paste0(site,"_",set)), aes(x = DaysAfterSowing)) +
  #geom_line(aes(y = LightInterception_median)) +
  geom_area(aes(y = HTtimeRinfection_median),fill='green2',linewidth=.5,alpha=.3) +
  geom_ribbon(aes(ymin = DiseaseSeverity_q40, ymax = DiseaseSeverity_q60), alpha = 0.4,fill='darkslateblue') +
  geom_line(aes(y = DiseaseSeverity_median),col='darkslateblue',linewidth=.9) +
  geom_ribbon(aes(ymin = Latent_q40, ymax = Latent_q60), alpha = 0.7,fill='orange') +
  geom_line(aes(y = Latent_median),col='orange',linewidth=.5) +
  geom_ribbon(aes(ymin = Sporulating_q40, ymax = Sporulating_q60), alpha = 0.7,fill='orange4') +
  geom_line(aes(y = Sporulating_median),col='orange4',linewidth=.5) +
  #geom_ribbon(aes(ymin = Susceptible_q40, ymax = Susceptible_q60), alpha = 0.4,fill='green2') +
  #geom_line(aes(y = Susceptible_median),col='green2',linewidth=.5) +
  geom_line(aes(y=Dead_median),fill='red4',alpha=.3)+

  facet_grid(set~site,scales='free') +
  theme_classic() +
  theme(legend.position = "none")+
  ylab('Disease severity, latent and infectious tissue')+
  xlab('Days after sowing')



site_sowing_dates <- c(
  FIJO = 127,
  ININ = 299,
  ITPO = 323,
  TRIZ = 320,
  UKRO = 289,
  USMA = 360
)


ref_data_avg <- reference_data |>
  mutate(
    sowing_doy = site_sowing_dates[site],
    DaysAfterSowing = if_else(
      site != "FIJO",
      if_else(DOY < sowing_doy, DOY + (365-sowing_doy), DOY - sowing_doy),
      DOY - sowing_doy  # For FIJO, just use DOY - sowingDOY
    )
  ) |>
  ungroup() |>
  group_by(site, DaysAfterSowing) |>
  summarise(across(where(is.numeric),
                   list(median = ~quantile(., 0.50, na.rm = TRUE)),
                   .names = "{col}_{fn}"),
            .groups = "drop") |>
  as.data.frame()



outDfs_yield_plot<-outDfs_synth |>
  mutate(site=substr(experiment,1,4)) |>
  left_join(ref_data_avg,by=c('DaysAfterSowing',"site")) |>
  filter((site=='FIJO'&DaysAfterSowing<120)|
           (site=='ININ'&DaysAfterSowing<150)|
           (site=='ITPO'&DaysAfterSowing<220)|
           (site=='TRIZ'&DaysAfterSowing<210)|
           (site=='UKRO'&DaysAfterSowing<310)|
           (site=='USMA'&DaysAfterSowing<150))



ggplot(outDfs_yield_plot |> mutate(stripLabel=paste0(site,"_",set)) , aes(x = DaysAfterSowing)) +
  #geom_area(aes(y = HTtimeRinfection_median), alpha = .5,fill='blue') +
  geom_line(aes(y = LightInterception_median), col = 'black', size = .5,linetype=3) +

  geom_line(aes(y = Susceptible_median),col='green4',linewidth=.5) +


  #geom_point(aes(y = YieldAttainable_median.y/10000), fill = 'black', size = .3,alpha = .2) +
  geom_area(aes(y = LightIntHealthy_median), fill = 'green', size = .3,alpha = .5) +
  geom_ribbon(aes(ymin = Susceptible_q40, ymax = Susceptible_q60), alpha = .5,fill='green4') +
  #geom_line(aes(y = DiseaseSeverity_median), alpha = .2, fill = 'slateblue') +
  geom_line(aes(y = YieldAttainable_median.x / 10000),linetype=2) +
  geom_ribbon(aes(ymin = YieldActual_q40 / 10000, ymax = YieldActual_q60 / 10000), fill = 'red', alpha = 0.8) +
  # geom_ribbon(aes(ymin = YieldActual_q25 / 10000, ymax = YieldActual_q75 / 10000), fill = 'darkslateblue', alpha = 0.4) +
  geom_line(aes(y = YieldActual_median / 10000), col = 'red3', linewidth = 1) +
  facet_wrap(~ stripLabel, ncol = 4, scales = 'free_x') +
  #geom_ribbon(aes(ymin = HTtimeRinfection_q40, ymax = HTtimeRinfection_q60), alpha = .4,fill='blue') +

  ylim(0, 1)+
  scale_y_continuous(
    name = "Light interception, Susceptible tissue, Pathogen suitability",
    sec.axis = sec_axis(~ . * 10, name = "Yield (Mg/ha)")
  ) +
  theme_classic() +
  theme(legend.position = "none")

