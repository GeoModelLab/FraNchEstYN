# Septoria fungicide trials in Ethiopia

## 🎯 Goal

This vignette demonstrates how to use **FraNchEstYN** to simulate yield
losses under different fungicide treatment for *Septoria tritici* in
Ethiopia, using data from fungicide trials on three varieties at two
locations (2012–2013).

The workflow has two main steps:

1.  **Crop and disease calibration**: adjust crop and disease parameters
    using the most susceptible variety.\
2.  **Adapt varietal resistance and fungicide scheduling**: simulate
    alternative fungicide schedules and varietal resistance levels.

Reference disease and yield observations and management metadata were
digitized from the [trial
report](https://core.ac.uk/download/pdf/234661852.pdf) (Said and
Hussien, 2016); hourly weather comes from NASA POWER. All datasets are
released with the package.

## 📦 Packages

We load FraNchEstYN plus helper libraries for wrangling and
visualization.

``` r

library(FraNchEstYN)
library(tidyverse)
library(nasapower)
library(scales)
```

## 📥 Data loading

Hourly weather data were queried from NASA POWER for Agecha and Hossana
sites.

``` r

data(weather_ethiopia)
glimpse(weather_ethiopia)
#> Rows: 17,568
#> Columns: 9
#> $ Site        <chr> "Agecha", "Agecha", "Agecha", "Agecha", "Agecha", "Agecha"…
#> $ lat         <dbl> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
#> $ YEAR        <int> 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012…
#> $ MO          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ DY          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ HR          <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ T2M         <dbl> 9.24, 8.75, 8.26, 7.74, 7.25, 6.70, 6.30, 10.10, 14.68, 19…
#> $ RH2M        <dbl> 63.62, 60.29, 58.47, 58.81, 59.50, 60.31, 60.23, 44.76, 31…
#> $ PRECTOTCORR <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

## 📑 Disease and yield data

Reference observations come from fungicide trials at two sites (Agecha
and Hossana). As in the article, calibration uses the susceptible
cultivar Gambo without fungicide at **both sites**. To pool the two
sites into a single calibration run, the Hossana seasons are shifted
forward by one year so that they do not overlap with the Agecha seasons;
all sites are then labelled `"all"`.

``` r

data(reference_ethiopia)
reference_data <- reference_ethiopia %>%
  mutate(year = ifelse(site == "Hossana", year + 1, year),
         site = "all") %>%
  filter(variety == "Gambo", fungicide == "no") %>%
  select(site, year, variety, DOY:YieldAttainable)
head(reference_data)
#>   site year variety DOY Disease YieldActual YieldAttainable
#> 1  all 2013   Gambo 201    0.00          NA              NA
#> 2  all 2013   Gambo 246    1.13          NA              NA
#> 3  all 2013   Gambo 253    3.66          NA              NA
#> 4  all 2013   Gambo 260    7.37          NA              NA
#> 5  all 2013   Gambo 267   21.23          NA              NA
#> 6  all 2013   Gambo 274   48.11          NA              NA
```

## 🧰 Management data

Basic management metadata (crop, sowing DOY, fungicide schedules) are
included as management_ethiopia. For calibration we use a single
untreated schedule, valid for all years and for the pooled site.

``` r

data(management_ethiopia)
head(management_ethiopia)
#>      site fungicide  crop variety year sowingDOY             treatment
#> 1 Hossana        no wheat     All 2012       201                     0
#> 2  Agecha        no wheat     All 2012       204                     0
#> 3 Hossana         2 wheat     All 2012       201        Sep 23, Oct 23
#> 4  Agecha         2 wheat     All 2012       204          Oct 8, Nov 8
#> 5 Hossana         3 wheat     All 2012       201 Sep 16, Oct 6, Oct 26
#> 6  Agecha         3 wheat     All 2012       204 Oct 7, Oct 27, Nov 16

management_data <- management_ethiopia |>
  mutate(year = "all", site = "all") |>
  slice_head() |>
  select(-c(treatment, fungicide, variety))
```

## 🌾🦠 Step 1 — Crop and disease calibration

We first calibrate crop parameters (cycle length, flowering start and
RUE; base and optimum temperatures are fixed) together with key disease
parameters for Septoria (senescence acceleration, light stealer damage).
Calibration is performed on the untreated Gambo plots of both sites.

``` r

# Crop: fixed cardinal temperatures and senescence; calibrate cycle length,
# radiation use efficiency and flowering start (settings used in the article)
thisCropParameters <- FraNchEstYN::disable_all_calibration(cropParameters$wheat)
thisCropParameters$TbaseCrop$value                    <- 2
thisCropParameters$ToptCrop$value                     <- 21
thisCropParameters$HalfIntSenescence$value            <- 90
thisCropParameters$CycleLength$calibration            <- TRUE
thisCropParameters$CycleLength$min                    <- 2100
thisCropParameters$CycleLength$max                    <- 2200
thisCropParameters$RadiationUseEfficiency$calibration <- TRUE
thisCropParameters$FloweringStart$calibration         <- TRUE
thisCropParameters$FloweringStart$min                 <- 45

# Disease: Septoria defaults with the settings used in the article
thisDiseaseParam <- diseaseParameters$septoria
thisDiseaseParam$AssimilateSappersDamage$calibration     <- FALSE
thisDiseaseParam$AssimilateSappersDamage$value           <- 0
thisDiseaseParam$SenescenceAcceleratorDamage$calibration <- TRUE
thisDiseaseParam$SenescenceAcceleratorDamage$max         <- 0.1
thisDiseaseParam$OuterInoculumMax$min                    <- 0.06
thisDiseaseParam$Topt$min                                <- 20
thisDiseaseParam$OuterInoculumShapeRelease$value         <- 1
thisDiseaseParam$LightStealerDamage$min                  <- 0
thisDiseaseParam$LightStealerDamage$max                  <- 0.3
thisDiseaseParam$HydroThermalTimeOnset$max               <- 5
thisDiseaseParam$CyclePercentageOnset$max                <- 10
thisDiseaseParam$CyclePercentageOnset$min                <- 0
thisDiseaseParam$PathogenSpread$max                      <- 0.3

# Weather: same one-year shift for Hossana, pooled site, no 29 February in 2013
weather_calib <- weather_ethiopia |>
  mutate(YEAR = ifelse(Site == "Hossana", YEAR + 1, YEAR),
         site = "all") |>
  select(-Site) |>
  filter(!(YEAR == 2013 & MO == 2 & DY == 29))

df_calib <- franchestyn(
  weather_data      = weather_calib,
  management_data   = management_data,
  reference_data    = reference_data,
  cropParameters    = thisCropParameters,
  diseaseParameters = thisDiseaseParam,
  calibration       = "all",
  start_end         = c(2012, 2014),
  iterations        = 999
)

sim <- df_calib$outputs$simulation
```

## 📊 Plot after calibration (Gambo, no fungicide, both sites)

![Simulated and observed Septoria severity, light interception and yield
for untreated cultivar Gambo in Ethiopia after calibration, one panel
per
season](case-study-ethiopia-septoria-fungicide_files/figure-html/plot_hossana-1.png)

## 💊 Step 2 — Varietal resistance and fungicide schedules

We now simulate both sites, all varieties (Gambo, Galama, Alidoro) and
fungicide schedules (no, 2, 3, 7) with the calibrated crop and disease
parameters, without further calibration. As in the article, only
varietal resistance differs between cultivars (Gambo = 0, Galama = 0.25,
Alidoro = 0.5), and fungicide parameters are set a priori to the package
defaults for a protectant fungicide.

``` r

#calibrated parameters
# Disable calibration (fix crop + disease params after step 1 calibration)
calibCrop    <- FraNchEstYN::disable_all_calibration(df_calib$parameters$crop)
calibDisease <- FraNchEstYN::disable_all_calibration(df_calib$parameters$disease)

# Initialize empty list to store all simulation outputs
outputsSim <- list()

# Fungicide: protectant; these are the package default values (fungicideParameters$protectant), set a priori as in the article
fungicideParam <- FraNchEstYN::disable_all_calibration(FraNchEstYN::fungicideParameters$protectant)
fungicideParam$TenacityFactor$value  <- 0.05
fungicideParam$DegradationRate$value <- 0.02
fungicideParam$InitialEfficacy$value <- 1
fungicideParam$AShapeParameter$value <- 3
fungicideParam$BShapeParameter$value <- 8

# Varietal resistance per cultivar (0 = susceptible)
varietal_resistance <- c(Gambo = 0, Galama = 0.25, Alidoro = 0.5)

# 🔁 Loop over management schedules (site × fungicide treatment)
for (i in seq_len(nrow(management_ethiopia))) {
  thisScheduling <- management_ethiopia[i, ]

  # 🔁 Loop over varieties (Gambo, Galama, Alidoro)
  for (var in unique(reference_ethiopia$variety)) {

    calibCrop$VarietalResistance$value <- varietal_resistance[[var]]

    # observed disease + yield for this site–variety–fungicide combination
    ref_sub <- reference_ethiopia %>%
      filter(site == thisScheduling$site, variety == var,
             fungicide == thisScheduling$fungicide) %>%
      select(-variety)

    res <- franchestyn(
      weather_data        = weather_ethiopia %>% filter(Site == thisScheduling$site),
      management_data     = thisScheduling %>% select(-variety),
      reference_data      = ref_sub,
      cropParameters      = calibCrop,
      diseaseParameters   = calibDisease,
      fungicideParameters = fungicideParam,
      calibration         = "none",   # no further calibration, just simulate
      start_end           = c(2012, 2014)
    )

    # If simulation produced outputs, store them in the list
    if (!is.null(res$outputs$simulation)) {
      outputsSim[[paste(thisScheduling$site, var, thisScheduling$fungicide, sep = "_")]] <-
        res$outputs$simulation %>%
        mutate(site = thisScheduling$site, variety = var,
               fungicide = thisScheduling$fungicide)
    }
  }
}

# Combine all site–variety–fungicide simulations into one data frame
sim_all <- bind_rows(outputsSim)
```

### 📊 Simulations across sites, varieties and fungicide schedules

![Simulated and observed Septoria severity, fungicide efficacy and yield
in Ethiopia by site, cultivar and fungicide
schedule](case-study-ethiopia-septoria-fungicide_files/figure-html/fungicide_plot-1.png)

## 📝 Key take-home messages

FraNchEstYN successfully reproduced Septoria epidemics and yield losses
observed in Ethiopia fungicide trials.

Calibration on the susceptible variety Gambo (untreated plots, both
sites) provided a baseline that transferred well to the other varieties
and to the fungicide schedules, which were never used in calibration.

Varietal resistance and fungicide scheduling strongly influenced
epidemic progress and yield protection, with more resistant varieties
and intensive spray schedules delaying disease development and
sustaining yields.

The modeling framework captured multiple canopy processes simultaneously
(light interception, disease severity, fungicide efficacy, attainable
and actual yields), allowing a holistic assessment of crop loss.

Outputs are fully structured, enabling further diagnostics, parameter
analysis, and scenario testing.

## 🚀 Next steps

- Apply the same workflow to other seasons or Ethiopian regions, by
  plugging in local weather and reference trial data.
- Test alternative fungicide parameterizations (protectant vs systemic,
  degradation rates, efficacy curves) to assess spray strategies.
- Explore genotype × environment × management interactions, by running
  varietal resistance levels under diverse climates.
- Assess uncertainty by increasing calibration iterations and comparing
  parameter stability across replicates.
- Extend this approach to economic or policy scenarios, estimating yield
  losses avoided through integrated disease management.
