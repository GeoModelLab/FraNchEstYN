# Yellow rust on wheat in Egypt

## 🎯 Goal

This vignette demonstrates how to use **FraNchEstYN** to estimate wheat
yield losses, using a real dataset on yellow rust in Egypt.

The workflow has **three progressive steps**:

1.  **Crop calibration**: fit basic agronomic parameters using published
    trial data.\
2.  **Disease calibration**: calibrate epidemiological parameters on the
    most susceptible variety (*Sids12*).\
3.  **Varietal extension**: adapt parameters to other varieties,
    adjusting only *Radiation Use Efficiency* and *Varietal Resistance*.

Data sources include digitized observations from [Omara et
al. (2016)](https://doi.org/10.21608/ejp.2016.92381).

## 📦 Packages

We load FraNchEstYN plus helper libraries for wrangling, visualization,
and reproducible tables.

``` r

library(FraNchEstYN)
library(tidyverse)
library(kableExtra)
library(plotly)
library(nasapower)
library(scales)
```

## 📥 Data loading

We use three inputs:

- weather_data: daily weather records for Sharkiya, Egypt (bundled as
  `weather_egypt`)
- management_data: basic agronomic metadata (crop, sowing DOY, etc.)
- reference_data: measured yields and disease severity for several
  cultivars

Alternatively, daily weather can be queried with the
[**nasapower**](https://github.com/ropensci/nasapower) package
(maintained by Adam Sparks 🙏), as shown below.

### 🌦 Weather data

The analysis in the article uses the daily weather bundled with the
package (`weather_egypt`, daily weather records for Sharkiya), which we
load here:

``` r

data(weather_egypt)
weather_data <- weather_egypt
head(weather_data)
#>       Site  TMAX TMIN RAIN year month day      lat
#> 1 Sharkiya 20.49 7.10 0.00 2013     1   1 30.61502
#> 2 Sharkiya 21.39 6.21 0.00 2013     1   2 30.61502
#> 3 Sharkiya 22.32 9.08 0.07 2013     1   3 30.61502
#> 4 Sharkiya 20.83 8.06 0.05 2013     1   4 30.61502
#> 5 Sharkiya 18.65 8.09 0.81 2013     1   5 30.61502
#> 6 Sharkiya 18.66 7.34 0.01 2013     1   6 30.61502
```

The same kind of data can be downloaded directly from NASA POWER. Note
that the precipitation column returned by the API (`PRECTOTCORR`) must
be renamed to `RAIN`, otherwise it is not recognised by the daily
weather reader:

``` r

weather_nasa <- nasapower::get_power(
  community    = "ag",
  lonlat       = c(31.5145167, 30.6150167),
  dates        = c("2012-01-01", "2015-12-31"),
  temporal_api = "daily",
  pars         = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR")) |>
  rename(DATE = YYYYMMDD, RAIN = PRECTOTCORR) |>
  mutate(year = year(DATE), month = month(DATE), day = day(DATE),
         Site = "Sharkiya", lat = 30.6150167) |>
  select(Site, T2M_MAX, T2M_MIN, RAIN, year, month, day, lat)
```

### 📑 Disease and yield data

The dataset reference_egypt is included in the package. It contains
observations of disease severity, final yield, and attainable yield for
several wheat varieties.

``` r

# comes with the package
data(reference_egypt)
```

A preview of its structure:

| site     | variety | year | DOY | disease | fint | yieldActual | yieldAttainable |
|:---------|:--------|-----:|----:|--------:|-----:|------------:|----------------:|
| Sharkiya | gem10   | 2014 |  38 |       2 |   NA |          NA |              NA |
| Sharkiya | gem10   | 2014 |  45 |       5 |   NA |          NA |              NA |
| Sharkiya | gem10   | 2014 |  52 |       5 |   NA |          NA |              NA |
| Sharkiya | gem10   | 2014 |  60 |       5 |   NA |          NA |              NA |
| Sharkiya | gem10   | 2014 |  66 |      10 |   NA |          NA |              NA |
| Sharkiya | gem10   | 2014 |  73 |      10 |   NA |          NA |              NA |

reference_egypt — structure preview (first 6 rows) {.table .table
.table-striped .table-condensed
style="width: auto !important; margin-left: auto; margin-right: auto;"}

### 🧰 Management data

We build a minimal management dataset “on the fly”, assuming a sowing
date of DOY 300 (≈ October 27). This provides the necessary metadata to
drive the crop model.

``` r

management_data <- data.frame(
  crop        = "wheat",
  variety     = "Generic",
  resistance  = 0,        # dimensionless varietal resistance baseline
  sowingDOY   = 300,      # adjust if you know the actual sowing date (DOY)
  year        = "All",
  stringsAsFactors = FALSE
)

kable(management_data, caption = "management_data — minimal setup for calibration") |>
  kableExtra::kable_styling(full_width = FALSE)
```

| crop  | variety | resistance | sowingDOY | year |
|:------|:--------|-----------:|----------:|:-----|
| wheat | Generic |          0 |       300 | All  |

management_data — minimal setup for calibration {.table .table
style="width: auto !important; margin-left: auto; margin-right: auto;"}

## 🌾 Step 1 — Crop calibration

We first calibrate crop parameters only, with disease processes
disabled. This minimal calibration aligns the fraction of intercepted
radiation (FINT) and attainable yield with observations.

➡️ Goal: adjust cycle length (harvest ≈ mid-May) and attainable yield
formation.

``` r

# Select crop parameters for wheat
thisCropParam <- FraNchEstYN::cropParameters$wheat

# Crop parameter bounds for the Egyptian environment (as in the article):
# cycle length to harvest around mid-May (~DOY 135), canopy growth and senescence slopes
thisCropParam$CycleLength$min     <- 3500
thisCropParam$CycleLength$max     <- 4000
thisCropParam$SlopeGrowth$max     <- 0.005
thisCropParam$SlopeSenescence$max <- 0.02

# We select the data from the most susceptible variety
thisReference_egypt<-reference_egypt |> 
  filter(variety=='sids12')

# Calibrate only crop component
df_crop <- franchestyn(
  weather_data      = weather_data,
  management_data   = management_data,
  reference_data    = thisReference_egypt,
  cropParameters    = thisCropParam,
  diseaseParameters = FraNchEstYN::diseaseParameters$yellow_rust,
  calibration       = "crop",
  start_end         = c(2012, 2015),
  iterations        = 333,
  franchy_message   = FALSE
)
```

The resulting chart shows canopy light interception and attainable yield
dynamics compared with measured values.

![Simulated canopy light interception and attainable yield against
observations for cultivar Sids12 in Sharkiya, Egypt, after crop
calibration](case-study-egypt-yellow-rust_files/figure-html/crop-calibration-chart-1.png)

## 🦠 Step 2 — Calibrating disease on the susceptible variety (Sids12)

We now keep the calibrated crop parameters fixed, and calibrate the
epidemiology of yellow rust using Sids12. We want to reproduce the
observed disease progress curves without over-fitting yield damage.

To avoid overparameterization of the damage mechanisms, we:

- Disable assimilate sappers and senescence accelerators.
- Keep active the light stealer (the main pathway for rust damage) and
  RUE reducer mechanisms, as in the article.

This ensures the model matches observed disease severity while keeping
yield linked to light interception.

``` r

# --- use calibrated crop from Step 1 ---
calibCrop <- df_crop$parameters$crop

# --- start from yellow rust defaults ---
calibDisease <- FraNchEstYN::diseaseParameters$yellow_rust

# settings used in the article
# damage mechanisms: assimilate sapping and senescence acceleration off,
# RUE reduction and light stealing calibrated
calibDisease$AssimilateSappersDamage$calibration     <- FALSE
calibDisease$AssimilateSappersDamage$value           <- 0
calibDisease$SenescenceAcceleratorDamage$calibration <- FALSE
calibDisease$RUEreducerDamage$calibration            <- TRUE
calibDisease$RUEreducerDamage$value                  <- 0
calibDisease$RUEreducerDamage$max                    <- 1
calibDisease$LightStealerDamage$min                  <- 0.1
calibDisease$LightStealerDamage$max                  <- 0.5

# epidemiological parameters: fixed values and bounds
calibDisease$Rain50Detachment$calibration  <- FALSE
calibDisease$OuterInoculumShapeRelease$value <- 1
calibDisease$DryCriticalInterruption$min   <- 16
calibDisease$CyclePercentageOnset$min      <- 15
calibDisease$WetnessDurationMinimum$min    <- 1
calibDisease$WetnessDurationOptimum$min    <- 15
calibDisease$WetnessDurationOptimum$max    <- 24
calibDisease$RelativeHumidityCritical$min  <- 45
calibDisease$HydroThermalTimeOnset$max     <- 50
calibDisease$PathogenSpread$min            <- 0.35

# --- run disease calibration ---
dfDisease <- FraNchEstYN::franchestyn(
  weather_data      = weather_data,
  management_data   = management_data,
  reference_data    = thisReference_egypt,
  cropParameters    = calibCrop,
  diseaseParameters = calibDisease,
  calibration       = "disease",
  start_end         = c(2012, 2015),
  iterations        = 999
)
```

The output chart overlays simulated vs observed disease severity, canopy
states, and actual yield.

![Simulated and observed yellow rust severity, canopy light interception
and yield for cultivar Sids12 in Sharkiya, Egypt, after disease
calibration](case-study-egypt-yellow-rust_files/figure-html/disease-calibration-chart-1.png)

## 🌾 Step 3 — Extending to multiple varieties

Finally, we extend calibration to all wheat varieties in the dataset.

👉 In this step:

- Disable all calibration by default.
- Enable only Radiation Use Efficiency (RUE) and Varietal Resistance.
- Run the calibration loop for each variety.

This allows varietal comparisons while keeping the disease model
unchanged.

``` r

#0. take the calibrated disease parameters
calibDisease<-dfDisease$parameters$disease

# 1. disable all calibration first
calibDisease <- FraNchEstYN::disable_all_calibration(calibDisease)
calibCrop    <- FraNchEstYN::disable_all_calibration(calibCrop)

# 2. enable calibration only for RUE and varietal resistance
calibCrop <- enable_calibration(calibCrop,
                                keys = c("RadiationUseEfficiency",
                                         "VarietalResistance"))

# containers for results
all_outputs <- list()

# loop across all varieties in the reference dataset
for (var in unique(reference_egypt$variety)) {
  message(sprintf("▶️ variety: %s", var))
  thisReference_data <- reference_egypt %>% filter(variety == var)

  res <- FraNchEstYN::franchestyn(
    weather_data      = weather_data,
    management_data   = management_data,
    reference_data    = thisReference_data,
    cropParameters    = calibCrop,
    diseaseParameters = calibDisease,
    calibration       = "all",
    start_end         = c(2012, 2015),
    iterations        = 999
  )

  # --- extract outputs only if simulation slot exists ---
  if (!is.null(res$outputs$simulation) && is.data.frame(res$outputs$simulation)) {
    out_df <- res$outputs$simulation %>% mutate(Variety = var)
    all_outputs[[var]] <- out_df
  }
}

# final binding
outputs_df <- if (length(all_outputs)) bind_rows(all_outputs) else tibble()

# bind final results
outputs_df <- bind_rows(all_outputs)
```

We display results in an interactive Plotly chart, enabling zooming,
tooltips, and toggling of variables for clearer interpretation.

![Simulated and observed yellow rust severity and yield for seven wheat
cultivars in Sharkiya, Egypt, by growing
season](case-study-egypt-yellow-rust_files/figure-html/variety-calibration-chart-1.png)

## 📝 Key take-home messages

- FraNchEstYN successfully reproduced yellow rust severity and yield
  losses in Egypt using published trial data.

- Crop calibration (Step 1) focused on cycle length and attainable
  yield, while disease calibration (Step 2) relied on the light stealer
  and RUE reducer mechanisms to match canopy-level impacts.

- Extending calibration to multiple varieties (Step 3) required
  adjusting only Radiation Use Efficiency and Varietal Resistance,
  keeping epidemiology consistent across cultivars.

- The framework outputs diagnostics, parameter estimates, and
  simulations in a structured way, ready for analysis and visualization
  (including interactive Plotly plots).

## 🚀 Next steps

This vignette showed how to calibrate wheat yield losses from yellow
rust in Egypt using bundled datasets. From here, you might want to:

🌎 Apply to other environments: swap in weather and reference datasets
from different sites or years.

🌾 Test other crops or pathogens: explore parameter sets in
cropParameters and diseaseParameters.

🎛️ Experiment with calibration scope:

- `"all"` → crop + disease jointly
- `"crop"` or `"disease"` → focus on one component

📊 Assess uncertainty: run more iterations or replicate calibrations to
check stability of parameter estimates.

🛠️ Extend the workflow: integrate with your own management practices,
local trial data, or yield-loss models for impact assessment.
