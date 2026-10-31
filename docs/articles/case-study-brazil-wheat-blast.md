# Wheat blast on wheat in Brazil

## 🎯 Goal

This vignette demonstrates how to use **FraNchEstYN** to estimate wheat
yield losses from *wheat head blast* in Brazil.

The workflow follows three progressive steps:

1.  Calibration: fit crop and disease parameters on a subset of planting
    periods (1, 5 and 10).
2.  Parameter averaging: derive robust mean crop and disease parameters
    across planting periods.
3.  Validation: apply the averaged parameters to all planting periods,
    and compare simulated vs observed disease severity and yield.

We start from the Brazil wheat parameter set bundled with the package
(`cropParameters_brazil`), and use open-access EPAMIG experimental data
(dos Santos *et al.*, 2023; 12 sowing dates × 6 years for cultivar
*BRILHANTE*) with daily weather from NASA POWER.

## 📦 Packages

We load FraNchEstYN plus helper libraries for wrangling, visualization,
and reproducible tables.

``` r

library(FraNchEstYN)
library(tidyverse)
library(lubridate)
library(scales)
library(kableExtra)
```

## 📥 Data loading

We use three sources:

- weather_data: daily NASA POWER weather for Empresa de Pesquisa
  Agropecuária de Minas Gerais (EPAMIG) in the municipality of Patos de
  Minas, in the state of Minas Gerais, Brazil (18°31′04.0”S,
  46°26′25.0”W)
- management_data: basic agronomic metadata (crop, sowing DOY, etc.)
- reference_data: measured yields and disease severity for one cultivar
  (BRILHANTE), six years and 12 sowing dates

We rely on [**NASAPOWER**](https://github.com/ropensci/nasapower)
package (maintained by Adam Sparks 🙏) to directly query daily weather
data.

### 🛰 NASA Power data

We request maximum and minimum temperature and precipitation from
2011–2020. Data are tagged with location and date components to simplify
later filtering.

``` r

# --- Load libraries
library(nasapower)
library(dplyr)
library(lubridate)

# --- Retrieve daily data from NASA POWER
weather_data <- nasapower::get_power(
  community     = "ag",
  lonlat        = c(-46.4403, -18.5178),
  dates         = c("2011-01-01", "2020-12-31"),
  temporal_api  = "daily",
  pars          = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR")
) %>%
  rename(
    TMAX = T2M_MAX,
    TMIN = T2M_MIN,
    RAIN = PRECTOTCORR,
    DATE = YYYYMMDD
  ) %>%
  mutate(
    year  = year(DATE),
    month = month(DATE),
    day   = day(DATE),
    Site  = "Sertaozinho",
    lat   = -18.5178
  ) %>%
  select(Site, TMAX, TMIN, RAIN, year, month, day, lat)

head(weather_data)
#> ────────────────────────────────────────────────────────────────────────────────
#> ────────────────────────────────────────────────────────────────────────────────
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 6 × 8
#>   Site         TMAX  TMIN  RAIN  year month   day   lat
#>   <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <int> <dbl>
#> 1 Sertaozinho  24.7  20.0 26.7   2011     1     1 -18.5
#> 2 Sertaozinho  23.6  19.2 22.0   2011     1     2 -18.5
#> 3 Sertaozinho  24.3  18.9 43.0   2011     1     3 -18.5
#> 4 Sertaozinho  24.9  19.0 15.5   2011     1     4 -18.5
#> 5 Sertaozinho  26.2  18.9  9.92  2011     1     5 -18.5
#> 6 Sertaozinho  26.3  18.4 28.0   2011     1     6 -18.5
```

## 📑 Management and reference datasets

For convenience, the package already includes the management and
reference datasets, derived from the open-access EPAMIG wheat trials
(cultivar BRILHANTE, multiple sowing dates and years). The original
experiments are described in [dos Santos *et al.*
(2023)](https://doi.org/10.1007/s10658-022-02614-7).

``` r

# comes with the package
data("reference_brazil")
data("management_brazil")

# a single cultivar (BRILHANTE) is simulated: label it "Generic" so that
# management, reference and crop parameters refer to the same variety
reference_brazil  <- reference_brazil  |> mutate(variety = "Generic")
management_brazil <- management_brazil |> mutate(variety = "Generic",
                                                 site    = "Sertaozinho")
```

A preview of their structure:

| year | variety | planting_period | Disease | yieldActual | site        | doy | fint |
|-----:|:--------|----------------:|--------:|------------:|:------------|----:|-----:|
| 2013 | Generic |               1 |      NA |          NA | Sertaozinho |  92 |    1 |
| 2013 | Generic |               2 |      NA |          NA | Sertaozinho | 100 |    1 |
| 2013 | Generic |               3 |      NA |          NA | Sertaozinho | 107 |    1 |
| 2013 | Generic |               4 |      NA |          NA | Sertaozinho | 116 |    1 |
| 2013 | Generic |               5 |      NA |          NA | Sertaozinho | 124 |    1 |
| 2013 | Generic |               6 |      NA |          NA | Sertaozinho | 131 |    1 |

reference_brazil — structure preview (first 6 rows) {.table .table
.table-striped .table-condensed
style="width: auto !important; margin-left: auto; margin-right: auto;"}

| crop  | variety | sowingDOY | year | planting_period | site        |
|:------|:--------|----------:|-----:|----------------:|:------------|
| wheat | Generic |        49 | 2013 |               1 | Sertaozinho |
| wheat | Generic |        56 | 2013 |               2 | Sertaozinho |
| wheat | Generic |        63 | 2013 |               3 | Sertaozinho |
| wheat | Generic |        70 | 2013 |               4 | Sertaozinho |
| wheat | Generic |        77 | 2013 |               5 | Sertaozinho |
| wheat | Generic |        84 | 2013 |               6 | Sertaozinho |

management_brazil — structure preview (first 6 rows) {.table .table
.table-striped .table-condensed
style="width: auto !important; margin-left: auto; margin-right: auto;"}

## 🦠 Step 1 — Crop and disease calibration on selected sowing dates

As in the published analysis, crop and disease parameters are calibrated
together (`calibration = "all"`) on three sowing dates, and the
remaining sowing dates are kept for validation.

👉 The idea is:

- Select a subset of sowing dates (planting periods 1, 5 and 10, as in
  the article) for calibration.
- For the crop, calibrate only cycle length, flowering start and
  radiation use efficiency.
- For wheat blast, start from the default parameters with the same
  settings used in the article.
- Run franchestyn separately for each planting period.
- Collect the estimated crop and disease parameters from each run.
- Average them across planting periods and rebuild consistent parameter
  sets.

``` r

# --------------------------------------------------------------
# 📌 Step 1: Crop + disease calibration on selected sowing dates
# --------------------------------------------------------------

# 1. Load the Brazil wheat parameter set bundled with the package
data("cropParameters_brazil")

# 2. Crop: calibrate only phenology and radiation use efficiency
thisCropParam <- FraNchEstYN::disable_all_calibration(cropParameters_brazil)
thisCropParam$CycleLength$calibration            <- TRUE
thisCropParam$CycleLength$min                    <- 1500
thisCropParam$CycleLength$max                    <- 2500
thisCropParam$RadiationUseEfficiency$calibration <- TRUE
thisCropParam$FloweringStart$calibration         <- TRUE

# 3. Disease: default wheat blast parameters, with the settings used in the article
thisDiseaseParam <- diseaseParameters$wheat_blast
thisDiseaseParam$SenescenceAcceleratorDamage$calibration <- FALSE
thisDiseaseParam$SenescenceAcceleratorDamage$value       <- 0
thisDiseaseParam$SenescenceAcceleratorDamage$max         <- 0.5
thisDiseaseParam$LightStealerDamage$calibration          <- FALSE
thisDiseaseParam$LightStealerDamage$value                <- 0
thisDiseaseParam$AssimilateSappersDamage$value           <- 0
thisDiseaseParam$AssimilateSappersDamage$calibration     <- TRUE
thisDiseaseParam$CyclePercentageOnset$min                <- 20
thisDiseaseParam$OuterInoculumShapeRelease$value         <- 1
thisDiseaseParam$OuterInoculumShapeParameter$value       <- 0
thisDiseaseParam$IsSplashBorne$value                     <- 0

# 4. Study period and calibration sowing dates
start_end <- c(2011, 2020)
pps <- c(1, 5, 10)

all_parameters_crop    <- list()
all_parameters_disease <- list()

# 5. Calibrate on each planting period
for (pp in pps) {

  df <- FraNchEstYN::franchestyn(
    weather_data      = weather_data,
    management_data   = management_brazil |> filter(planting_period == pp),
    reference_data    = reference_brazil  |> filter(planting_period == pp),
    cropParameters    = thisCropParam,
    diseaseParameters = thisDiseaseParam,
    calibration       = "all",     # crop + disease
    start_end         = start_end,
    iterations        = 999        # simplex iterations
  )

  all_parameters_crop[[length(all_parameters_crop) + 1]] <-
    parameters_to_df(df$parameters$crop) |> mutate(planting_period = pp)
  all_parameters_disease[[length(all_parameters_disease) + 1]] <-
    parameters_to_df(df$parameters$disease) |> mutate(planting_period = pp)
}

# 6. Merge the calibrations from all planting periods
parameters_df_crop <- dplyr::bind_rows(all_parameters_crop,    .id = "run_id")
parameters_df      <- dplyr::bind_rows(all_parameters_disease, .id = "run_id")
```

We then average across planting periods:

``` r

# 7. Mean, standard deviation and sample size across planting periods
summarise_params <- function(x) {
  x %>%
    group_by(Parameter) %>%
    summarise(
      value    = mean(as.numeric(Value), na.rm = TRUE),
      sd_value = sd(as.numeric(Value),   na.rm = TRUE),
      n        = sum(!is.na(Value)),
      .groups  = "drop"
    )
}
param_summary_crop <- summarise_params(parameters_df_crop)
param_summary      <- summarise_params(parameters_df)

# 8. Rebuild parameter lists with the averaged values (±20% range)
#    and switch calibration off for the validation runs
calibCrop    <- FraNchEstYN::df_to_parameters(param_summary_crop, range_pct = 0.2) |>
  FraNchEstYN::disable_all_calibration()
calibDisease <- FraNchEstYN::df_to_parameters(param_summary, range_pct = 0.2) |>
  FraNchEstYN::disable_all_calibration()
```

Snapshot of the averaged disease parameters (`calibDisease`):

|                             | Parameter                   |      Value |
|:----------------------------|:----------------------------|-----------:|
| AssimilateSappersDamage     | AssimilateSappersDamage     |  9.0201876 |
| CyclePercentageOnset        | CyclePercentageOnset        | 26.0343339 |
| DryCriticalInterruption     | DryCriticalInterruption     | 33.6759877 |
| HydroThermalTimeOnset       | HydroThermalTimeOnset       | 15.9242244 |
| IsSplashBorne               | IsSplashBorne               |  0.0000000 |
| LatencyDuration             | LatencyDuration             |  5.9185682 |
| LightStealerDamage          | LightStealerDamage          |  0.0000000 |
| OuterInoculumMax            | OuterInoculumMax            |  0.0541543 |
| OuterInoculumShapeParameter | OuterInoculumShapeParameter |  0.4246513 |
| OuterInoculumShapeRelease   | OuterInoculumShapeRelease   |  1.0000000 |

Snapshot of calibrated disease parameters {.table .table .table-striped
.table-condensed
style="width: auto !important; margin-left: auto; margin-right: auto;"}

At this stage, calibCrop and calibDisease contain the crop and
epidemiological parameters averaged across the calibration sowing dates.
This reduces noise from single calibrations and provides a robust
baseline for validation on the remaining experiments.

## 🌾 Step 2 — Batch validation

We validate the averaged parameters across all sowing periods and years.

The procedure is straightforward:

1.  **Loop across planting periods** recorded in the dataset.\
2.  **Run FraNchEstYN without further calibration**, using the averaged
    crop and disease parameters (`calibCrop`, `calibDisease`).\
3.  **Collect simulation outputs** for each planting period.\
4.  **Bind everything into one tibble** for further analysis and
    plotting.

## 📊 Step 3 — Validation against experiments

In this final step we bring together the model simulations and the field
observations from EPAMIG.\
We join the simulated outputs with the observed **disease severity
(DisSev)** and **yield** per planting period, making sure that only the
last record of each experimental group retains observed values (all
intermediate time steps are set to `NA`).

    #> `summarise()` has grouped output by 'year', 'planting_period'. You can override
    #> using the `.groups` argument.

## 🎨 Visualization

To compare simulations with experiments, we rescale yield onto the same
axis (by dividing by a scaling factor) and facet the plot by year ×
planting experiment.

The final plot shows:

- Healthy light interception (green area)
- Simulated light interception (dark green line)
- Disease severity (blue line and points)
- Grain yield (black line and points, rescaled)

![Simulated and observed wheat blast severity, light interception and
grain yield of cultivar BRILHANTE in Brazil, one panel per year and
sowing
date](case-study-brazil-wheat-blast_files/figure-html/last-plot-1.png)

## 🌍 Interpretation and conclusion

The plot above summarizes model simulations against observed data for
six years and twelve sowing dates of cultivar *BRILHANTE* in Brazil.

- ✅ **Healthy light interception** (green area) provides the baseline
  without disease.\
- ✅ **Simulated interception** (dark green line) captures crop canopy
  dynamics.\
- ✅ **Disease severity** (blue) is both simulated (line) and observed
  (points).\
- ✅ **Yield** (black) shows observed vs simulated grain yield (scaled
  to fit).

Overall, the calibrated disease parameters reproduce the magnitude and
seasonal patterns of wheat blast epidemics. The framework highlights how
integrating **weather, management, and epidemiology** can provide robust
estimates of yield losses.

------------------------------------------------------------------------

## 📌 Key takeaway

This vignette illustrated the **full calibration–validation cycle**:

1.  **Calibrate crop and disease parameters** on a subset of
    experiments.\
2.  **Average** them into a robust epidemiological baseline.\
3.  **Validate** on all experiments across years and sowing dates; the
    nine sowing dates not used for calibration provide the independent
    test.

This workflow can be extended to other crops, pathosystems, and datasets
using the same FraNchEstYN framework.
