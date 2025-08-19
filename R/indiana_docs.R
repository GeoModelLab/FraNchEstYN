#' Daily weather for Indiana
#'
#' A dataset containing daily weather observations for Indiana sites,
#' sourced from NASA POWER.
#'
#' @format A data frame with one row per day:
#' \describe{
#'   \item{site}{Site name (here always `"Indiana"`).}
#'   \item{year}{Year (numeric).}
#'   \item{month}{Month (1–12).}
#'   \item{day}{Day of month (1–31).}
#'   \item{tx}{Daily maximum temperature (°C).}
#'   \item{tn}{Daily minimum temperature (°C).}
#'   \item{p}{Daily precipitation (mm).}
#'   \item{rad}{Daily global radiation (MJ m\eqn{^{-2}} d\eqn{^{-1}}).}
#' }
#'
#' @details
#' Data were prepared from FraNchEstYN’s bundled weather files, located in
#' `src_csharp/FraNchEstYN/FraNchEstYN/files/weather/daily/Indiana.csv`.
#' The original source is the NASA POWER project
#' (\url{https://power.larc.nasa.gov/}).
#'
#' @examples
#' data(weather_indiana)
#' dplyr::glimpse(weather_indiana)
"weather_indiana"

#' Indiana reference disease observations
#'
#' Plot- or site-level reference observations from Indiana used for model
#' calibration/validation and retrospective analyses.
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{site}{Character. Site identifier (e.g., station or field name).}
#'   \item{year}{Integer. Calendar year (>= 1972).}
#'   \item{DOY}{Integer. Day of year (1–366) for the observation.}
#'   \item{FINT}{Numeric. Fungal infection index (or other model-specific
#'   infection/intensity metric).}
#'   \item{Disease}{Numeric or integer. Observed disease value (e.g., %
#'   severity or incidence; unit depends on the study protocol).}
#' }
#'
#' @details
#' Filtered from the original source to include only records from 1972 onward.
#' Columns are renamed so that `site` corresponds to `sName` in the raw data.
#' Data were digitized from the original publication by Shaner & Buechley (1995).
#'
#' @source
#' Shaner, G., Buechley, G. (1995). Epidemiology of leaf blotch of soft red
#' winter wheat caused by *Septoria tritici* and *Stagonospora nodorum*.
#' *Plant Disease*, 79(9), 928–938.
#' \doi{10.1094/PD-79-0928}
#' Available at
#' \url{https://www.apsnet.org/publications/plantdisease/backissues/Documents/1995Articles/PlantDisease79n09_928.PDF}
#'
#' @examples
#' data(reference_indiana)
#' head(reference_indiana)
"reference_indiana"

#' Indiana management practices
#'
#' Management metadata used by the FraNchEstYN pipeline for Indiana sites.
#'
#' @format A data frame with one row per site-year combination and the following columns:
#' \describe{
#'   \item{site}{Character. Site identifier (here always `"Indiana"`).}
#'   \item{crop}{Character. Crop grown (here `"wheat"`).}
#'   \item{variety}{Character. Cultivar/variety name (here `"Generic"`).}
#'   \item{year}{Integer. Calendar year of the management record.}
#'   \item{sowingDOY}{Integer. Sowing day of year (1–366).}
#' }
#'
#' @details
#' Read from `files/management/mgt_indiana.csv` and stored in the package via
#' `usethis::use_data(management_indiana, overwrite = TRUE)`.
#' Data were digitized from the original publication by Shaner & Buechley (1995).
#'
#' @source
#' Internal CSV
#' (`src_csharp/FraNchEstYN/FraNchEstYN/files/management/mgt_indiana.csv`).
#' Shaner, G., Buechley, G. (1995). Epidemiology of leaf blotch of soft red
#' winter wheat caused by *Septoria tritici* and *Stagonospora nodorum*.
#' *Plant Disease*, 79(9), 928–938.
#' \doi{10.1094/PD-79-0928}
#' Available at
#' \url{https://www.apsnet.org/publications/plantdisease/backissues/Documents/1995Articles/PlantDisease79n09_928.PDF}
#'
#' @examples
#' data(management_data)
#' head(management_data)
"management_indiana"

