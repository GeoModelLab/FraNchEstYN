#' Daily weather for Ethiopia fungicide trial sites
#'
#' A dataset containing hourly weather observations for two sites in
#' Ethiopia, sourced from NASA POWER.
#'
#' @format A data frame with one row per hour:
#' \describe{
#'   \item{Site}{Character. Site name (e.g., `"Agecha"`, `"Hossana"`).}
#'   \item{lat}{Numeric. Site latitude (decimal degrees).}
#'   \item{YEAR}{Integer. Calendar year of the observation.}
#'   \item{MO}{Integer. Month (1–12).}
#'   \item{DY}{Integer. Day of month (1–31).}
#'   \item{HR}{Integer. Hour of the day (0–23).}
#'   \item{T2M}{Numeric. Air temperature at 2 m height (°C).}
#'   \item{RH2M}{Numeric. Relative humidity at 2 m height (percent).}
#'   \item{PRECTOTCORR}{Numeric. Precipitation (mm).}
#' }
#'
#' @details
#' Weather data prepared from NASA POWER reanalysis, extracted for Ethiopia
#' fungicide trial locations, and bundled with this package. Raw files were
#' located under `tests/fungicide/weather/`.
#'
#' @source
#' NASA POWER project (\url{https://power.larc.nasa.gov/}).
#' Original trial context documented in
#' \doi{10.22004/ag.econ.234661852} (open-access PDF available at
#' \url{https://core.ac.uk/download/pdf/234661852.pdf}).
#'
#' @examples
#' data(weather_ethiopia)
#' dplyr::glimpse(weather_ethiopia)
"weather_ethiopia"


#' Ethiopia reference disease observations
#'
#' Plot-level reference observations from Ethiopia used for model
#' calibration/validation and retrospective analyses.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{site}{Character. Site identifier (e.g., `"Hossana"`, `"Agecha"`).}
#'   \item{variety}{Character. Wheat variety name (e.g., `"Alidoro"`).}
#'   \item{fungicide}{Factor. Fungicide treatment (`"no"`, `"2"`, `"3"`, `"7"`).}
#'   \item{year}{Integer. Calendar year of observation.}
#'   \item{DOY}{Integer. Day of year (1–366).}
#'   \item{Disease}{Numeric. Observed disease severity (percent).}
#'   \item{YieldActual}{Numeric. Actual measured yield (kg/ha).}
#'   \item{YieldAttainable}{Numeric. Attainable yield (kg/ha) under control, fixed at 6000 kg ha-1.}
#' }
#'
#' @details
#' Data digitized from fungicide trial results in Ethiopia.
#' `YieldActual` and `YieldAttainable` values may be `NA` where not reported.
#'
#' @source
#' Trial data compiled from
#' \url{https://core.ac.uk/download/pdf/234661852.pdf}.
#'
#' @examples
#' data(reference_ethiopia)
#' head(reference_ethiopia)
"reference_ethiopia"


#' Ethiopia management practices
#'
#' Management metadata associated with Ethiopia fungicide trial sites.
#'
#' @format A data frame with one row per site-treatment combination:
#' \describe{
#'   \item{site}{Character. Site identifier.}
#'   \item{fungicide}{Character. Fungicide treatment (`"no"`, `"2"`, `"3"`, `"7"`).}
#'   \item{crop}{Character. Crop grown (here `"wheat"`).}
#'   \item{variety}{Character. Cultivar/variety name (often `"All"`).}
#'   \item{year}{Integer. Calendar year of the record.}
#'   \item{sowingDOY}{Integer. Sowing day of year (1–366).}
#'   \item{treatment}{Character. Application dates for fungicide treatments
#'   (comma-separated, e.g., `"Sep 23, Oct 23"`).}
#' }
#'
#' @details
#' Management data were read from `tests/fungicide/managementData.csv`
#' and stored in the package via `usethis::use_data()`.
#'
#' @source
#' Data digitized from the Ethiopia fungicide trial report
#' (\url{https://core.ac.uk/download/pdf/234661852.pdf}).
#'
#' @examples
#' data(management_ethiopia)
#' head(management_ethiopia)
"management_ethiopia"
