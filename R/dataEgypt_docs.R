#' Reference dataset for Egypt, yellow rust severity and yield loss on seven wheat varieties (Sharkiya, 2013–2015)
#'
#' This dataset provides reference observations for wheat stripe rust trials
#' conducted in Sharkiya, Egypt. It includes disease severity assessments and
#' attainable vs. actual yield data digitized from Omara et al. (2018), available at
#' [Losses Assessment in some Egyptian Wheat Cultivars caused by Stripe Rust Pathogen *Puccinia striiformis*](https://ejp.journals.ekb.eg/article_92381_3f2e26b0341fd17294835a460fad0c50.pdf).
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{site}{Trial site (e.g. "Sharkiya")}
#'   \item{variety}{Cultivar identifier (e.g. "sids12")}
#'   \item{year}{Year of trial (numeric)}
#'   \item{DOY}{Day of year (numeric)}
#'   \item{disease}{Observed disease severity (\%), may be NA}
#'   \item{fint}{Field intensity score (numeric, may be NA)}
#'   \item{yieldActual}{Measured actual yield (kg/ha)}
#'   \item{yieldAttainable}{Measured attainable yield (kg/ha)}
#' }
#'
#' @source Omara et al. (2018). \url{https://ejp.journals.ekb.eg/article_92381_3f2e26b0341fd17294835a460fad0c50.pdf}
#'
#' @examples
#' data(reference_egypt)
#' head(reference_egypt)
"reference_egypt"

#' Daily weather dataset for Sharkiya, Egypt (2013–2015)
#'
#' This dataset provides daily weather observations for Sharkiya, Egypt, covering
#' the same period as the wheat yellow rust severity and yield trials
#' (`reference_egypt`). It includes maximum and minimum temperatures, rainfall,
#' and metadata such as site and latitude.
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{Site}{Trial site ("Sharkiya")}
#'   \item{TMAX}{Daily maximum temperature (°C)}
#'   \item{TMIN}{Daily minimum temperature (°C)}
#'   \item{RAIN}{Daily rainfall (mm)}
#'   \item{year}{Calendar year (numeric)}
#'   \item{month}{Calendar month (1–12)}
#'   \item{day}{Calendar day of month (1–31)}
#'   \item{lat}{Latitude of trial site (decimal degrees)}
#' }
#'
#' @source Digitized from Sharkiya daily weather records (2013–2015).
#'
#' @examples
#' data(weather_egypt)
#' head(weather_egypt)
"weather_egypt"
