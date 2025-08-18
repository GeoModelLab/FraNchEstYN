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
