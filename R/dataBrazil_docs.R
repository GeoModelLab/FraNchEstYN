#' Reference wheat data from Brazil (BRILHANTE cultivar, Sertaozinho site)
#'
#' A dataset with experimental reference observations for the BRILHANTE variety
#' grown in Sertaozinho, Brazil.
#' These data come from EPAMIG trials and were processed in
#' `data-raw/build_referenceBrazil.R`.
#'
#' @format A data frame with one row per planting period × year, containing:
#' \describe{
#'   \item{year}{integer. Growing season year.}
#'   \item{variety}{character. Cultivar name (always `"BRILHANTE"`).}
#'   \item{planting_period}{integer. Planting period index.}
#'   \item{Disease}{numeric. Disease severity (DHS), if available; otherwise `NA`.}
#'   \item{yieldActual}{numeric. Yield (kg/ha), if available; otherwise `NA`.}
#'   \item{site}{character. Site name (always `"Sertaozinho"`).}
#'   \item{doy}{numeric. Day of year of the event (heading or harvest).}
#'   \item{fint}{numeric. Indicator of event type:
#'     \code{1} for heading (no yield/disease),
#'     \code{0.1} for harvest (with yield and disease observations).}
#' }
#'
#' @details
#' The dataset includes two kinds of rows:
#' \itemize{
#'   \item Heading observations (\code{fint = 1}), with only \code{doy} filled.
#'   \item Harvest observations (\code{fint = 0.1}), with \code{yieldActual} and
#'     \code{Disease} values from trial data.
#' }
#'
#' @source Santos, G.B. dos, Coelho, M.A.O., & Del Ponte, E.M. (2023).
#' Critical-point yield loss models based on incidence and severity of wheat
#' head blast epidemics in the Brazilian Cerrado. *European Journal of Plant
#' Pathology, 165*, 421–431. \doi{10.1007/s10658-022-02614-7}
#'
#' @examples
#' data(reference_brazil)
#' head(reference_brazil)
"reference_brazil"



#' Management information for BRILHANTE wheat trials in Brazil
#'
#' A dataset with management information derived from EPAMIG experimental
#' trials in Brazil, filtered for the cultivar BRILHANTE.
#' These data were processed in `data-raw/reference_egypt.R`.
#'
#' @format A data frame with one row per planting period × year, containing:
#' \describe{
#'   \item{crop}{character. Crop name (always `"wheat"`).}
#'   \item{variety}{character. Cultivar name (always `"BRILHANTE"`).}
#'   \item{sowingDOY}{integer. Sowing date expressed as day-of-year (DOY).}
#'   \item{year}{integer. Growing season year.}
#'   \item{planting_period}{integer. Planting period index within each year.}
#' }
#'
#' @details
#' These records were derived from EPAMIG trial data (`data-raw/data_epamig.csv`)
#' by summarising heading, harvest, disease severity, and yield across replicates,
#' then extracting only the sowing information relevant for management simulation.
#'
#' @source Santos, G.B. dos, Coelho, M.A.O., & Del Ponte, E.M. (2023).
#' Critical-point yield loss models based on incidence and severity of wheat
#' head blast epidemics in the Brazilian Cerrado. *European Journal of Plant
#' Pathology, 165*, 421–431. \doi{10.1007/s10658-022-02614-7}
#'
#' @examples
#' data(management_brazil)
#' head(management_brazil)
"management_brazil"


#' Default crop parameters for BRILHANTE wheat (Brazil)
#'
#' A nested list of crop parameter values calibrated for the BRILHANTE variety
#' of wheat in Brazil, to be used as input for \code{FraNchEstYN::franchestyn()}.
#'
#' Each element corresponds to one model parameter with fields:
#' \describe{
#'   \item{description}{Character. Parameter description.}
#'   \item{unit}{Character. Measurement unit.}
#'   \item{min}{Numeric. Minimum allowed value.}
#'   \item{max}{Numeric. Maximum allowed value.}
#'   \item{value}{Numeric. Default calibrated value.}
#'   \item{calibration}{Logical. Whether the parameter is estimated in calibration.}
#' }
#'
#' @format A named list with parameter entries.
#'
#' @source Santos, G.B. dos, Coelho, M.A.O., & Del Ponte, E.M. (2023).
#' Critical-point yield loss models based on incidence and severity of wheat
#' head blast epidemics in the Brazilian Cerrado. *European Journal of Plant
#' Pathology, 165*, 421–431. \doi{10.1007/s10658-022-02614-7}
#'
#' @examples
#' data(cropParameters_brazil)
#' names(cropParameters_brazil)[1:5]
"cropParameters_brazil"
