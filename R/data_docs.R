#' Crop parameter sets for FraNchEstYN
#'
#' A named list of predefined crop parameter sets used in the FraNchEstYN model.
#' The top-level names correspond to crop types (e.g., `"Wheat"`, `"Maize"`).
#'
#' Each set is itself a named list of parameters.
#' Each parameter entry contains:
#' \itemize{
#'   \item \code{description} Description of the parameter
#'   \item \code{unit} Unit of measurement
#'   \item \code{min} Minimum allowed value
#'   \item \code{max} Maximum allowed value
#'   \item \code{value} Default value
#'   \item \code{calibration} Logical flag indicating if the parameter is used in calibration
#' }
#'
#' @format A named list of lists of lists:
#'   \code{cropParameters[[set]][[parameter]]} → parameter details list.
#' @examples
#' data(cropParameters)
#' names(cropParameters)             # available crop sets
#' names(cropParameters$Wheat)       # parameters in Wheat set
#' cropParameters$Wheat$TbaseCrop    # details for a single parameter
"cropParameters"


#' Disease parameter sets for FraNchEstYN
#'
#' A named list of predefined disease parameter sets used in the FraNchEstYN model.
#' The top-level names correspond to diseases (e.g., `"Septoria"`, `"WheatRust"`).
#'
#' Each set is itself a named list of parameters.
#' Each parameter entry contains:
#' \itemize{
#'   \item \code{description} Description of the parameter
#'   \item \code{unit} Unit of measurement
#'   \item \code{min} Minimum allowed value
#'   \item \code{max} Maximum allowed value
#'   \item \code{value} Default value
#'   \item \code{calibration} Logical flag indicating if the parameter is used in calibration
#' }
#'
#' @format A named list of lists of lists:
#'   \code{diseaseParameters[[set]][[parameter]]} → parameter details list.
#' @examples
#' data(diseaseParameters)
#' names(diseaseParameters)               # available disease sets
#' names(diseaseParameters$Septoria)      # parameters in Septoria set
#' diseaseParameters$Septoria$Tmin        # details for a single parameter
"diseaseParameters"
