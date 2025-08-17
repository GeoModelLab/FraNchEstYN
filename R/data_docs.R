#' Crop parameter sets for FraNchEstYN
#'
#' A named list of predefined crop parameter sets used in the FraNchEstYN model.
#' The top-level names correspond to crop types (e.g., `"wheat"`, `"rice"`).
#'
#' \strong{Structure of each crop set:}
#' \itemize{
#'   \item Each set is a named list of parameters.
#'   \item Each parameter is itself a list with fields:
#'     \itemize{
#'       \item \code{description} — description of the parameter
#'       \item \code{unit} — unit of measurement
#'       \item \code{min}, \code{max} — allowed bounds
#'       \item \code{value} — default value
#'       \item \code{calibration} — logical; if \code{TRUE}, the parameter can be optimized during calibration
#'     }
#' }
#'
#' @format A named list of crop parameter sets:
#'   \code{cropParameters[[crop]][[parameter]]} → parameter details list.
#'
#' @examples
#' data(cropParameters)
#' names(cropParameters)             # available crop sets
#' names(cropParameters$wheat)       # parameters in Wheat set
#' cropParameters$wheat$TbaseCrop    # details for a single parameter
"cropParameters"


#' Disease parameter sets for FraNchEstYN
#'
#' A named list of predefined disease parameter sets used in the FraNchEstYN model.
#' The top-level names correspond to diseases (e.g., `"brown_rust"`, `"black_rust"`,`"yellow_rust"`, `"septoria"`).
#'
#' \strong{Structure of each disease set:}
#' \itemize{
#'   \item Each set is a named list of parameters.
#'   \item Each parameter is itself a list with fields:
#'     \itemize{
#'       \item \code{description} — description of the parameter
#'       \item \code{unit} — unit of measurement
#'       \item \code{min}, \code{max} — allowed bounds
#'       \item \code{value} — default value
#'       \item \code{calibration} — logical; if \code{TRUE}, the parameter can be optimized during calibration
#'     }
#' }
#'
#' @format A named list of disease parameter sets:
#'   \code{diseaseParameters[[disease]][[parameter]]} → parameter details list.
#'
#' @examples
#' data(diseaseParameters)
#' names(diseaseParameters)               # available disease sets
#' names(diseaseParameters$septoria)      # parameters in Septoria set
#' diseaseParameters$septoria$Tmin        # details for a single parameter
"diseaseParameters"


#' Fungicide parameter sets for FraNchEstYN
#'
#' A named list of predefined fungicide parameter sets used in the FraNchEstYN model.
#' The top-level names correspond to fungicide modes or classes (e.g., `"protectant"`).
#'
#' \strong{Structure of each fungicide set:}
#' \itemize{
#'   \item Each set is a named list of parameters.
#'   \item Each parameter is itself a list with fields:
#'     \itemize{
#'       \item \code{description} — description of the parameter
#'       \item \code{unit} — unit of measurement
#'       \item \code{min}, \code{max} — allowed bounds
#'       \item \code{value} — default value
#'       \item \code{calibration} — logical; if \code{TRUE}, the parameter can be optimized during calibration
#'     }
#' }
#'
#' @format A named list of fungicide parameter sets:
#'   \code{fungicideParameters[[set]][[parameter]]} → parameter details list.
#'
#' @examples
#' data(fungicideParameters)
#' names(fungicideParameters)                   # available fungicide sets
#' names(fungicideParameters$protectant)        # parameters in the protectant set
#' fungicideParameters$protectant$InitialDose   # details for a single parameter
"fungicideParameters"

