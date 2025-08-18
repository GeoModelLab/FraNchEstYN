#' Calibration toggles for FraNchEstYN parameter sets
#'
#' Utilities to quickly disable all calibration flags, enable only a subset,
#' or set calibration flags in bulk for nested parameter lists used by
#' \strong{FraNchEstYN} (e.g., `cropParameters$wheat`, `diseaseParameters$septoria`).
#'
#' Parameter lists are nested lists where each \strong{leaf} has at least:
#' `description`, `unit`, `min`, `max`, `value`, and `calibration` (logical).
#'
#' @param params A named (possibly nested) list of parameter definitions.
#' @param keys Character vector of parameter names to enable (exact leaf names).
#' @return A modified parameter list with updated `calibration` flags.
#'
#' @section Notes:
#' - Functions traverse the list and only flip `calibration` at **leaf** nodes
#'   that contain that field.
#' - Names are matched \strong{exactly} at the leaf level (no partial matching).
#'
#' @examples
#' # Disable all, then re-enable a few:
#' p <- cropParameters$wheat
#' p <- disable_all_calibration(p)
#' p <- enable_calibration_only(
#'   p,
#'   keys = c("RadiationUseEfficiency", "PartitioningMaximum")
#' )
#'
#' # Or set flags in one call:
#' p <- set_calibration_flags(
#'   p,
#'   enable  = c("RadiationUseEfficiency"),
#'   disable = c("CycleLength", "TmaxCrop")
#' )
#'
#' # Non-running examples:
#' \dontrun{
#' # Disable all parameters for calibration in a disease set
#' d <- disable_all_calibration(diseaseParameters$septoria)
#'
#' # Re-enable a few by exact name
#' d2 <- enable_calibration_only(
#'   d,
#'   keys = c("RelativeHumidityCritical", "Rain50Detachment")
#' )
#'
#' # Enable an entire family of parameters using a regex pattern
#' w <- cropParameters$wheat
#' t_keys <- grep("^T(min|opt|max)Crop$", names(w), value = TRUE)
#' w2 <- set_calibration_flags(w, enable = t_keys)
#' }
#' @name calibration_toggles
#' @aliases set_calibration_flags enable_calibration_only disable_all_calibration
NULL
#' @rdname calibration_toggles
#' @export
disable_all_calibration <- function(params) {
  walk <- function(x) {
    if (is.list(x) && all(c("min", "max", "value", "calibration") %in% names(x))) {
      x$calibration <- FALSE
      return(x)
    }
    if (is.list(x)) return(lapply(x, walk))
    x
  }
  walk(params)
}

#' @rdname calibration_toggles
#' @export
enable_calibration_only <- function(params, keys) {
  keys <- unique(as.character(keys))
  walk <- function(x, nm = NULL) {
    if (is.list(x) && all(c("min", "max", "value", "calibration") %in% names(x))) {
      if (!is.null(nm) && nm %in% keys) x$calibration <- TRUE else x$calibration <- FALSE
      return(x)
    }
    if (is.list(x)) {
      nms <- names(x)
      return(Map(function(el, n) walk(el, n), x, if (is.null(nms)) vector("list", length(x)) else nms))
    }
    x
  }
  structure(walk(params), names = names(params))
}

#' @rdname calibration_toggles
#' @param enable Character vector of leaf names to enable.
#' @param disable Character vector of leaf names to disable.
#' @export
set_calibration_flags <- function(params, enable = character(), disable = character()) {
  en <- unique(as.character(enable))
  dis <- unique(as.character(disable))
  walk <- function(x, nm = NULL) {
    if (is.list(x) && all(c("min", "max", "value", "calibration") %in% names(x))) {
      if (!is.null(nm) && nm %in% en) x$calibration <- TRUE
      if (!is.null(nm) && nm %in% dis) x$calibration <- FALSE
      return(x)
    }
    if (is.list(x)) {
      nms <- names(x)
      return(Map(function(el, n) walk(el, n), x, if (is.null(nms)) vector("list", length(x)) else nms))
    }
    x
  }
  structure(walk(params), names = names(params))
}
