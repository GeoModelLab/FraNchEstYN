#' Toggle calibration flags on nested FraNchEstYN parameter lists
#'
#' These helpers enable or disable the `calibration` flag on **leaf** parameters
#' in the nested lists used by FraNchEstYN (e.g., `cropParameters$Wheat`,
#' `diseaseParameters$Septoria`, or `fungicideParameters$protectant`).
#'
#' A leaf parameter is a list that contains the fields:
#' `min`, `max`, `value`, and `calibration`.
#'
#' @param params A nested parameter list (one set), e.g. `cropParameters$Wheat`.
#'   You may also pass an entire collection (e.g., `cropParameters`), in which
#'   case the operation is applied to each set.
#' @param keys Character vector of parameter names to enable/disable.
#'   Matching occurs against the **leaf parameter name** (e.g., `"RelativeHumidityCritical"`).
#' @param match One of `"exact"` (default), `"regex"`, or `"starts_with"`.
#'   Controls how `keys` are matched to parameter names.
#' @param ignore_case Logical; ignore case during matching (default `TRUE`).
#' @param warn_if_missing Logical; if `TRUE` (default), emit a warning for keys
#'   that did not match any parameter.
#'
#' @return A modified copy of `params` with updated `calibration` flags.
#'
#' @examples
#' \dontrun{
#' # Disable all parameters for calibration in a disease set
#' d <- disable_all_calibration(diseaseParameters$Septoria)
#'
#' # Re-enable a few by exact name
#' d2 <- enable_calibration(d, c("RelativeHumidityCritical", "Rain50Detachment"))
#'
#' # Alternatively: enable only a subset (disables all others)
#' d3 <- enable_calibration_only(
#'   diseaseParameters$Septoria,
#'   keys = c("RelativeHumidityCritical", "Rain50Detachment")
#' )
#'
#' # Use regex to enable whole families
#' c2 <- enable_calibration(
#'   cropParameters$Wheat,
#'   keys = c("^T(min|opt|max)Crop$"),
#'   match = "regex"
#' )
#' }
#' @name calibration_toggles
NULL

# Internal: identify a parameter leaf
.is_param_leaf <- function(x) {
  is.list(x) && all(c("min", "max", "value", "calibration") %in% names(x))
}

# Internal: recursively map a function over a nested params structure
.param_recurse <- function(x, f_leaf, f_branch = identity, name = NULL) {
  if (.is_param_leaf(x)) {
    f_leaf(x, name = name)
  } else if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) nms <- rep(NA_character_, length(x))
    out <- vector("list", length(x))
    for (i in seq_along(x)) {
      out[[i]] <- .param_recurse(x[[i]], f_leaf, f_branch, name = nms[[i]])
    }
    names(out) <- names(x)
    f_branch(out, name = name)
  } else {
    x
  }
}

#' Disable calibration for all parameters
#' @rdname calibration_toggles
#' @export
disable_all_calibration <- function(params) {
  .param_recurse(
    params,
    f_leaf = function(leaf, name) { leaf$calibration <- FALSE; leaf },
    f_branch = identity
  )
}

#' Enable calibration for matching parameters
#' @rdname calibration_toggles
#' @export
enable_calibration <- function(params,
                               keys,
                               match = c("exact", "regex", "starts_with"),
                               ignore_case = TRUE,
                               warn_if_missing = TRUE) {
  match <- match[1]

  # build matcher
  make_matcher <- function(keys, match, ignore_case) {
    if (match == "exact") {
      if (ignore_case) {
        keys_low <- tolower(keys)
        function(name) !is.null(name) && tolower(name) %in% keys_low
      } else {
        function(name) !is.null(name) && name %in% keys
      }
    } else if (match == "starts_with") {
      if (ignore_case) {
        keys_low <- tolower(keys)
        function(name) {
          if (is.null(name)) return(FALSE)
          any(startsWith(tolower(name), keys_low))
        }
      } else {
        function(name) {
          if (is.null(name)) return(FALSE)
          any(startsWith(name, keys))
        }
      }
    } else if (match == "regex") {
      # precompile regex
      flags <- if (ignore_case) "(?i)" else ""
      pats  <- paste0(flags, "(", keys, ")")
      function(name) {
        if (is.null(name)) return(FALSE)
        any(vapply(pats, function(p) grepl(p, name, perl = TRUE), logical(1)))
      }
    } else {
      stop("Unknown 'match' mode: ", match)
    }
  }

  matcher <- make_matcher(keys, match, ignore_case)

  found <- character(0)
  out <- .param_recurse(
    params,
    f_leaf = function(leaf, name) {
      if (matcher(name)) {
        leaf$calibration <- TRUE
        found <<- c(found, name %||% "")
      }
      leaf
    },
    f_branch = identity
  )

  if (warn_if_missing) {
    # unfound keys by the selected matching rule (approximate check for exact mode)
    if (match == "exact") {
      nms <- unique(na.omit(unlist(.collect_leaf_names(params))))
      missing_keys <- setdiff(tolower(keys), tolower(found))
      if (length(missing_keys)) {
        warning("Some keys did not match any parameter: ",
                paste(sort(unique(missing_keys)), collapse = ", "))
      }
    }
  }

  out
}

#' Enable *only* the specified parameters (disable all others)
#' @rdname calibration_toggles
#' @export
enable_calibration_only <- function(params,
                                    keys,
                                    match = c("exact", "regex", "starts_with"),
                                    ignore_case = TRUE,
                                    warn_if_missing = TRUE) {
  params %>%
    disable_all_calibration() %>%
    enable_calibration(keys, match = match, ignore_case = ignore_case,
                       warn_if_missing = warn_if_missing)
}

# Internal: collect leaf names for diagnostics
.collect_leaf_names <- function(x) {
  out <- list()
  .param_recurse(
    x,
    f_leaf = function(leaf, name) { out[[length(out) + 1]] <<- name; leaf },
    f_branch = identity
  )
  out
}

# fallback for `%||%`
`%||%` <- function(a, b) if (!is.null(a)) a else b
