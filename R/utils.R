#' Toggle calibration flags on nested FraNchEstYN parameter lists
#'
#' These helpers enable or disable the `calibration` flag on **leaf** parameters
#' in the nested lists used by FraNchEstYN (e.g., `cropParameters$wheat`,
#' `diseaseParameters$septoria`, or `fungicideParameters$protectant`).
#'
#' A leaf parameter is a list that contains the fields:
#' `min`, `max`, `value`, and `calibration`.
#'
#' @param params A nested parameter list (one set), e.g. `cropParameters$wheat`.
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
#' # Disable all parameters for calibration in a disease set (not run)
#' # d <- disable_all_calibration(diseaseParameters$septoria)
#'
#' # Re-enable a few by exact name (not run)
#' # d2 <- enable_calibration(d, c("RelativeHumidityCritical", "Rain50Detachment"))
#'
#' # Enable only a subset (disables all others) (not run)
#' # d3 <- enable_calibration_only(
#' #   diseaseParameters$septoria,
#' #   keys = c("RelativeHumidityCritical", "Rain50Detachment")
#' # )
#'
#' # Use regex to enable whole families (not run)
#' # c2 <- enable_calibration(
#' #   cropParameters$wheat,
#' #   keys = c("^T(min|opt|max)Crop$"),
#' #   match = "regex"
#' # )
#' @name calibration_toggles
NULL

# --- internals ---------------------------------------------------------------

# Identify a parameter leaf
.is_param_leaf <- function(x) {
  is.list(x) && all(c("min", "max", "value", "calibration") %in% names(x))
}

# Identity that accepts an unused name arg
.identity_with_name <- function(x, name = NULL) x

# Recursively apply functions to a nested params structure
.param_recurse <- function(x, f_leaf, f_branch = .identity_with_name, name = NULL) {
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

# Collect leaf names for diagnostics
.collect_leaf_names <- function(x) {
  out <- list()
  .param_recurse(
    x,
    f_leaf   = function(leaf, name) { out[[length(out) + 1]] <<- name; leaf },
    f_branch = .identity_with_name
  )
  out
}

# fallback `%||%`
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- exported helpers --------------------------------------------------------

#' Disable calibration for all parameters
#' @rdname calibration_toggles
#' @export
disable_all_calibration <- function(params) {
  .param_recurse(
    params,
    f_leaf   = function(leaf, name) { leaf$calibration <- FALSE; leaf },
    f_branch = .identity_with_name
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
  match <- match.arg(match)

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
    } else { # regex
      flags <- if (ignore_case) "(?i)" else ""
      pats  <- paste0(flags, "(", keys, ")")
      function(name) {
        if (is.null(name)) return(FALSE)
        any(vapply(pats, function(p) grepl(p, name, perl = TRUE), logical(1)))
      }
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
    f_branch = .identity_with_name
  )

  if (isTRUE(warn_if_missing) && match == "exact") {
    leaf_names <- unique(na.omit(unlist(.collect_leaf_names(params))))
    missing_keys <- setdiff(tolower(keys), tolower(found))
    if (length(missing_keys)) {
      warning("Some keys did not match any parameter: ",
              paste(sort(unique(missing_keys)), collapse = ", "))
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
  params2 <- disable_all_calibration(params)
  enable_calibration(params2, keys,
                     match = match, ignore_case = ignore_case,
                     warn_if_missing = warn_if_missing)
}

#' Convert a nested parameter list back into a tidy data.frame
#'
#' @param param_list A nested parameter list (whole model, or a single set)
#' @return A tidy data.frame with columns:
#'   Parameter, Description, Unit, Min, Max, Value, Calibration
#' @export
parameters_to_df <- function(param_list) {
  stopifnot(is.list(param_list), !is.null(names(param_list)))

  data.frame(
    Parameter   = names(param_list),
    Description = vapply(param_list, function(x) x$description, character(1)),
    Unit        = vapply(param_list, function(x) x$unit, character(1)),
    Min         = vapply(param_list, function(x) x$min, numeric(1)),
    Max         = vapply(param_list, function(x) x$max, numeric(1)),
    Value       = vapply(param_list, function(x) x$value, numeric(1)),
    Calibration = vapply(param_list, function(x) x$calibration, logical(1)),
    stringsAsFactors = FALSE
  )
}

#' Convert parameter summary dataframe to FraNchEstYN parameter list
#'
#' This function converts a dataframe of parameter estimates into the nested
#' list format used by FraNchEstYN (`cropParameters` or `diseaseParameters`).
#'
#' @param df A `data.frame` with at least columns `Parameter` and `Value`.
#' @param range_pct Numeric, optional. Percentage of the mean value to define
#'   min/max ranges (default = 0.2, i.e. ±20%).
#' @param atomic_if_missing_meta Logical, optional. If `TRUE`, parameters
#'   without metadata are stored as scalars instead of lists.
#' @param zero_buffer Numeric, optional. Small positive buffer when value = 0
#'   (default = 1e-6).
#'
#' @return A **named list** of parameters. Each element is either:
#' \itemize{
#'   \item A single numeric value (if `atomic_if_missing_meta = TRUE` and no
#'         metadata are provided).
#'   \item Otherwise, a list with elements:
#'     \describe{
#'       \item{description}{Character string with parameter description (if available).}
#'       \item{unit}{Unit of measurement (if available).}
#'       \item{min}{Lower bound of the parameter (mean - range).}
#'       \item{max}{Upper bound of the parameter (mean + range).}
#'       \item{value}{Central value (mean).}
#'       \item{calibration}{Logical flag, set to `FALSE` by default.}
#'     }
#' }
#'
#' @details
#' The ranges are computed as \code{value * (1 ± range_pct)}. This ensures
#' each parameter has a plausible min and max around the central estimate.
#' If `value` is 0, a buffer of `[0, zero_buffer]` is used.
#'
#' @examples
#' param_summary <- data.frame(
#'   Parameter = c("RUEreducerDamage","LatentPeriod"),
#'   Value     = c(0.5, 120)
#' )
#' df_to_parameters(param_summary, range_pct = 0.1)
#'
#' @export
df_to_parameters <- function(df,
                             atomic_if_missing_meta = FALSE,
                             range_pct = 0.10,
                             zero_buffer = 1e-6) {
  stopifnot(is.data.frame(df), nrow(df) > 0)

  # normalize range_pct: allow 10 or 0.10
  if (range_pct > 1 && range_pct <= 100) range_pct <- range_pct / 100
  if (range_pct > 1) stop("`range_pct` > 100% is not allowed.")

  find_col <- function(cands) {
    nms <- tolower(names(df))
    for (cand in cands) {
      j <- which(nms == tolower(cand))
      if (length(j)) return(names(df)[j[1]])
    }
    NULL
  }

  col_param <- find_col(c("Parameter","param","name"))
  col_value <- find_col(c("Value","val"))
  if (is.null(col_param) || is.null(col_value)) {
    stop("`Parameter` and `Value` columns are required (case-insensitive).")
  }

  col_desc <- find_col(c("Description","desc"))
  col_unit <- find_col(c("Unit","units"))
  col_min  <- find_col(c("Min","minimum"))
  col_max  <- find_col(c("Max","maximum"))
  col_cal  <- find_col(c("Calibration","calibrated","calib"))

  params <- df[[col_param]]

  get_chr <- function(col) if (!is.null(col)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
  get_num <- function(col) if (!is.null(col)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))
  get_lgl <- function(col) {
    if (is.null(col)) return(rep(NA, nrow(df)))
    x <- df[[col]]
    if (is.logical(x)) return(x)
    lx <- tolower(as.character(x))
    out <- rep(NA, length(lx))
    out[lx %in% c("true","t","1","yes","y")]  <- TRUE
    out[lx %in% c("false","f","0","no","n")] <- FALSE
    out
  }

  desc <- get_chr(col_desc)
  unit <- get_chr(col_unit)
  minv <- get_num(col_min)
  maxv <- get_num(col_max)
  val  <- get_num(col_value)
  cal  <- get_lgl(col_cal)

  # fill min/max when missing
  for (i in seq_len(nrow(df))) {
    if (!is.na(val[i]) && (is.na(minv[i]) || is.na(maxv[i]))) {
      if (abs(val[i]) < .Machine$double.eps) {
        # value ~ 0 → give a small buffer [0, zero_buffer]
        minv[i] <- 0
        maxv[i] <- zero_buffer
      } else {
        delta <- abs(val[i]) * range_pct
        minv[i] <- val[i] - delta
        maxv[i] <- val[i] + delta
      }
    }
  }

  out <- vector("list", length = nrow(df))
  names(out) <- params

  for (i in seq_len(nrow(df))) {
    has_meta <- any(!is.na(c(desc[i], unit[i], minv[i], maxv[i], cal[i])))
    if (atomic_if_missing_meta && !has_meta) {
      out[[i]] <- val[i]
    } else {
      out[[i]] <- list(
        description  = desc[i],
        unit         = unit[i],
        min          = minv[i],
        max          = maxv[i],
        value        = val[i],
        calibration  = cal[i]
      )
    }
  }

  out
}
