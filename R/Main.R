#' Run the FraNchEstYN crop–disease simulation or calibration
#'
#' Runs the FraNchEstYN model. The function prepares inputs
#' (weather, management, parameters, reference) and launches the executable.
#' Users do not manage paths or config files manually.
#'
#' @param weather_data A data frame of daily or hourly weather for \strong{one site only}.
#'   The function will detect if input is daily or hourly.
#'   Column names are matched case-insensitively, ignoring spaces, underscores, and dashes.
#'
#'   \strong{Date columns (mandatory):}
#'
#'   Provide the combination of \code{year}, \code{month}, \code{day}
#'   (and optionally \code{hour} for hourly data).
#'
#'   \strong{Meteorological variables:}
#'
#'   \emph{Mandatory:}
#'   \itemize{
#'     \item Daily inputs: \code{tmax}, \code{tx}, \code{t2mmax}, \code{maxtemp}
#'           (max temperature, °C),
#'           \code{tmin}, \code{tn}, \code{t2mmin}, \code{mintemp}
#'           (min temperature, °C),
#'           \code{precipitation}, \code{prec}, \code{p}, \code{rainfall}, \code{rain}
#'           (mm d\eqn{^{-1}}).
#'     \item Hourly inputs: \code{temp}, \code{temperature}, \code{t2m}
#'           (air temperature, °C),
#'           \code{precipitation}, \code{prec}, \code{precip}, \code{prectotcorr},
#'           \code{rainfall}, \code{rain} (mm h\eqn{^{-1}}).
#'   }
#'
#'   \emph{Radiation or Latitude (one required):}
#'   \itemize{
#'     \item Radiation: \code{rad}, \code{solar}, \code{solarrad} [MJ m\eqn{^{-2}} d\eqn{^{-1}}]
#'     \item Latitude: \code{lat}, \code{latitude}, \code{site_lat} [decimal degrees]
#'   }
#'   If radiation is missing, it will be estimated from latitude and day length.
#'
#'   \emph{Optional variables (used if present, estimated otherwise):}
#'   \itemize{
#'     \item Relative humidity:
#'       \code{rh}, \code{humidity}, \code{relhumidity}, \code{relativehumidity} (hourly),
#'       \code{rhmax}, \code{rhx} and \code{rhmin}, \code{rhn} (daily).
#'     \item Leaf wetness: not required — computed internally from
#'       humidity > 90\% or rainfall ≥ 0.2 mm/h.
#'   }
#'
#' @param management_data A data frame with management information for the
#'   \strong{same site} as \code{weather_data}. Column matching is case-insensitive;
#'   spaces/underscores/dashes are ignored and normalized to snake_case.
#'
#'   \strong{Required columns:}
#'   \itemize{
#'     \item \code{crop} — character (e.g., "Wheat").
#'     \item \code{sowingDOY} — integer DOY in \code{[1, 366]}.
#'     \item \code{year} — either an ISO year (YYYY) or the string \code{"All"}.
#'   }
#'
#'   \strong{Optional:}
#'   \itemize{
#'     \item \code{treatment} — character with one or more fungicide dates
#'           separated by commas/semicolons (e.g., \code{"12 Feb; 28 Feb"}).
#'   }
#'
#' @param reference_data An optional data frame with observations;
#'   **required when** \code{calibration != "none"}.
#'   Column names are matched case-insensitively, spaces trimmed, and aliases accepted.
#'
#'   \strong{Minimum requirement for disease calibration:}
#'   Must contain a disease severity column:
#'   \code{DiseaseSeverity}, \code{dissev}, or \code{disease}.
#'   Values should be fractional in \code{[0,1]}.
#'
#'   Recommended alignment: year + DOY
#'   (\code{year}/\code{yr} + \code{doy}/\code{day_of_year}).
#'
#' @param cropParameters A named list of crop parameters
#'   (see \code{data(cropParameters)}).
#'
#' @param diseaseParameters A named list of disease parameters
#'   (see \code{data(diseaseParameters)}).
#'
#' @param fungicideParameters Optional list of fungicide parameters
#'   (see \code{data(fungicideParameters)}).
#'
#' @param calibration Character. What to calibrate:
#'   \code{"none"}, \code{"crop"}, \code{"disease"}, or \code{"all"}.
#'
#' @param start_end Numeric vector of length 2. Start and end years for simulation
#'   (default: \code{c(2000, 2025)}).
#'
#' @param apikey Optional string. API key for enabling LLM-based commentary.
#'   Generated at \url{https://openrouter.ai/}.
#'
#' @param franchy_message Logical. If TRUE, generate commentary via LLM.
#'
#' @param ... Advanced options (hidden). Currently supports:
#'   \code{iterations} (integer; default 100).
#'
#'
#' @details
#' - Only one site per run.
#' - Column names matched case-insensitively, tolerant to spaces, underscores, and dashes.
#'
#' @return A list with elements: \code{outputs}, \code{diagnostics},
#'   \code{parameters}, \code{spooky_message}.
#'
#' @examples
#' \dontrun{
#' franchestyn(
#'   weather_data        = weather_df,
#'   management_data     = mgmt_df,
#'   reference_data      = ref_df,
#'   cropParameters      = cropParameters$wheat,
#'   diseaseParameters   = diseaseParameters$septoria,
#'   fungicideParameters = fungicideParameters$protectant,
#'   calibration         = "all",
#'   start_end           = c(2010, 2020),
#'   apikey              = "sk-or-v1-xxxx",
#'   franchy_message     = TRUE,
#'   iterations          = 200
#' )
#' }
#' @export
franchestyn <- function(weather_data, management_data, reference_data = NULL,
                        cropParameters = NULL, diseaseParameters = NULL, fungicideParameters = NULL,
                        calibration  = 'disease',
                        start_end = c(2000,2025),
                        apikey = NULL,                 # optional API key
                        franchy_message = FALSE,
                        ...)# whether to request spooky LLM summary)
{
  # --- capture advanced args ---
  dots <- list(...)
  iterations <- dots$iterations %||% 100  # default to 100 if not provided

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # --- detect timestep automatically ---
  normalize <- function(x) {
    # Lowercase and remove spaces, underscores, and dashes
    tolower(gsub("[ _-]+", "", x))
  }

  cn_norm <- normalize(names(weather_data))
  hourly_aliases <- normalize(c("hour", "hours", "hourly", "hr", "hod"))

  if (any(cn_norm %in% hourly_aliases)) {
    timestep <- "hourly"
    message(sprintf("🔮 Hourly weather data detected (column(s): %s).",
                    paste(names(weather_data)[cn_norm %in% hourly_aliases], collapse = ", ")))
  } else {
    timestep <- "daily"
    message("🐍 No hourly column detected; assuming daily weather data.")
  }

  # check if reference_data is required
  # Parse calibration argument
  calibration <- tolower(calibration)
  disease     <- 'thisDisease'

  # Determine mode
  mode <- if (calibration == "none") "simulation" else "calibration"

  # Detect calibration model
  calibrationModel <- "crop"
  if (calibration %in% c("crop", "all")) {
    calibrationModel <- ifelse(calibration == "crop", "crop", "All")
  } else if (grepl("^disease(:.*)?$", trimws(calibration), ignore.case = TRUE)) {
    calibrationModel <- "disease"
  }

  # Check requirements for calibration
  if (calibration != "none") {
    if (is.null(reference_data)) {
      stop("🦇 'reference_data' must be provided for calibration.")
    }
  } else {
    if (is.null(reference_data)) {
      # create fake reference data for C#
      reference_data <- data.frame(
        crop    = "thisCrop",
        Disease = 0,
        doy     = 300,
        year    = start_year,
        stringsAsFactors = FALSE
      )
    }
  }

  #TODO PATHS!!!
  pkg_path <- system.file("", package = "FraNchEstYN")
  exe_path <- system.file("bin", "FraNchEstYN.exe", package = "FraNchEstYN")
  # pkg_path <- file.path(getwd(), "inst")
  # exe_path <- file.path(pkg_path, "bin", "FraNchEstYN.exe")

  if (!file.exists(exe_path)) stop("❌ Executable not found.")

  list_files_out <- list.files(
    path = file.path(dirname(exe_path), "outputs"),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # Remove all matching files
  if (length(list_files_out) > 0) {file.remove(list_files_out)}

  # Check weather_data
  if (!is.data.frame(weather_data)) stop("❌ 'weather_data' must be a data frame.")
  # Case-insensitive check for 'Site'
  site_col <- names(weather_data)[tolower(names(weather_data)) == "site"]
  if (length(site_col) == 0) {
    stop("🧟 'weather_data' must contain a 'Site' column (case-insensitive).")
  } else {
    names(weather_data)[names(weather_data) == site_col] <- "Site"
  }

  # will accept "Indiana" and return the canonical name from management_data
  sites <- unique(weather_data$Site)

  if(length(sites)>1)
  {
    stop("💀 weather_data' must contain a single site!")
  }

  # Check Parameters
  if (!is.list(cropParameters)) stop("🕸 'cropParameters' must be a nested list.")
  if (!is.list(diseaseParameters)) stop("🕸 'diseaseParameters' must be a nested list.")

  # Run the check:
  validate_parameter_ranges(
    cropParameters = cropParameters,
    diseaseParameters = diseaseParameters,
    fungicideParameters = fungicideParameters
  )

  # Check years
  # --- validate start_end ---
  if (!is.numeric(start_end) || length(start_end) != 2) {
    stop("👹 'start_end' must be a numeric vector of length 2: c(start, end)")
  }
  start_year <- start_end[1]
  end_year   <- start_end[2]

  if (start_year > end_year) {
    stop("👹 'start_end[1]' must be <= 'start_end[2]'")
  }

  # Check timestep
  if (!timestep %in% c("daily", "hourly")) {
    stop("🎃 'timestep' must be either 'daily' or 'hourly'.")
  }

  # CONFIGURATION----
  # Path to the EXE
  if (!file.exists(exe_path)) stop("🧛 Executable not found.")

  # Subfolders
  input_weather_dir <- file.path(dirname(exe_path), "files", "weather", tolower(timestep))
  input_parameters_dir   <- file.path(dirname(exe_path), "files", "parameters")
  input_reference_dir     <- file.path(dirname(exe_path), "files", "reference")
  input_management_dir     <- file.path(dirname(exe_path), "files", "management")

  dir.create(input_weather_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(input_parameters_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(input_reference_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(input_management_dir, recursive = TRUE, showWarnings = FALSE)

  # Path to write the reference file
  prepare_reference_data <- function(reference_data,
                                     calibration,
                                     disease_name = "thisDisease",
                                     verbose = TRUE) {
    # Normalize calibration
    calib <- tolower(trimws(as.character(calibration)))

    # Trim spaces from column names
    names(reference_data) <- trimws(names(reference_data))

    # Accepted aliases (case-insensitive)
    disease_aliases <- c("diseaseseverity", "dissev", "disease")
    year_aliases    <- c("year")
    doy_aliases     <- c("doy")

    nms <- names(reference_data)
    low <- tolower(nms)

    # ---------------------------
    # Check YEAR column
    year_hit <- match(year_aliases, low, nomatch = 0)
    year_hit <- year_hit[year_hit > 0]
    if (length(year_hit) == 0) {
      stop(
        sprintf("reference_data must contain a 'year' column (accepted names: %s).\nColumns present: %s",
                paste(year_aliases, collapse = ", "),
                paste(nms, collapse = ", ")),
        call. = FALSE
      )
    }

    # Check DOY column
    doy_hit <- match(doy_aliases, low, nomatch = 0)
    doy_hit <- doy_hit[doy_hit > 0]
    if (length(doy_hit) == 0) {
      stop(
        sprintf("reference_data must contain a 'doy' column (accepted names: %s).\nColumns present: %s",
                paste(doy_aliases, collapse = ", "),
                paste(nms, collapse = ", ")),
        call. = FALSE
      )
    }

    # ---------------------------
    # Locate potential disease column(s)
    hit <- match(disease_aliases, low, nomatch = 0)
    hit <- hit[hit > 0]

    if (calib == "crop") {
      # Disease column is OPTIONAL
      if (length(hit) > 0) {
        idx <- hit[1]
        names(reference_data)[idx] <- disease_name
      }
    } else {
      # Disease column is REQUIRED
      if (length(hit) == 0) {
        stop(
          sprintf(
            "When calibration is '%s', reference_data must contain a disease column named one of: %s.\nColumns present: %s",
            calib,
            paste(c("DiseaseSeverity", "dissev", "disease"), collapse = ", "),
            paste(nms, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      idx <- hit[1]
      names(reference_data)[idx] <- disease_name
    }

    reference_data
  }


  ref_file <- file.path(input_reference_dir, "referenceData.csv")

  #required columns for c#
  reference_data$site    <- sites
  reference_data$variety <- "Generic"

  # Make the column named exactly "thisDisease"
  reference_data <- prepare_reference_data(reference_data,
                                           calibration = calibration,
                                           disease_name = "thisDisease")

  write.table(
    reference_data,
    file = ref_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  # build output file path
  man_file <- file.path(input_management_dir, "sowing.csv")

  # normalize names for safety
  names(management_data) <- tolower(gsub("\\s+", "_", names(management_data)))

  # required columns
  required <- c("crop", "sowingdoy", "year")
  missing_req <- setdiff(required, names(management_data))
  if (length(missing_req)) {
    stop("Missing required column(s): ", paste(missing_req, collapse = ", "))
  }

  # override or assign site column
  if (length(sites) == 1L) {
    management_data$site <- sites
  } else if (length(sites) == nrow(management_data)) {
    management_data$site <- sites
  } else {
    stop("`sites` must be length 1 or equal to nrow(management_data).")
  }

  # coerce crop & variety
  management_data$crop    <- as.character(management_data$crop)
  management_data$variety <- "Generic"

  # sowingDOY checks
  management_data$sowingdoy <- as.integer(management_data$sowingdoy)
  bad_doy <- !is.na(management_data$sowingdoy) &
    !(management_data$sowingdoy >= 1 & management_data$sowingdoy <= 366)
  if (any(bad_doy)) {
    warning("Some sowingDOY values are out of [1..366]: rows ",
            paste(which(bad_doy), collapse = ", "))
  }

  # year handling
  management_data$year <- trimws(as.character(management_data$year))
  is_all   <- tolower(management_data$year) == "all"
  is_yyyy  <- grepl("^\\d{4}$", management_data$year)
  bad_year <- !(is_all | is_yyyy | is.na(management_data$year))
  if (any(bad_year)) {
    warning("Some 'year' values are neither 'All' nor YYYY: rows ",
            paste(which(bad_year), collapse = ", "))
  }
  yr_num <- suppressWarnings(as.integer(management_data$year))
  yr_num[!is_yyyy] <- NA_integer_
  if (!any(is_all, na.rm = TRUE)) {
    management_data$year <- yr_num
    if (any(!is.na(management_data$year) &
            (management_data$year < 1900 | management_data$year > 2100))) {
      warning("Some 'year' values are outside [1900..2100]: rows ",
              paste(which(!is.na(management_data$year) &
                            (management_data$year < 1900 | management_data$year > 2100)),
                    collapse = ", "))
    }
  } else {
    out_of_range <- !is.na(yr_num) & (yr_num < 1900 | yr_num > 2100)
    if (any(out_of_range)) {
      warning("Some numeric 'year' values are outside [1900..2100]: rows ",
              paste(which(out_of_range), collapse = ", "))
    }
    # keep as character when "All" is present
  }

  # if a 'treatment' column is present, parse dates into DOY treatment_1..n
  if ("treatment" %in% names(management_data)) {
    fallback_year <- 2021L  # used when year == "All" or missing

    parse_doy <- function(tok, yr) {
      tok <- gsub("[-./]", " ", tok, perl = TRUE)
      tok <- gsub("\\s+", " ", trimws(tok))
      for (fmt in c("%d %b %Y", "%b %d %Y", "%d %B %Y", "%B %d %Y")) {
        d <- as.Date(paste(tok, yr), format = fmt)
        if (!is.na(d)) return(as.integer(strftime(d, "%j")))
      }
      NA_integer_
    }

    for (i in seq_len(nrow(management_data))) {
      cell <- management_data$treatment[[i]]
      if (is.null(cell)) next

      toks <- if (length(cell) > 1) as.character(cell) else {
        x <- as.character(cell)
        if (is.na(x) || !nzchar(trimws(x))) character(0) else
          trimws(unlist(strsplit(x, "[,;]+", perl = TRUE)))
      }
      if (!length(toks)) next

      ref_year <- if (is_all[i]) fallback_year else suppressWarnings(as.integer(management_data$year[i]))
      if (is.na(ref_year)) ref_year <- fallback_year

      doys <- vapply(toks, parse_doy, integer(1L), yr = ref_year)
      doys <- doys[!is.na(doys)]
      if (!length(doys)) next

      for (k in seq_along(doys)) {
        management_data[[paste0("treatment_", k)]][i] <- as.numeric(doys[k])
      }
    }
    # we won't select 'treatment' below, so it won't appear in the output
  }

  # detect treatment columns
  tr_cols <- grep("^treatment_\\d+$", names(management_data),
                  ignore.case = TRUE, value = TRUE)
  if (length(tr_cols)) {
    tr_order <- order(as.integer(sub("^treatment_(\\d+)$", "\\1", tr_cols)))
    tr_cols <- tr_cols[tr_order]
    management_data[tr_cols] <- lapply(management_data[tr_cols], as.numeric)
  }

  # select & order columns
  keep_cols <- c("site", "crop", "variety", "sowingdoy", "year", tr_cols)
  management_data <- management_data[, keep_cols, drop = FALSE]

  # write the file
  write.table(
    management_data,
    file = man_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  # WRITE WEATHER FILE ----
  # Validate required columns
  if (!"Site" %in% names(weather_data)) stop("Missing 'Site' column in weather_data")

  site_names <- sites  # This will be used in the JSON config

  # WRITE PARAMETERS FILE----
  # Detect a leaf parameter node
  .is_param_leaf <- function(x) {
    is.list(x) && all(c("min","max","value","calibration") %in% names(x))
  }

  params_to_df <- function(x, model = NA_character_) {
    rows <- list()

    walk <- function(node, path = character()) {
      if (.is_param_leaf(node)) {
        param_name <- if (length(path)) tail(path, 1) else NA_character_
        rows[[length(rows) + 1]] <<- data.frame(
          Model       = model,
          Parameter   = param_name,
          Description = node$description,
          unit = node$unit,
          min         = node$min,
          max         = node$max,
          value       = node$value,
          calibration = ifelse(isTRUE(node$calibration), "x", ""),
          stringsAsFactors = FALSE
        )
        return()
      }
      if (is.list(node)) {
        nms <- names(node)
        if (is.null(nms)) nms <- rep(NA_character_, length(node))
        for (i in seq_along(node)) {
          walk(node[[i]], c(path, nms[[i]]))
        }
      }
    }

    walk(x)

    if (!length(rows)) {
      return(data.frame(
        Model=character(), Parameter=character(), Description = character(), unit = character(),
        min=numeric(), max=numeric(), value=numeric(), calibration=character(),
        stringsAsFactors = FALSE
      ))
    }

    do.call(rbind, rows)
  }

  params_crop_df <- params_to_df(cropParameters, model = "crop")
  params_disease_df <- params_to_df(diseaseParameters, model = "disease")
  params_fungicide_df <- params_to_df(fungicideParameters, model = 'fungicide')

  param_file <- file.path(input_parameters_dir, "franchestynParameters.csv")
  readr::write_csv(rbind(params_crop_df,params_disease_df,params_fungicide_df),param_file)

  # --- WEATHER FILE WRITING ---
  # Aliases for date/time columns
  year_aliases  <- c("year", "yr", "yyyy")
  month_aliases <- c("month", "mo", "mn", "mm")
  day_aliases   <- c("day", "dy", "dd")
  hour_aliases  <- c("hour", "hr", "hh")

  rad_aliases <- c("rad", "radiation", "solar", "solarrad", "rs", "gsr", 'srad')
  lat_aliases <- c("lat", "latitude", "site_lat", "phi")

  nms <- tolower(names(weather_data))

  # --- require either rad or lat ---
  has_rad <- any(nms %in% rad_aliases)
  has_lat <- any(nms %in% lat_aliases)
  if (!has_rad && !has_lat) {
    stop("☠: 'weather_data' must contain either a radiation column (rad/solar/rs/...) or a latitude column (lat/latitude/...).")
  }

  # --- find matching column names via aliases ---
  ycol <- names(weather_data)[match(TRUE, nms %in% year_aliases)]
  mcol <- names(weather_data)[match(TRUE, nms %in% month_aliases)]
  dcol <- names(weather_data)[match(TRUE, nms %in% day_aliases)]

  if (any(is.na(c(ycol, mcol, dcol)))) {
    stop("☠: 'weather_data' must contain columns for year, month, and day (aliases allowed).")
  }

  # --- if timestep = hourly, check for hour col ---
  if (timestep == "hourly") {
    hcol <- names(weather_data)[match(TRUE, nms %in% hour_aliases)]
    if (is.na(hcol)) {
      stop("☠: 'weather_data' must contain an 'hour' column for hourly timestep.")
    }
  }

  # --- build date/time ---
  y <- as.integer(weather_data[[ycol]])
  m <- as.integer(weather_data[[mcol]])
  d <- as.integer(weather_data[[dcol]])
  if (timestep == "hourly") {
    h <- as.integer(weather_data[[hcol]])
    weather_data$DateTime <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:00", y, m, d, h), tz = "UTC")
  } else {
    weather_data$Date <- as.Date(sprintf("%04d-%02d-%02d", y, m, d))
  }

  # --- write to CSV ---
  out_file <- file.path(input_weather_dir, paste0(sites, ".csv"))
  write.table(weather_data, out_file, sep = ",",
              row.names = FALSE, col.names = TRUE, quote = FALSE)

  # BUILD JSON ----
  config <- list(
    settings = list(
      startYear = start_year,
      endYear = end_year,
      sites = as.list(site_names),
      isCalibration = ifelse(calibration!='none','true','false'),
      calibrationVariable = calibration,
      varieties = as.list(c("Generic")),
      simplexes = 3,
      disease = disease,
      iterations = iterations,
      weatherTimeStep = tolower(timestep)
    ),
    paths = list(
      weatherDir = normalizePath(dirname(input_weather_dir), winslash = "\\", mustWork = FALSE),
      referenceFilePaths = normalizePath(input_reference_dir, winslash = "\\", mustWork = FALSE),
      paramFile = normalizePath(param_file, winslash = "\\", mustWork = FALSE),
      sowingFile = normalizePath(man_file, winslash = "\\", mustWork = FALSE)
    )
  )

  config_path <- file.path(dirname(exe_path), "FraNchEstYNConfig.json")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)

  cmd <- paste(shQuote(exe_path), shQuote(config_path))

  # random spooky icons for variety
  spooky_icons <- c("🧟", "🦇", "🕷", "🎃", "💀", "👻", "☠",  "🪓", "🩸",     # blood drop    # axe     # trap
                    "🩻" , "🕯","👹" , "🕸", "🧛", "🔪", "📜", "🕸", "💣", "🌖", "🐀")
  start_icon <- sample(spooky_icons, 1)
  end_icon   <- sample(spooky_icons, 1)

  # message depends on mode
  run_label <- if (mode == "calibration") {
    paste("Calibration (", calibrationModel, ")", sep = "")
  } else {
    "Validation"
  }

  message(sprintf(
    "%s FraNchEstYN %s on site %s for %s",
    start_icon,
    run_label,
    paste(sites, collapse = ", "),
    disease
  ))

  # --- run EXE with live streaming -------------------------------------------
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Please install the 'processx' package to stream progress: install.packages('processx')")
  }

  # Extract quoted pieces: exe and args
  exe <- exe_path
  args <- c(config_path)

  p <- processx::process$new(exe, args, stdout = "|", stderr = "|")

  repeat {
    p$poll_io(100L)  # <-- integer ms, not 0.1

    out <- p$read_output()
    if (nzchar(out)) {
      cat(out)        # preserves \r so the line updates in place
      flush.console()
    }

    err <- p$read_error()
    if (nzchar(err)) {
      cat(err, file = stderr())
      flush.console()
    }

    if (!p$is_alive()) {
      # drain any remaining bytes
      out <- p$read_output(); if (nzchar(out)) cat(out)
      err <- p$read_error();  if (nzchar(err)) cat(err, file = stderr())
      break
    }
  }

  message(sprintf(
    "%s Done! Simulation results, performance metrics and parameters are available in the results object.",
    end_icon
  ))


  #OUTPUTS----
  list_files_out <- list.files(paste0(dirname(exe_path),"\\outputs"),
                               pattern = "\\.csv$", full.names = TRUE)

  # read all files in a single dataframe
  outputs_df <- do.call(rbind, lapply(list_files_out, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$file <- basename(f)  # aggiunge colonna con nome file
    df
  }))

  #compute AUDPC
  audpc_calc <- function(severity, time) {
    sum((head(severity, -1) + tail(severity, -1)) / 2 *
          (tail(time, -1) - head(time, -1)), na.rm = TRUE)
  }

  # Compute summaries
  summary_df <- outputs_df |>
    dplyr::group_by(GrowingSeason, Site, Variety) |>
    dplyr::summarise(
      AveTx = mean(Tmax),
      AveTn = mean(Tmin),
      AveRHx = mean(RHmax),
      AveRHn = mean(RHmin),
      TotalPrec = sum(Prec, na.rm = TRUE),
      TotalRad = sum(Rad, na.rm = TRUE),
      TotalLW = sum(LW),
      AUDPC = audpc_calc(DiseaseSeverity*100, DaysAfterSowing),
      DiseaseSeverity = max(DiseaseSeverity),
      YieldAttainable = max(YieldAttainable),
      YieldActual = max(YieldActual),
      YieldLossRaw = YieldAttainable - YieldActual,
      YieldLossPerc = YieldLossRaw/YieldAttainable*100,
      AGBAttainable = max(AGBattainable),
      AGBActual = max(AGBactual),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(YieldLossPerc))

  #flag for no epidemics
  no_epidemic <- !any(summary_df$DiseaseSeverity > 0, na.rm = TRUE)

  #compute error metrics
  # Assuming you already have outputs_df
  metrics_df <- compute_error_metrics(outputs_df)

  # Elimina i file
  file.remove(list_files_out)


  # ---- BUILD RESULT (plots + tables + outputs + updated parameters) -----------
  if (!exists("result")) result <- list()
  result$outputs$simulation <- outputs_df
  result$outputs$summary <- summary_df
  result$diagnostics$metrics <- metrics_df

  #parameter files
  if (calibration != "none") {
    # ---- load calibrated parameter files --------------------------------------
    list_files_out_param <- list.files(
      file.path(dirname(exe_path), "calibratedParameters"),
      pattern = "\\.csv$", full.names = TRUE
    )

    param_df <- if (length(list_files_out_param)) {
      do.call(rbind, lapply(list_files_out_param, function(f) {
        df <- read.csv(f, stringsAsFactors = FALSE)
        df$file <- basename(f)
        df
      }))
    } else data.frame()

    # ---- apply calibrated values back to param lists --------------------------
    update_param_list <- function(param_list, calib_df, clamp = TRUE) {
      if (is.null(param_list) || !length(param_list) || !nrow(calib_df)) return(param_list)
      df <- calib_df
      names(df) <- tolower(names(df))
      pcol <- if ("parameter" %in% names(df)) "parameter" else if ("param" %in% names(df)) "param" else return(param_list)
      vcol <- if ("value" %in% names(df)) "value" else if ("val" %in% names(df)) "val" else return(param_list)

      for (i in seq_len(nrow(df))) {
        pname <- trimws(as.character(df[[pcol]][i]))
        if (!nzchar(pname) || !(pname %in% names(param_list))) next
        v <- suppressWarnings(as.numeric(df[[vcol]][i])); if (is.na(v)) next
        if (clamp) {
          mn <- suppressWarnings(as.numeric(param_list[[pname]]$min))
          mx <- suppressWarnings(as.numeric(param_list[[pname]]$max))
          if (!is.na(mn)) v <- max(v, mn)
          if (!is.na(mx)) v <- min(v, mx)
        }
        param_list[[pname]]$value <- v
      }
      param_list
    }

    updatedCropParameters    <- update_param_list(cropParameters,    param_df)
    updatedDiseaseParameters <- update_param_list(diseaseParameters, param_df)

    ## ---- FLATTEN PARAMETER LISTS (incl. calibration flag) ----------------------
    .is_param_leaf <- function(x)
      is.list(x) && all(c("min","max","value","calibration") %in% names(x))

    flatten_params <- function(x, model) {
      rows <- list()
      walk <- function(node, path = character()) {
        if (.is_param_leaf(node)) {
          nm <- if (length(path)) tail(path, 1) else NA_character_
          rows[[length(rows) + 1]] <<- data.frame(
            Model       = model,
            Parameter   = nm,
            unit        = if (!is.null(node$unit)) node$unit else NA_character_,
            min         = suppressWarnings(as.numeric(node$min)),
            max         = suppressWarnings(as.numeric(node$max)),
            default     = suppressWarnings(as.numeric(node$value)),
            calibration = isTRUE(node$calibration),
            stringsAsFactors = FALSE
          )
        } else if (is.list(node)) {
          nms <- names(node); if (is.null(nms)) nms <- rep(NA_character_, length(node))
          for (i in seq_along(node)) walk(node[[i]], c(path, nms[[i]]))
        }
      }
      walk(x)
      if (!length(rows)) {
        return(data.frame(Model=character(), Parameter=character(), unit=character(),
                          min=numeric(), max=numeric(), default=numeric(),
                          calibration=logical(), stringsAsFactors = FALSE))
      }
      do.call(rbind, rows)
    }

    bounds_crop    <- if (!is.null(cropParameters))    flatten_params(cropParameters,    "crop")    else NULL
    bounds_disease <- if (!is.null(diseaseParameters)) flatten_params(diseaseParameters, "disease") else NULL
    bounds_df <- do.call(rbind, Filter(Negate(is.null), list(bounds_crop, bounds_disease))) |>
      dplyr::group_by(Model,Parameter) |>
      dplyr::slice_head()

    ## ---- LOAD CALIBRATED PARAM CSVs --------------------------------------------
    merged<-bounds_df |>
      dplyr::left_join(param_df ,
                       by=c('Model'="model",
                            'Parameter'= "param")) |>
      dplyr::group_by(Model,Parameter) |>
      dplyr::slice_head()

    # facet label = "Parameter (unit)"
    merged$facet_label <- ifelse(!is.na(merged$unit) & nzchar(merged$unit),
                                 paste0(merged$Parameter, " (", merged$unit, ")"),
                                 merged$Parameter)

    # split
    crop_dat    <- subset(merged, tolower(Model) == "crop")
    disease_dat <- subset(merged, tolower(Model) == "disease")

    ## ---- FACET PLOTS: default vs calibrated (nudged), color = calibration flag ---
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("Package 'ggplot2' is required for plotting. Please install it.")
    if (!requireNamespace("rlang", quietly = TRUE))
      stop("Package 'rlang' is required for tidy-eval in plotting. Please install it.")

    plot_params_facets <- function(df, title) {
      if (!nrow(df)) return(NULL)

      # build a plotting frame with both points: default & calibrated
      # two layers with position nudges to avoid overlap

      ggplot2::ggplot(df, ggplot2::aes(x = 1)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = min,  color = "Min"),  linewidth = 1.5) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = max,  color = "Max"),  linewidth = 1.5) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = default, color = "Default"), linewidth = 1.2) +
        ggplot2::geom_point(
          ggplot2::aes(y = value, color = "Calibration"),
          shape = 16, size = 4, na.rm = TRUE
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(
          name = "Legend",
          values = c("Min" = "black", "Max" = "grey50", "Calibration" = "red", "Default" = "blue"),
          breaks = c("Min", "Max", "Calibration", "Default")
        ) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text.y  = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        ) +
        ggplot2::facet_wrap(~facet_label, scales = "free")


    }

    crop_plot    <- plot_params_facets(crop_dat,    "CROP parameters: calibrated vs default vs bounds")
    disease_plot <- plot_params_facets(disease_dat, "DISEASE parameters: calibrated vs default vs bounds")

      # expose in your final result (no early return)
      if (!exists("result")) result <- list()
      result$diagnostics$calibration <- list(
        tables = list(crop = crop_dat, disease = disease_dat),
        plots  = list(crop = crop_plot, disease = disease_plot)
      )


    # cleanup calibrated param CSVs quietly
    if (length(list_files_out_param)) {
      try(suppressWarnings(file.remove(list_files_out_param)), silent = TRUE)
    }

      # include the (possibly updated) parameter lists so users can reuse them
      result$parameters <- list(
        crop    = updatedCropParameters,
        disease = updatedDiseaseParameters
      )
  }

  # --- OPTIONAL SPOOKY MESSAGE ---------------------------------------------
  if (isTRUE(franchy_message)) {
    try({
      # ---------------- helpers ----------------
      safe_mean  <- function(x, k = 1) round(mean(x, na.rm = TRUE), k)
      safe_round <- function(x, k = 1) if (is.null(x)) NA_real_ else round(x, k)

      # Parse outputs_df$Date to Date (supports "mm/dd/YYYY" and "YYYY-mm-dd")
      .parse_output_date <- function(x) as.Date(x, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))

      # Convert Year+DOY → Date (vectorized)
      .year_doy_to_date <- function(year, doy) {
        y <- suppressWarnings(as.integer(year)); d <- suppressWarnings(as.integer(doy))
        as.Date(d - 1L, origin = sprintf("%04d-01-01", y))
      }

      # Format Date → "mm/dd" (vectorized)
      fmt_mmdd <- function(dates) ifelse(is.na(dates), NA_character_, format(as.Date(dates), "%m/%d"))

      # Find a DOY column (case-insensitive)
      .find_doy_col <- function(df) {
        nms <- names(df)
        i <- which(tolower(nms) %in% c("doy","dayofyear"))[1]
        if (is.na(i)) stop("DOY column not found (expected 'DOY'/'Doy'/'doy' or 'DayOfYear').", call. = FALSE)
        nms[i]
      }

      mmdd_to_date <- function(mmdd, ref_year = 2001) {
        # mmdd: character vector like "10/27" or NA
        x <- ifelse(is.na(mmdd) | mmdd == "", NA_character_, mmdd)
        # turn "10/27" -> "10-27", then paste "2001-10-27"
        y <- paste0(ref_year, "-", sub("^([0-9]{1,2})/([0-9]{1,2})$", "\\1-\\2", x))
        suppressWarnings(as.Date(y, format = "%Y-%m-%d"))
      }

      median_mmdd_from_dates <- function(dates) {
        dates <- as.Date(dates)
        if (!length(dates) || all(is.na(dates))) return(NA_character_)
        doy <- as.integer(strftime(dates, "%j"))
        med <- stats::median(doy, na.rm = TRUE)
        if (is.na(med)) return(NA_character_)
        format(as.Date(med - 1L, origin = "2001-01-01"), "%m/%d")
      }


      # Robust epidemic phases using TRUE calendar dates
      # - If a valid Date column exists, use it
      # - Otherwise, reconstruct Date per row using Year + DOY
      detect_epidemic_phases_dates <- function(df_daily) {
        if (!nrow(df_daily)) {
          return(data.frame(GrowingSeason=integer(), onset_date=character(),
                            rapid_date=character(), peak_date=character(),
                            onset_doy=integer(), rapid_doy=integer(), peak_doy=integer(),
                            peak_sev=numeric(), stringsAsFactors = FALSE))
        }

        # Parse/construct Date
        if ("Date" %in% names(df_daily)) {
          Dcol <- .parse_output_date(df_daily$Date)
        } else {
          Dcol <- as.Date(NA)
        }
        if (all(is.na(Dcol))) {
          doy_col <- .find_doy_col(df_daily)
          if (!all(c("Year", doy_col) %in% names(df_daily))) {
            stop("Need either a valid 'Date' column or both 'Year' and 'DOY' columns.", call. = FALSE)
          }
          Dcol <- .year_doy_to_date(df_daily$Year, df_daily[[doy_col]])
        }

        tmp <- df_daily
        tmp$.Date <- Dcol

        daily <- tmp |>
          dplyr::group_by(GrowingSeason, .Date) |>
          dplyr::summarise(sev = mean(DiseaseSeverity, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(GrowingSeason, .Date)

        phases_base <- daily |>
          dplyr::group_by(GrowingSeason) |>
          dplyr::mutate(
            sev_max  = max(sev, na.rm = TRUE),
            sev_norm = dplyr::if_else(sev_max > 0, sev / sev_max, 0),
            sev_diff = sev - dplyr::lag(sev),
            has_epi  = sev_max > 0
          ) |>
          dplyr::summarise(
            has_epi   = any(has_epi),
            onset_idx = { if (!has_epi[1]) NA_integer_ else { ii <- which(sev_norm >= 0.05); if (length(ii)) ii[1] else NA_integer_ } },
            rapid_idx = { if (!has_epi[1]) NA_integer_ else { dd <- replace(sev_diff, is.na(sev_diff), -Inf); if (all(!is.finite(dd))) NA_integer_ else which.max(dd) } },
            peak_idx  = { if (!has_epi[1]) NA_integer_ else { if (all(is.na(sev))) NA_integer_ else which.max(sev) } },
            peak_sev  = if (!has_epi[1]) 0 else max(sev, na.rm = TRUE) * 100,
            .groups   = "drop"
          )

        # Map indices back to dates/DOYs; return NA when no epidemic
        out <- phases_base |>
          dplyr::rowwise() |>
          dplyr::mutate(
            onset_date = {
              if (!has_epi || is.na(onset_idx)) NA_character_ else {
                s <- daily[daily$GrowingSeason == GrowingSeason, , drop = FALSE]
                if (onset_idx > nrow(s)) NA_character_ else fmt_mmdd(s$.Date[onset_idx])
              }
            },
            rapid_date = {
              if (!has_epi || is.na(rapid_idx)) NA_character_ else {
                s <- daily[daily$GrowingSeason == GrowingSeason, , drop = FALSE]
                if (rapid_idx > nrow(s)) NA_character_ else fmt_mmdd(s$.Date[rapid_idx])
              }
            },
            peak_date  = {
              if (!has_epi || is.na(peak_idx)) NA_character_ else {
                s <- daily[daily$GrowingSeason == GrowingSeason, , drop = FALSE]
                if (peak_idx > nrow(s)) NA_character_ else fmt_mmdd(s$.Date[peak_idx])
              }
            },
            onset_doy = {
              if (!has_epi || is.na(onset_idx)) NA_integer_ else {
                s <- daily[daily$GrowingSeason == GrowingSeason, , drop = FALSE]
                if (onset_idx > nrow(s)) NA_integer_ else as.integer(strftime(s$.Date[onset_idx], "%j"))
              }
            },
            rapid_doy = {
              if (!has_epi || is.na(rapid_idx)) NA_integer_ else {
                s <- daily[daily$GrowingSeason == GrowingSeason, , drop = FALSE]
                if (rapid_idx > nrow(s)) NA_integer_ else as.integer(strftime(s$.Date[rapid_idx], "%j"))
              }
            },
            peak_doy  = {
              if (!has_epi || is.na(peak_idx)) NA_integer_ else {
                s <- daily[daily$GrowingSeason == GrowingSeason, , drop = FALSE]
                if (peak_idx > nrow(s)) NA_integer_ else as.integer(strftime(s$.Date[peak_idx], "%j"))
              }
            }
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-has_epi)

        out
      }



      # First treatment per season as DATE STRING and timing class vs onset
      first_treatment_dates <- function(management_data, seasons, phases_df) {
        tr_cols <- grep("^treatment_\\d+$", names(management_data), value = TRUE, ignore.case = TRUE)
        if (!length(tr_cols)) {
          return(data.frame(GrowingSeason = seasons, first_spray_date = NA_character_, timing_class = NA_character_))
        }
        tr_cols <- tr_cols[order(as.integer(sub("^treatment_(\\d+)$", "\\1", tr_cols)))]

        out <- lapply(seasons, function(yr) {
          row_idx <- integer(0)
          if ("year" %in% names(management_data)) {
            ychar <- as.character(management_data$year)
            row_idx <- which(ychar == as.character(yr))
            if (!length(row_idx)) row_idx <- which(tolower(ychar) == "all")
          }
          if (!length(row_idx)) row_idx <- 1L
          row_idx <- row_idx[1]

          t_doys <- suppressWarnings(as.numeric(unlist(management_data[row_idx, tr_cols, drop = FALSE])))
          t_doys <- t_doys[!is.na(t_doys)]
          if (!length(t_doys)) {
            return(data.frame(GrowingSeason = yr, first_spray_date = NA_character_, timing_class = NA_character_))
          }
          t1_doy <- t_doys[1]
          onset_row <- phases_df[phases_df$GrowingSeason == yr, , drop = FALSE]
          onset_doy <- if (nrow(onset_row)) suppressWarnings(as.numeric(onset_row$onset_doy[1])) else NA_real_

          # classify relative to onset using DOYs (works across winter/spring sowing)
          cls <- if (is.na(onset_doy) || is.na(t1_doy)) NA_character_ else {
            if (t1_doy <  onset_doy) "preventive"
            else if (t1_doy <= onset_doy + 7) "on-time"
            else "late"
          }

          data.frame(
            GrowingSeason    = yr,
            first_spray_date = fmt_mmdd(.year_doy_to_date(yr, t1_doy)),
            timing_class     = cls,
            stringsAsFactors = FALSE
          )
        })
        do.call(rbind, out)
      }

      # Summarize metrics (RMSE/R²/Bias) by variable, using medians across years
      summarize_metrics <- function(metrics_df) {
        if (is.null(metrics_df) || !nrow(metrics_df)) return("not available.")
        nm <- tolower(names(metrics_df))
        vcol_idx <- which(nm %in% c("variable","var","name","metric"))[1]
        if (is.na(vcol_idx)) return("not available.")
        vcol <- names(metrics_df)[vcol_idx]
        num_df <- metrics_df[sapply(metrics_df, is.numeric)]
        if (!ncol(num_df)) return("not available.")
        agg <- stats::aggregate(num_df, list(Variable = metrics_df[[vcol]]),
                                FUN = function(x) suppressWarnings(median(as.numeric(x), na.rm = TRUE)))
        lines <- apply(agg, 1, function(r) {
          var  <- as.character(r[["Variable"]])
          rmse <- suppressWarnings(as.numeric(r[["RMSE"]]))
          r2   <- suppressWarnings(as.numeric(r[["R2"]]))
          bias <- suppressWarnings(as.numeric(r[["Bias"]]))
          if (is.na(rmse) || is.na(r2)) {
            paste0(var, ": insufficient validation signal")
          } else {
            dir <- if (!is.na(bias) && bias > 0) "overestimates" else if (!is.na(bias)) "underestimates" else "bias unclear"
            paste0(var, ": RMSE≈", round(rmse, 2), ", R²≈", round(r2, 2), " (model ", dir, ")")
          }
        })
        paste0("\n• ", paste(lines, collapse = "\n• "))
      }

      # ---------------- overall stats ----------------
      avg_sev   <- safe_round(mean(summary_df$DiseaseSeverity * 100, na.rm = TRUE), 1)
      avg_yield <- safe_round(mean(summary_df$YieldActual, na.rm = TRUE), 0)
      avg_loss  <- safe_round(mean(summary_df$YieldLossPerc, na.rm = TRUE), 1)
      mean_tx   <- safe_mean(summary_df$AveTx)
      mean_tn   <- safe_mean(summary_df$AveTn)
      mean_lw   <- safe_mean(summary_df$TotalLW)
      mean_pr   <- safe_mean(summary_df$TotalPrec)

      year_max  <- summary_df$GrowingSeason[which.max(summary_df$DiseaseSeverity)]
      year_min  <- summary_df$GrowingSeason[which.min(summary_df$DiseaseSeverity)]

      # epidemic phases (dates per season)
      phases <- detect_epidemic_phases_dates(outputs_df)

      # Peak YieldActual per season (date + value)
      peak_yield <- outputs_df |>
        dplyr::mutate(.Date = .parse_output_date(Date)) |>
        dplyr::group_by(GrowingSeason) |>
        dplyr::slice_max(order_by = YieldActual, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          GrowingSeason,
          peak_yield_value = round(as.numeric(YieldActual), 1),
          peak_yield_date  = fmt_mmdd(.Date)
        )

      # sprays: first date + timing class
      seasons  <- sort(unique(summary_df$GrowingSeason))
      spray_df <- first_treatment_dates(management_data, seasons, phases)

      per_year <- summary_df |>
        dplyr::group_by(GrowingSeason) |>
        dplyr::summarise(
          sev_pct  = safe_round(mean(DiseaseSeverity, na.rm = TRUE) * 100, 1),
          y_act    = safe_round(mean(YieldActual,    na.rm = TRUE), 1),
          loss_pct = safe_round(mean(YieldLossPerc,  na.rm = TRUE), 1),
          .groups  = "drop"
        ) |>
        dplyr::left_join(phases[, c("GrowingSeason","onset_date","rapid_date","peak_date","peak_sev")], by = "GrowingSeason") |>
        dplyr::left_join(spray_df[, c("GrowingSeason","first_spray_date","timing_class")], by = "GrowingSeason") |>
        dplyr::left_join(peak_yield, by = "GrowingSeason")


      # median timing across seasons (calendar, no years)
      onset_med_mmdd <- median_mmdd_from_dates(mmdd_to_date(phases$onset_date))
      rapid_med_mmdd <- median_mmdd_from_dates(mmdd_to_date(phases$rapid_date))
      peak_med_mmdd  <- median_mmdd_from_dates(mmdd_to_date(phases$peak_date))

      # season duration from actual Date range
      gs_len <- outputs_df |>
        dplyr::mutate(.Date = .parse_output_date(Date)) |>
        dplyr::group_by(GrowingSeason) |>
        dplyr::summarise(
          start = suppressWarnings(min(.Date, na.rm = TRUE)),
          end   = suppressWarnings(max(.Date, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::mutate(length_days = as.numeric(end - start) + 1)
      dur_med <- safe_round(stats::median(gs_len$length_days, na.rm = TRUE), 0)

      # weather–disease associations (Pearson r)
      yearly_summary <- summary_df |>
        dplyr::group_by(GrowingSeason) |>
        dplyr::summarise(
          lw = mean(TotalLW, na.rm = TRUE),
          tx = mean(AveTx,   na.rm = TRUE),
          tn = mean(AveTn,   na.rm = TRUE),
          rhx = mean(AveRHx, na.rm = T),
          rhn = mean(AveRHn, na.rm = T),
          totalPrec = mean(TotalPrec, na.rm = TRUE),
          avg_severity = mean(DiseaseSeverity, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      do_cor <- function(x, y) {
        ok <- is.finite(x) & is.finite(y)
        if (sum(ok) < 4) return(NA_real_)
        unname(suppressWarnings(cor(x[ok], y[ok], method = "pearson")))
      }
      cors <- data.frame(
        var = c("rainfall", "leaf-wetness", "t max", "t min", "rh max", "rh min"),
        r   = c(
          do_cor(yearly_summary$totalPrec, yearly_summary$avg_severity),
          do_cor(yearly_summary$lw,        yearly_summary$avg_severity),
          do_cor(yearly_summary$tx,        yearly_summary$avg_severity),
          do_cor(yearly_summary$tn,        yearly_summary$avg_severity),
          do_cor(yearly_summary$rhx,        yearly_summary$avg_severity),
          do_cor(yearly_summary$rhn,        yearly_summary$avg_severity)
        )
      )
      block_climate_cor <- if (no_epidemic) {
        "No disease was detected across seasons; weather–disease correlations were not computed."
      } else {
        paste0(
          "Weather–disease associations: ",
          paste(cors$var, "(r=", sprintf("%.2f", cors$r), ")", collapse = "; "),
          "."
        )
      }



      # proportions by class
      timing_tab <- table(factor(spray_df$timing_class, levels = c("preventive","on-time","late")), useNA = "no")
      total_n <- sum(timing_tab)
      pct <- function(n) if (total_n == 0) NA_real_ else round(100 * n / total_n, 1)
      pct_prev <- pct(timing_tab["preventive"])
      pct_on   <- pct(timing_tab["on-time"])
      pct_late <- pct(timing_tab["late"])

      # Count number of treatments per row of a management data.frame
      count_treatments <- function(management_data) {
        tr_cols <- grep("^treatment_\\d+$", names(management_data), value = TRUE, ignore.case = TRUE)
        if (!length(tr_cols)) return(integer(nrow(management_data)))

        apply(management_data[tr_cols], 1, function(row) {
          sum(!is.na(row) & row != "")
        })
      }

      # --- LLM utilities ------------------------------------------------------------
      .llm_enabled <- function(apikey) is.character(apikey) && nzchar(apikey)

      .llm_chat <- function(prompt, apikey, model = "openai/gpt-4o-mini", max_tokens = 300) {
        if (!.llm_enabled(apikey)) return(NA_character_)
        if (!requireNamespace("httr2", quietly = TRUE)) {
          warning("Package 'httr2' is required for LLM features. Falling back to rule-based text.")
          return(NA_character_)
        }
        req <- httr2::request("https://openrouter.ai/api/v1/chat/completions") |>
          httr2::req_headers(
            "Authorization" = paste("Bearer", apikey),
            "Content-Type" = "application/json"
          ) |>
          httr2::req_body_json(list(
            model = model,
            messages = list(
              list(role = "system", content = "You are a concise agrometeorology & plant pathology assistant. Keep outputs short, precise, and non-redundant. Use mm/dd for dates; never use DOY."),
              list(role = "user", content = prompt)
            ),
            max_tokens = max_tokens,
            temperature = 0.2
          ))
        resp <- try(httr2::req_perform(req), silent = TRUE)
        if (inherits(resp, "try-error")) return(NA_character_)
        out <- try(httr2::resp_body_json(resp), silent = TRUE)
        if (inherits(out, "try-error")) return(NA_character_)
        txt <- try(out$choices[[1]]$message$content, silent = TRUE)
        if (inherits(txt, "try-error") || is.null(txt)) return(NA_character_)
        as.character(txt)
      }

      # Compose DISEASE DYNAMICS prompt
      .llm_prompt_disease_dyn <- function(no_epidemic, onset_med_mmdd, rapid_med_mmdd, peak_med_mmdd, dur_med) {
        if (isTRUE(no_epidemic)) {
          sprintf(paste(
            "Write a 2–3 sentence block",
            "Context: No epidemic observed (severity ~0). Typical season length ~%s days.",
            "Style: neutral, scientific, mm/dd only, no DOY; avoid implying 'infection windows'.",
            "Mention that timing benchmarks (onset/peak) will be reported when disease appears.",
            sep = " "), dur_med)
        } else {
          sprintf(paste(
            "Write a 2–3 sentence block",
            "Median timings: onset %s, rapid-rise %s, peak %s; season length ~%s days.",
            "Style: neutral, scientific, mm/dd only, no DOY; avoid redundancy.",
            sep = " "),
            onset_med_mmdd, rapid_med_mmdd, peak_med_mmdd, dur_med)
        }
      }

      # Compose FUNGICIDE block prompt
      .llm_prompt_sprays <- function(no_epidemic, avg_treatments, pct_prev, pct_on, pct_late, onset_med_mmdd, rapid_med_mmdd) {
        if (isTRUE(no_epidemic)) {
          sprintf(paste(
            "Write a 2–3 sentence block",
            "Inputs: average applications = %s; no epidemic observed (onset unavailable).",
            "Guidance: emphasize scouting and weather-based risk; suggest reducing prophylactic sprays.",
            "Style: neutral, scientific, mm/dd only, no DOY.",
            sep = " "),
            ifelse(is.finite(avg_treatments), round(avg_treatments,1), "NA"))
        } else {
          sprintf(paste(
            "Write a 2–3 sentence block",
            "Inputs: average applications=%s; first-spray vs onset distribution: preventive=%s%%, on-time=%s%%, late=%s%%.",
            "Benchmarks: onset≈%s, rapid-rise≈%s (mm/dd).",
            "Provide precise, actionable timing guidance; mm/dd only.",
            sep = " "),
            ifelse(is.finite(avg_treatments), round(avg_treatments,1), "NA"),
            ifelse(is.na(pct_prev), "NA", pct_prev),
            ifelse(is.na(pct_on),   "NA", pct_on),
            ifelse(is.na(pct_late), "NA", pct_late),
            ifelse(is.na(onset_med_mmdd), "NA", onset_med_mmdd),
            ifelse(is.na(rapid_med_mmdd), "NA", rapid_med_mmdd))
        }
      }

      # no_epidemic already computed above
      use_llm <- .llm_enabled(apikey)
      # minimal OpenRouter call with httr2 (optional dependency)
      .llm_generate <- function(apikey, prompt, model = "openai/gpt-4o-mini", max_tokens = 120) {
        if (is.null(apikey) || !nzchar(apikey)) return(NA_character_)
        if (!requireNamespace("httr2", quietly = TRUE)) return(NA_character_)
        url <- "https://openrouter.ai/api/v1/chat/completions"
        req <- httr2::request(url) |>
          httr2::req_headers(
            "Authorization" = paste("Bearer", apikey),
            "HTTP-Referer"  = "https://geomodellab.github.io/FraNchEstYN/",
            "X-Title"       = "FraNchEstYN"
          ) |>
          httr2::req_body_json(list(
            model = model,
            messages = list(list(role="user", content = prompt)),
            max_tokens = max_tokens,
            temperature = 0.7
          ))
        resp <- try(httr2::req_perform(req), silent = TRUE)
        if (inherits(resp, "try-error")) return(NA_character_)
        js <- try(httr2::resp_body_json(resp), silent = TRUE)
        if (inherits(js, "try-error")) return(NA_character_)
        out <- try(js$choices[[1]]$message$content, silent = TRUE)
        if (inherits(out, "try-error") || is.null(out)) return(NA_character_)
        as.character(out)
      }

      get_llm_overview <- function(apikey, stats, no_epidemic) {
        base_prompt <- if (no_epidemic) {
          sprintf(
            "Write 1–2 precise sentences (no emojis) summarizing a crop season set with **no epidemic signal**.
Include mean yield ≈ %s kg/ha, Tx/Tn ≈ %s/%s °C, rainfall ≈ %s mm, and leaf-wetness ≈ %s h.
Avoid implying 'infection windows'. Be concise and scientific.",
            stats$avg_yield, stats$mean_tx, stats$mean_tn, stats$mean_pr, stats$mean_lw)
        } else {
          sprintf(
            "Write 1–2 precise sentences (no emojis) summarizing a crop–disease dataset.
Include avg severity ≈ %s%%, mean yield ≈ %s kg/ha, Tx/Tn ≈ %s/%s °C, rainfall ≈ %s mm, leaf-wetness ≈ %s h.
You may mention 'infection windows' if appropriate. Be concise and scientific.",
            stats$avg_sev, stats$avg_yield, stats$mean_tx, stats$mean_tn, stats$mean_pr, stats$mean_lw)
        }
        .llm_generate(apikey, base_prompt, max_tokens = 90)
      }

      get_llm_proverb <- function(apikey) {
        prompt <- "Invent a single-line, field-savvy proverb about crop disease timing and moisture. 10 words max, no emojis."
        .llm_generate(apikey, prompt, max_tokens = 25)
      }

      # --- DISEASE DYNAMICS via LLM (fallback to rule-based) ---
      if (use_llm) {
        dd_prompt <- .llm_prompt_disease_dyn(
          no_epidemic = no_epidemic,
          onset_med_mmdd = onset_med_mmdd,
          rapid_med_mmdd = rapid_med_mmdd,
          peak_med_mmdd  = peak_med_mmdd,
          dur_med        = dur_med
        )
        dd_txt <- .llm_chat(dd_prompt, apikey, max_tokens = 180)
      }

      block_disease_dyn <- if (!is.na(dd_txt)) {
        dd_txt
      } else if (no_epidemic) {
        paste0(
          "No epidemic signal was detected (severity remained at or near zero). ",
          "A typical growing season spanned ~", dur_med, " days. ",
          "Monitor canopy wetness and night temperatures; if disease pressure emerges in future seasons, ",
          "onset/peak timing will be reported here."
        )
      } else {
        paste0(
          "Epidemics typically began around ", onset_med_mmdd, ", accelerated in ",
          rapid_med_mmdd, ", and peaked near ", peak_med_mmdd, ". ",
          "A typical growing season spanned ~", dur_med, " days."
        )
      }

      # --- FUNGICIDE PROGRAM & TIMING via LLM (fallback to rule-based) ---
      avg_tr_num <- suppressWarnings(mean(count_treatments(management_data)))
      if (use_llm) {
        sp_prompt <- .llm_prompt_sprays(
          no_epidemic       = no_epidemic,
          avg_treatments    = avg_tr_num,
          pct_prev          = pct_prev,
          pct_on            = pct_on,
          pct_late          = pct_late,
          onset_med_mmdd    = onset_med_mmdd,
          rapid_med_mmdd    = rapid_med_mmdd
        )
        sp_txt <- .llm_chat(sp_prompt, apikey, max_tokens = 180)
      }
      block_sprays <- if (!is.na(sp_txt)) {
        sp_txt
      } else if (no_epidemic) {
        paste0(
          "Fungicide program averaged ", round(avg_tr_num,1), " applications. ",
          "Onset was not detected, so timing classification is not applicable. ",
          "Disease pressure was absent; consider reducing or omitting prophylactic sprays and emphasize scouting and weather-based risk monitoring."
        )
      } else {
        paste0(
          "Fungicide program averaged ", round(avg_tr_num,1), " applications. ",
          "First application timing relative to onset: preventive ", pct_prev, "%, on-time ",
          pct_on, "%, late ", pct_late, "%."
        )
      }

      no_epidemic <- !any(summary_df$DiseaseSeverity > 0, na.rm = TRUE)

      avg_treatments <- count_treatments(management_data)

      stats_list <- list(
        avg_sev   = avg_sev,
        avg_yield = avg_yield,
        mean_tx   = mean_tx,
        mean_tn   = mean_tn,
        mean_pr   = mean_pr,
        mean_lw   = mean_lw
      )

      llm_overview <- get_llm_overview(apikey, stats_list, no_epidemic)

      block_overview <- if (isTRUE(no_epidemic)) {
        if (nzchar(llm_overview)) {
          llm_overview
        } else {
          paste0(
            "Across seasons, average disease severity was ", avg_sev, "%, with mean yield around ",
            avg_yield, " kg/ha. Conditions averaged ", mean_tx, "/", mean_tn, " °C, ~",
            mean_pr, " mm rainfall, and ~", mean_lw, " h leaf-wetness. ",
            "No epidemic signal was detected."
          )
        }
      } else {
        if (nzchar(llm_overview)) {
          llm_overview
        } else {
          paste0(
            "Across seasons, average disease severity was ", avg_sev, "%, with mean yield around ",
            avg_yield, " kg/ha. Conditions averaged ", mean_tx, "/", mean_tn, " °C, ~",
            mean_pr, " mm rainfall, and ~", mean_lw, " h leaf-wetness, creating recurrent infection windows."
          )
        }
      }


      timing_advice <- if (no_epidemic) {
        "Disease pressure was absent; consider reducing or omitting prophylactic sprays and emphasize scouting and weather-based risk monitoring."
      } else if (!is.na(pct_prev) && pct_prev >= 60) {
        paste0(
          "Applications were mostly preventive; consider delaying the first spray to just precede onset (≈ ",
          onset_med_mmdd, ") and concentrate coverage during sustained wet-canopy periods."
        )
      } else if (!is.na(pct_late) && pct_late >= 30) {
        paste0(
          "Applications often lagged the epidemic; advance the first spray to just before onset (≈ ",
          onset_med_mmdd, ") and avoid long gaps during the rapid increase window (≈ ", rapid_med_mmdd, ")."
        )
      } else {
        paste0(
          "Timing broadly aligned with onset; keep the first spray just before onset (≈ ",
          onset_med_mmdd, ") and tighten intervals through the rapid rise window (≈ ",
          rapid_med_mmdd, ")."
        )
      }

      # Metrics block
      block_metrics <- paste0("Model–observation performance:", summarize_metrics(metrics_df))

      # Yearly highlights (use mm/dd everywhere)
      per_year <- summary_df |>
        dplyr::group_by(GrowingSeason) |>
        dplyr::summarise(
          sev_pct  = safe_round(mean(DiseaseSeverity, na.rm = TRUE) * 100, 1),
          y_act    = safe_round(mean(YieldActual,    na.rm = TRUE), 1),
          loss_pct = safe_round(mean(YieldLossPerc,  na.rm = TRUE), 1),
          .groups  = "drop"
        ) |>
        dplyr::left_join(phases[, c("GrowingSeason","onset_date","rapid_date","peak_date","peak_sev")], by = "GrowingSeason") |>
        dplyr::left_join(spray_df[, c("GrowingSeason","first_spray_date","timing_class")], by = "GrowingSeason")

      badge_for <- function(gs) {
        if (no_epidemic) "" else if (isTRUE(gs == year_max)) "[highest severity]"
        else if (isTRUE(gs == year_min)) "[lowest severity]" else ""
      }

      fmt_year_line <- function(r) {
        gs   <- r[["GrowingSeason"]]

        on_d <- r[["onset_date"]]
        rr_d <- r[["rapid_date"]]
        pk_d <- r[["peak_date"]]
        pk_s <- suppressWarnings(as.numeric(r[["peak_sev"]]))

        py_d <- r[["peak_yield_date"]]
        py_v <- suppressWarnings(as.numeric(r[["peak_yield_value"]]))

        fs_d <- r[["first_spray_date"]]
        fs_c <- r[["timing_class"]]

        parts <- c(
          sprintf("• %s: %s%% severity; %s kg/ha (%s%% loss).",
                  gs, r[["sev_pct"]], r[["y_act"]], r[["loss_pct"]])
        )

        # disease timing (add only if available)
        if (isTRUE(nzchar(on_d))) parts <- c(parts, paste0(" Onset ", on_d))
        if (isTRUE(nzchar(rr_d))) parts <- c(parts, paste0(", rapid rise ~", rr_d))
        if (isTRUE(nzchar(pk_d))) {
          if (isTRUE(!is.na(pk_s))) {
            parts <- c(parts, paste0(", peak ", pk_d, " (", round(pk_s, 1), "%)"))
          } else {
            parts <- c(parts, paste0(", peak ", pk_d))
          }
        }

        # peak yield (date + value), NA-safe
        if (isTRUE(nzchar(py_d)) && isTRUE(!is.na(py_v))) {
          parts <- c(parts, paste0(". Peak yield ", round(py_v, 1), " kg/ha on ", py_d))
        }

        # first spray
        if (isTRUE(nzchar(fs_d))) {
          if (isTRUE(nzchar(fs_c))) {
            parts <- c(parts, paste0(". First spray ", fs_d, " (", fs_c, ")"))
          } else {
            parts <- c(parts, paste0(". First spray ", fs_d))
          }
        }

        paste0(paste(parts, collapse = ""), " ", badge_for(gs))
      }

      pick_ids <- unique(na.omit(c(
        per_year$GrowingSeason[which.max(per_year$sev_pct)],
        per_year$GrowingSeason[which.min(per_year$sev_pct)]
      )))
      if (length(pick_ids) < 4) {
        remain <- setdiff(per_year$GrowingSeason, pick_ids)
        pick_ids <- c(pick_ids, tail(sort(remain), 4 - length(pick_ids)))
      }
      hl_years <- per_year[match(pick_ids, per_year$GrowingSeason), , drop = FALSE]
      yearly_lines <- if (nrow(hl_years)) vapply(seq_len(nrow(hl_years)), function(i) fmt_year_line(hl_years[i, ]), FUN.VALUE = character(1)) else "• No seasonal highlights."

      # Proverb
      proverb_llm <- get_llm_proverb(apikey)
      proverb <- if (nzchar(proverb_llm)) proverb_llm else sample(c(
        "Spray before the rise, harvest before the fall.",
        "Where wet nights linger, rust remembers.",
        "A timely dose saves a season.",
        "Moisture writes what management must read."
      ), 1)


      # ---------------- message ----------------
      msg <- paste(
        "\n==================== 📊 FraNchEstYN Decision-support diagnostic 📊 ====================\n",
        "🧐 OVERVIEW\n", block_overview, "\n\n",
        "🌧️ WEATHER–DISEASE ASSOCIATIONS\n", block_climate_cor, "\n\n",
        "🍄 DISEASE DYNAMICS (CALENDAR DATES)\n", block_disease_dyn, "\n\n",
        "⚕️ FUNGICIDE PROGRAM & TIMING\n", block_sprays, "\n", timing_advice, "\n\n",
        "🤖 MODEL PERFORMANCE\n", block_metrics, "\n\n",
        "📅 YEARLY HIGHLIGHTS\n", paste(yearly_lines, collapse = "\n"), "\n\n",
        "💬 PROVERB\n", proverb, "\n",
        "======================================================================================\n",
        sep = ""
      )

      cat(msg)
      result$decision_support_message <- msg
    }, silent = TRUE)
  }

  # ---- RETURN EVERYTHING -------------------------------------------------------
  return(result)
}

#' Compute error metrics for model vs reference variables (internal)
#'
#' @param outputs_df data.frame with sim & ref columns
#' @param group_cols character vector of grouping columns
#' @return tibble with one row per group/variable
#' @keywords internal
#' Compute error metrics for model vs reference variables (internal)
#'
#' @param outputs_df data.frame with simulation and reference columns
#' @param group_cols character vector of grouping columns
#' @return tibble with one row per group/variable
#' @keywords internal
compute_error_metrics <- function(outputs_df,
                                  group_cols = c("GrowingSeason", "Site", "Variety", "file")) {

  pairs <- list(
    LightInterception = c("LightInterception", "LightInterceptionRef"),
    DisSev            = c("DiseaseSeverity",            "DiseaseSeverityRef"),
    YieldAttainable             = c("YieldAttainable",             "YieldAttainableRef"),
    YieldActual             = c("YieldActual",             "YieldActualRef")
  )

  present_pairs <- Filter(function(x) all(x %in% names(outputs_df)), pairs)

  # Remove pairs with no non-NA reference values
  present_pairs <- Filter(function(cols) {
    any(is.finite(suppressWarnings(as.numeric(outputs_df[[cols[2]]]))))
  }, present_pairs)

  if (length(present_pairs) == 0) {
    return(dplyr::tibble())  # empty table if no usable pairs
  }

  metrics_vec <- function(sim, ref) {
    sim <- suppressWarnings(as.numeric(sim))
    ref <- suppressWarnings(as.numeric(ref))
    ok  <- is.finite(sim) & is.finite(ref)
    sim <- sim[ok]; ref <- ref[ok]
    n <- length(sim)
    if (n == 0) {
      return(list(n = 0, Bias = NA_real_, MAE = NA_real_, RMSE = NA_real_,
                  r = NA_real_, R2 = NA_real_, NSE = NA_real_))
    }
    e    <- sim - ref
    Bias <- mean(e)
    MAE  <- mean(abs(e))
    RMSE <- sqrt(mean(e^2))
    nz   <- abs(ref) > 0
    denom <- abs(sim) + abs(ref)
    sm_ok <- denom > 0
    r  <- if (n > 1 && stats::sd(sim) > 0 && stats::sd(ref) > 0) stats::cor(sim, ref) else NA_real_
    R2 <- if (is.finite(r)) r^2 else NA_real_
    denom_nse <- sum((ref - mean(ref))^2)
    NSE <- if (denom_nse > 0) 1 - sum(e^2) / denom_nse else NA_real_
    list(n = n, Bias = Bias, MAE = MAE, RMSE = RMSE,
         r = r, R2 = R2, NSE = NSE)
  }

  compute_for_pair <- function(df, pair_name, sim_col, ref_col) {
    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::reframe({
        m <- metrics_vec(.data[[sim_col]], .data[[ref_col]])
        tibble::tibble(
          Variable = pair_name,
          n = m$n, Bias = m$Bias, MAE = m$MAE, RMSE = m$RMSE,
          r = m$r, R2 = m$R2, NSE = m$NSE
        )
      })
  }

  out <- lapply(names(present_pairs), function(v) {
    cols <- present_pairs[[v]]
    compute_for_pair(outputs_df, v, cols[1], cols[2])
  })

  dplyr::bind_rows(out)
}


#' Validate parameter lists: ensure min < max
#'
#' @param ... Named lists of parameters (e.g. cropParameters, diseaseParameters, fungicideParameters)
#' @return TRUE (invisible) if all checks pass, otherwise stops with an error
#' @keywords internal
validate_parameter_ranges <- function(...) {
  param_sets <- list(...)

  errors <- list()

  for (set_name in names(param_sets)) {
    plist <- param_sets[[set_name]]
    if (is.null(plist)) next

    for (pname in names(plist)) {
      p <- plist[[pname]]
      if (!is.list(p)) next

      minv <- tryCatch(p$min, error=function(e) NA)
      maxv <- tryCatch(p$max, error=function(e) NA)

      if (is.numeric(minv) && is.numeric(maxv) && length(minv)==1 && length(maxv)==1) {
        if (!is.na(minv) && !is.na(maxv) && minv >= maxv) {
          errors[[length(errors)+1]] <- paste0(
            "In ", set_name, ": parameter '", pname,
            "' has min=", minv, " higher than max=", maxv, ". Please correct."
          )
        }
      }
    }
  }

  if (length(errors)) {
    stop(paste(errors, collapse="\n"), call.=FALSE)
  }

  invisible(TRUE)
}

