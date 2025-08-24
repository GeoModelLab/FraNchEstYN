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
#' @param cropParameters A named list of crop parameters
#'   (see \code{data(cropParameters)}).
#'
#' @param diseaseParameters A named list of disease parameters
#'   (see \code{data(diseaseParameters)}).
#'
#' @param fungicideParameters Optional list of fungicide parameters
#'   (see \code{data(fungicideParameters)}).
#'
#' @param start_end Numeric vector of length 2. Start and end years for simulation
#'   (default: \code{c(2000, 2025)}).
#'
#' @param out_folder output folder path
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
#' franchestyn_batch(
#'   weather_data        = weather_df,
#'   management_data     = mgmt_df,
#'   cropParameters      = cropParameters$wheat,
#'   diseaseParameters   = diseaseParameters$septoria,
#'   fungicideParameters = fungicideParameters$protectant,
#'   start_end           = c(2010, 2020),
#'   iterations          = 200,
#'   outputDir          = "your_output_folder"
#' )
#' }
#' @export
franchestyn_batch <- function(weather_data, management_data,
                        cropParameters = NULL, diseaseParameters = NULL,
                        fungicideParameters = NULL,
                        start_end = c(2000,2025),
                        ...,
                        outputDir)# whether to request spooky LLM summary)
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
  calibration <-'none'
  disease     <- 'thisDisease'


  # Determine mode
  mode <- if (calibration == "none") "simulation" else "calibration"

  # Detect calibration model
  calibrationModel <- "none"



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
      isCalibration = 'false',
      calibrationVariable = "none",
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
  # Ensure outputs directory exists


  list_files_out <- list.files(paste0(dirname(exe_path),"\\outputs"),
                               pattern = "\\.csv$", full.names = TRUE)

  # read all files in a single dataframe
  outputs_df <- do.call(rbind, lapply(list_files_out, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$file <- basename(f)  # aggiunge colonna con nome file
    df
  }))

  # Ensure outputDir exists
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  }
  write.csv(outputs_df,file = paste0(outputDir,"//",sites,"_daily.csv"),row.names = F)

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

  write.csv(summary_df,file = paste0(outputDir,"//",sites,"_summary.csv"),row.names = F)

  # Elimina i file
  invisible(file.remove(list_files_out))

}
