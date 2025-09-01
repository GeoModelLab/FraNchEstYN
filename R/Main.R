#' Run the FraNchEstYN crop–disease simulation or calibration
#'
#' Prepares inputs (weather, management, crop model, parameters, reference) and
#' runs the FraNchEstYN model executable. Users never manage paths or config
#' files manually.
#'
#' FraNchEstYN can operate standalone (with its internal crop growth routines)
#' or be coupled with \strong{any external crop model} by supplying
#' \code{cropModel_data}. In that case, crop growth dynamics (e.g. biomass,
#' intercepted radiation, yield) come directly from the external model, while
#' FraNchEstYN handles disease progression, fungicide programs, and yield loss.
#'
#' Supports flexible management inputs, default or user-specified fungicide
#' parameterizations, and optional AI-based commentary in different
#' communication styles (\code{"scientist"}, \code{"extensionist"},
#' \code{"farmer"}).
#'
#' @param weather_data A data frame of daily or hourly weather for \strong{one site}.
#'   Input frequency is auto-detected. Column names are matched case-insensitively,
#'   ignoring spaces, underscores, and dashes.
#'
#'   \strong{Date columns (mandatory):}
#'   \itemize{
#'     \item Daily: \code{year}, \code{month}, \code{day}.
#'     \item Hourly: additionally \code{hour}.
#'   }
#'
#'   \strong{Meteorological variables:}
#'   \itemize{
#'     \item Daily (mandatory): max temp (\code{tmax}, \code{tx}, etc.), min temp (\code{tmin}, \code{tn}, etc.),
#'           precipitation (\code{precipitation}, \code{rain}, etc.).
#'     \item Hourly (mandatory): air temp (\code{temp}, \code{t2m}, etc.), precipitation.
#'   }
#'
#'   \strong{Radiation/Latitude (at least one):}
#'   \itemize{
#'     \item Radiation (\code{rad}, \code{solar}, etc.) [MJ m\eqn{^{-2}} d\eqn{^{-1}}]
#'     \item Latitude (\code{lat}, \code{latitude}) [decimal degrees]
#'   }
#'   Radiation will be estimated if missing but latitude is supplied.
#'
#'   \strong{Optional:}
#'   \itemize{
#'     \item Relative humidity (daily \code{rhmax}/\code{rhmin}, or hourly \code{rh})
#'     \item Leaf wetness (computed internally from RH>90\% or rain ≥ 0.2 mm h\eqn{^{-1}})
#'   }
#'
#' @param management_data Optional data frame of management operations
#'   (for the same site as \code{weather_data}). Column names are normalized
#'   to snake_case. Required if fungicide schedules are used.
#'
#'   \strong{Required columns:}
#'   \itemize{
#'     \item \code{crop} — e.g. "Wheat".
#'     \item \code{sowingDOY} — integer day-of-year.
#'     \item \code{year} — ISO year (YYYY) or \code{"All"}.
#'   }
#'
#'   \strong{Optional:}
#'   \itemize{
#'     \item \code{treatment_1}, \code{treatment_2}, … — DOY of fungicide sprays.
#'   }
#'
#' @param cropModel_data Optional crop-model output used instead of internal crop
#'   growth simulation. If supplied:
#'   \itemize{
#'     \item Disease calibration is enforced automatically.
#'     \item \code{cropParameters} are ignored.
#'   }
#'
#'   Required columns: \code{year}, \code{doy}, \code{agb}, \code{yield},
#'   and either \code{fint} or \code{lai}.
#'
#'   Optional: \code{gdd} / \code{thermal_time}.
#'
#' @param reference_data Optional data frame with observed values.
#'   Required if \code{calibration != "none"}. Column names are normalized.
#'
#'   \strong{For disease calibration:} must include a disease severity column
#'   named \code{DiseaseSeverity}, \code{dissev}, or \code{disease}, with
#'   values in \eqn{[0,1]}.
#'
#'   Recommended keys: \code{year}, \code{doy}.
#'
#' @param cropParameters Nested list of crop parameters. Required unless
#'   \code{cropModel_data} is supplied.
#'
#' @param diseaseParameters Nested list of disease parameters (always required).
#'
#' @param fungicideParameters Optional list of fungicide parameters.
#'   If omitted, defaults to \code{fungicideParameters$protectant}.
#'
#' @param calibration What to calibrate. One of:
#'   \itemize{
#'     \item \code{"none"} — run only.
#'     \item \code{"crop"} — calibrate crop parameters.
#'     \item \code{"disease"} — calibrate disease parameters.
#'     \item \code{"all"} — calibrate both.
#'   }
#'   If \code{cropModel_data} is provided, this is forced to \code{"disease"}.
#'
#' @param start_end Numeric length-2. Start and end years for simulation
#'   (default \code{c(2000, 2025)}).
#'
#' @param k Extinction coefficient for Beer’s law conversion of LAI→fInt
#'   (default 0.6).
#'
#' @param iterations Integer. Number of Monte Carlo runs (default 100).
#'   Can also be set via \code{...}.
#'
#' @param personality Character. Style of AI-generated commentary:
#'   \code{"scientist"}, \code{"extensionist"}, or \code{"farmer"}.
#'
#' @param apikey Optional API key string for enabling LLM commentary
#'   (via \url{https://openrouter.ai/}).
#'
#' @param franchy_message Logical. If \code{TRUE}, generates a diagnostic
#'   commentary block. If \code{apikey} is valid, LLM-based text is used,
#'   otherwise rule-based summaries are returned.
#'
#'
#' @details
#' - One site per run.
#' - Column matching is forgiving (case-insensitive, ignores underscores/dashes).
#' - If \code{calibration="none"} and \code{reference_data} is missing,
#'   a dummy dataset is created for compatibility.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item \code{outputs} — simulated time series.
#'     \item \code{diagnostics} — calibration diagnostics.
#'     \item \code{parameters} — parameter sets used.
#'     \item \code{decision_support_message} — optional persona-driven report.
#'   }
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
#'   personality         = "farmer",
#'   iterations          = 200
#' )
#' }
#' @export
franchestyn <- function(weather_data,
                        cropModel_data = NULL,
                        management_data = NULL,
                        reference_data = NULL,
                        cropParameters = NULL,
                        diseaseParameters = NULL,
                        fungicideParameters = NULL,
                        calibration  = 'disease',
                        start_end = c(2000,2025),
                        k = 0.6,
                        iterations=100,# extinction coefficient for LAI→fInt
                        apikey = NULL,                 # optional API key
                        franchy_message = FALSE,
                        personality = c("scientist", "extensionist", "farmer"))  # NEW...)# whether to request spooky LLM summary)
{
  # Always coerce to character first
  personality <- match.arg(as.character(personality),
                           choices = c("scientist", "extensionist", "farmer"))

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

  # --- Detect calibration model ---
  if (!is.null(cropModel_data)) {
    if (tolower(calibration) == "none") {
      # crop model but user explicitly said no calibration
      calibrationModel <- "none"
      message("🌱 cropModel_data detected, sowing years/DOYs inferred from cycles")
    } else {
      calibrationModel <- "disease"
      calibration <- "disease"  # force disease mode
      message("🌱 cropModel_data detected, sowing years/DOYs inferred from cycles")
    }
  } else {
    if (tolower(calibration) %in% c("crop","all")) {
      calibrationModel <- ifelse(tolower(calibration) == "crop", "crop", "All")
    } else if (grepl("^disease(:.*)?$", trimws(calibration), ignore.case = TRUE)) {
      calibrationModel <- "disease"
    } else {
      calibrationModel <- "none"
    }
  }

  # --- Only run calibration block if not "none" ---
  if (calibrationModel != "none") {
    # your calibration code block here
  }


  # --- Check requirements for calibration ---
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
        year    = start_end[1],
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

  ## ------------------- CROP MODEL HANDLING ------------------- ##
  crop_model_dir <- file.path(dirname(exe_path), "files", "cropModel")
  dir.create(crop_model_dir, recursive = TRUE, showWarnings = FALSE)

  cropModel_flag <- "no"

  if (!is.null(cropModel_data)) {
    if (!is.data.frame(cropModel_data)) {
      stop("❌ 'cropModel_data' must be a data.frame if provided.")
    }

    # Normalizer (like in C#)
    normalize <- function(x) tolower(gsub("[ _-]", "", x))
    nms <- normalize(names(cropModel_data))

    # --- required column mapping ---
    year_col <- names(cropModel_data)[nms %in% c("year","yr")][1]
    doy_col  <- names(cropModel_data)[nms %in% c("doy","dayofyear","dy","d")][1]
    agb_col  <- names(cropModel_data)[nms %in% c("agb","abovegroundbiomass","biomass","wtop")][1]
    yld_col  <- names(cropModel_data)[nms %in% c("yield","yieldattainable","yieldpotential","wgrn","grainyieldpotential")][1]
    fint_col <- names(cropModel_data)[nms %in% c("fint","f_int","lightinterception","lightint","lightinterception")][1]
    lai_col  <- names(cropModel_data)[nms %in% c("lai","leafareaindex")][1]
    gdd_col  <- names(cropModel_data)[nms %in% c("gdd","thermaltime","thermal_time")][1]

    if (is.null(year_col) || is.null(doy_col)) stop("❌ cropModel_data must contain 'year' and 'doy' columns")
    if (is.null(agb_col) || is.null(yld_col)) stop("❌ cropModel_data must contain both 'AGB' and 'Yield' columns")
    if (is.null(fint_col) && is.null(lai_col))stop("❌ cropModel_data must contain either 'fInt' or 'LAI' column")

    # --- LAI → fInt conversion (Beer’s law) ---
    if (is.null(fint_col) && !is.null(lai_col)) {
      cropModel_data$fInt <- 1 - exp(-k * cropModel_data[[lai_col]])
      fint_col <- "fInt"
      message(sprintf("🌱 Converted LAI to fInt using k = %.2f", k))
    }

    # --- build Date from Year + DOY ---
    cropModel_data$Date <- as.Date(
      cropModel_data[[doy_col]] - 1,
      origin = paste0(cropModel_data[[year_col]], "-01-01")
    )

    # --- standardize columns ---
    cm <- data.frame(
      Date  = cropModel_data$Date,
      Year  = as.integer(cropModel_data[[year_col]]),
      DOY   = as.integer(cropModel_data[[doy_col]]),
      fInt  = as.numeric(cropModel_data[[fint_col]]),
      AGB   = as.numeric(cropModel_data[[agb_col]]),
      Yield = as.numeric(cropModel_data[[yld_col]]),
      stringsAsFactors = FALSE
    )

    # optional GDD / ThermalTime
    has_gdd <- FALSE
    if (!is.na(gdd_col)) {cm$GDD <- as.numeric(cropModel_data[[gdd_col]])
      has_gdd <- TRUE
      message("📈 Included GDD/ThermalTime column.")}

    # --- detect cycles (sowing jumps or harvest resets) ---
    dates <- sort(unique(cm$Date))
    cycles <- list()
    cycle_start <- min(dates)

    for (i in seq(2, length(dates))) {
      prev <- dates[i-1]; curr <- dates[i]

      # detect DOY jump not due to year wrap
      doy_backwards <- as.integer(format(curr, "%j")) < as.integer(format(prev, "%j"))
      year_wrap <- format(prev, "%m") == "12" && format(curr, "%m") == "01"
      sowing_jump <- doy_backwards && !year_wrap

      # detect harvest reset (yield drops >100 → <=100)
      yPrev <- cm$Yield[cm$Date == prev][1]
      yCurr <- cm$Yield[cm$Date == curr][1]
      harvest_reset <- !is.na(yPrev) && !is.na(yCurr) && (yCurr <= 100 && yPrev > 100)

      if (sowing_jump || harvest_reset) {
        cycles[[length(cycles)+1]] <- c(cycle_start, prev)
        cycle_start <- curr
      }
    }
    cycles[[length(cycles)+1]] <- c(cycle_start, max(dates))  # close final cycle

    # --- compute cycle percentage ---
    cm$cyclePercentage <- NA_real_

    for (cyc in cycles) {
      start <- as.Date(cyc[1]); end <- as.Date(cyc[2])
      idx <- cm$Date >= start & cm$Date <= end
      if (!any(idx)) next

      if (has_gdd) {
        gdd_vals <- cm$GDD[idx]
        gdd_max <- suppressWarnings(max(gdd_vals, na.rm = TRUE))
        if (is.finite(gdd_max) && gdd_max > 0) {
          cm$cyclePercentage[idx] <- gdd_vals / gdd_max * 100
        }
      } else {
        total_days <- as.numeric(end - start)
        if (total_days > 0) {
          cm$cyclePercentage[idx] <- as.numeric(difftime(cm$Date[idx], start, units = "days")) /
            total_days * 100
        }
      }
    }

    # --- write file ---
    out_crop_file <- file.path(crop_model_dir, "cropModelData.csv")
    write.table(cm, out_crop_file,
                sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

    cropModel_flag <- "yes"
  }


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

  if(length(sites)>1){
    stop("💀 weather_data' must contain a single site!")
  }

  # ---- Fallback for fungicides ----
  if (is.null(fungicideParameters)) {
    if (exists("fungicideParameters", envir = asNamespace("FraNchEstYN"))) {
      pkg_fungicides <- get("fungicideParameters", envir = asNamespace("FraNchEstYN"))
      fungicideParameters <- pkg_fungicides$protectant
    }
  }

  # ---- Check Parameters ----
  if (is.null(cropModel_data)) {
    # Standard case → need crop & disease parameters
    if (!is.list(cropParameters)) stop("🕸 'cropParameters' must be a nested list.")
    if (!is.list(diseaseParameters)) stop("🕸 'diseaseParameters' must be a nested list.")

    validate_parameter_ranges(
      cropParameters      = cropParameters,
      diseaseParameters   = diseaseParameters,
      fungicideParameters = fungicideParameters
    )

  } else {
    # Crop model provided → cropParameters ignored
    if (!is.null(cropParameters)) {
      cropParameters <- NULL
    }
    if (!is.list(diseaseParameters)) stop("🕸 'diseaseParameters' must be a nested list.")

    validate_parameter_ranges(
      diseaseParameters   = diseaseParameters,
      fungicideParameters = fungicideParameters
    )
  }

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

    # If calibration = "none" and reference_data is NULL → just return NULL
    if (calib == "none" && is.null(reference_data)) {
      if (isTRUE(verbose)) {
        message("ℹ️ Calibration = 'none' and no reference_data supplied → skipping checks.")
      }
      return(NULL)
    }

    if (is.null(reference_data)) {
      stop(sprintf("reference_data must be provided when calibration = '%s'.", calib),
           call. = FALSE)
    }

    # Trim spaces from column names
    names(reference_data) <- trimws(names(reference_data))

    # Accepted aliases (case-insensitive)
    disease_aliases <- c("diseaseseverity", "dissev", "disease")
    year_aliases    <- c("year")
    doy_aliases     <- c("doy")

    nms <- names(reference_data)
    low <- tolower(nms)

    # ---------------------------
    # Check YEAR column (always required)
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

    # Check DOY column (always required)
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

    if (calib %in% c("crop", "none")) {
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

  # required columns for C#
  if (!is.null(reference_data)) {
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
  }


  ##-- MANAGEMENT DATA --##
  man_file <- file.path(input_management_dir, "sowing.csv")

  management_user <- management_data  # copy raw

  if (is.null(cropModel_data)) {
    # ------------------ STANDARD CASE ------------------
    if (is.null(management_user)) {
      stop("❌ Either 'management_data' or 'cropModel_data' must be provided.")
    }

    names(management_user) <- tolower(gsub("\\s+", "_", names(management_user)))

    # required cols
    required <- c("crop", "year")
    missing_req <- setdiff(required, names(management_user))
    if (length(missing_req)) {
      stop("Missing required column(s): ", paste(missing_req, collapse = ", "))
    }

    # site assignment
    if (!"site" %in% names(management_user)) {
      management_user$site <- sites[1]
    }
    if (!"variety" %in% names(management_user)) {
      management_user$variety <- "All"
    }

    # sowing DOY (optional)
    if (!"sowingdoy" %in% names(management_user)) {
      management_user$sowingdoy <- NA_integer_
    } else {
      management_user$sowingdoy <- as.integer(management_user$sowingdoy)
    }

    # year cleanup
    management_user$year <- trimws(as.character(management_user$year))
    is_all   <- tolower(management_user$year) == "all"
    is_yyyy  <- grepl("^\\d{4}$", management_user$year)
    yr_num   <- suppressWarnings(as.integer(management_user$year))
    yr_num[!is_yyyy] <- NA_integer_
    if (!any(is_all, na.rm = TRUE)) {
      management_user$year <- yr_num
    }

  } else {
    # --- derive sowings directly from cropModel cycles ---
    sowings <- lapply(cycles, function(cyc) {
      start <- as.Date(cyc[1])
      data.frame(
        site      = sites,
        crop      = "wheat",
        variety   = "Generic",
        sowingdoy = as.integer(strftime(start, "%j")),
        year      = as.integer(format(start, "%Y")),
        stringsAsFactors = FALSE
      )
    })
    management_auto <- do.call(rbind, sowings)

    # 👇 merge user treatments if provided
    if (!is.null(management_data) && "treatment" %in% names(management_data)) {
      message("💊 Using fungicide treatments from user-supplied management_data.")

      names(management_data) <- tolower(gsub("\\s+", "_", names(management_data)))

      # Keep only year + treatment
      tr_in <- management_data[, c("year", "treatment"), drop = FALSE]

      # Merge by year → treatments appear only where year matches
      management_auto <- merge(
        management_auto,
        tr_in,
        by = "year",
        all.x = TRUE
      )
    }

    management_user <- management_auto
  }

  # ---- detect and parse treatments ----
  treatment_cols <- character(0)
  if (!is.null(management_user) && "treatment" %in% names(management_user)) {
    fallback_year <- 2021L

    parse_doy <- function(tok, yr) {
      tok <- gsub("[-./]", " ", tok)
      tok <- gsub("\\s+", " ", trimws(tok))
      for (fmt in c("%d %b %Y", "%b %d %Y", "%d %B %Y", "%B %d %Y", "%d %b", "%b %d")) {
        d <- as.Date(paste(tok, yr), format = fmt)
        if (!is.na(d)) return(as.integer(strftime(d, "%j")))
      }
      NA_integer_
    }

    is_all <- tolower(management_user$year) == "all"

    for (i in seq_len(nrow(management_user))) {
      cell <- management_user$treatment[[i]]
      if (is.null(cell) || !nzchar(cell)) next
      toks <- trimws(unlist(strsplit(as.character(cell), "[,;]+")))
      toks <- toks[nzchar(toks)]
      if (!length(toks)) next

      ref_year <- if (is_all[i]) fallback_year else suppressWarnings(as.integer(management_user$year[i]))
      if (is.na(ref_year)) ref_year <- fallback_year

      doys <- vapply(toks, parse_doy, integer(1L), yr = ref_year)
      doys <- doys[!is.na(doys)]

      for (k in seq_along(doys)) {
        col <- paste0("treatment_", k)
        if (!col %in% names(management_user)) {
          management_user[[col]] <- NA_real_
        }
        management_user[[col]][i] <- as.numeric(doys[k])
      }
    }

    # keep track of treatment columns if created
    treatment_cols <- grep("^treatment_\\d+$", names(management_user), value = TRUE)
    if (length(treatment_cols)) {
      tr_order <- order(as.integer(sub("^treatment_(\\d+)$", "\\1", treatment_cols)))
      treatment_cols <- treatment_cols[tr_order]
    }

    # drop raw text col
    management_user$treatment <- NULL
  }

  # ---- final selection & order ----
  keep_cols <- c("site", "crop", "variety", "sowingdoy", "year", treatment_cols)
  management_user <- management_user[, keep_cols, drop = FALSE]

  # ---- write sowing.csv ----
  write.table(
    management_user,
    file = man_file,
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ""
  )

  management_data <- management_user

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
      cropModel = ifelse(is.null(cropModel_data), "no", "yes"),
      simplexes = 3,
      disease = disease,
      iterations = iterations,
      weatherTimeStep = tolower(timestep)
    ),
    paths = list(
      weatherDir        = normalizePath(dirname(input_weather_dir), winslash = "\\", mustWork = FALSE),
      referenceFilePaths= normalizePath(input_reference_dir, winslash = "\\", mustWork = FALSE),
      paramFile         = normalizePath(param_file, winslash = "\\", mustWork = FALSE),
      sowingFile        = normalizePath(man_file, winslash = "\\", mustWork = FALSE),
      cropModelFile     = if (!is.null(cropModel_data))
        normalizePath(crop_model_dir, winslash = "\\", mustWork = FALSE)
      else ""
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

    ## ---- packages (fail early if missing) -----------------------------------
    if (!requireNamespace("dplyr", quietly = TRUE))
      stop("Package 'dplyr' is required. Please install it.")
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("Package 'ggplot2' is required. Please install it.")

    ## ---- load calibrated parameter files ------------------------------------
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

    # normalize names (lowercase) once
    if (nrow(param_df)) names(param_df) <- tolower(names(param_df))

    ## ---- deep copy originals BEFORE updating --------------------------------
    deep_copy <- function(x) unserialize(serialize(x, NULL))
    origCropParameters    <- if (exists("cropParameters"))    deep_copy(cropParameters)    else NULL
    origDiseaseParameters <- if (exists("diseaseParameters")) deep_copy(diseaseParameters) else NULL

    ## ---- apply calibrated values (do NOT touch originals) -------------------
    update_param_list <- function(param_list, calib_df, clamp = TRUE) {
      if (is.null(param_list) || !length(param_list) || !nrow(calib_df)) return(param_list)

      # figure out columns present in CSVs
      pcol <- if ("parameter" %in% names(calib_df)) "parameter" else if ("param" %in% names(calib_df)) "param" else return(param_list)
      vcol <- if ("value" %in% names(calib_df)) "value"       else if ("val" %in% names(calib_df))   "val"   else return(param_list)

      for (i in seq_len(nrow(calib_df))) {
        pname <- trimws(as.character(calib_df[[pcol]][i]))
        if (!nzchar(pname) || !(pname %in% names(param_list))) next

        v <- suppressWarnings(as.numeric(calib_df[[vcol]][i]))
        if (is.na(v)) next

        if (clamp) {
          mn <- suppressWarnings(as.numeric(param_list[[pname]]$min))
          mx <- suppressWarnings(as.numeric(param_list[[pname]]$max))
          if (!is.na(mn)) v <- max(v, mn)
          if (!is.na(mx)) v <- min(v, mx)
        }
        # overwrite current value with calibrated (originals are in orig*)
        param_list[[pname]]$value <- v
      }
      param_list
    }

    updatedCropParameters    <- update_param_list(cropParameters,    param_df)
    updatedDiseaseParameters <- update_param_list(diseaseParameters, param_df)

    # If we're calibrating 'disease' only, ignore crop parameters
    if (tolower(calibrationModel) == "disease") {
      updatedCropParameters <- NULL
    }

    ## ---- flatten helpers ----------------------------------------------------
    .is_param_leaf <- function(x) {
      is.list(x) && all(c("min","max","value","calibration") %in% names(x))
    }

    # Flatten a parameter tree into a table with a single numeric column called 'value'
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
            value       = suppressWarnings(as.numeric(node$value)),
            calibration = isTRUE(node$calibration),
            stringsAsFactors = FALSE
          )
        } else if (is.list(node)) {
          nms <- names(node); if (is.null(nms)) nms <- rep(NA_character_, length(node))
          for (i in seq_along(node)) walk(node[[i]], c(path, nms[[i]]))
        }
      }
      if (is.null(x)) {
        return(data.frame(Model=character(), Parameter=character(), unit=character(),
                          min=numeric(), max=numeric(), value=numeric(),
                          calibration=logical(), stringsAsFactors = FALSE))
      }
      walk(x)
      if (!length(rows)) {
        return(data.frame(Model=character(), Parameter=character(), unit=character(),
                          min=numeric(), max=numeric(), value=numeric(),
                          calibration=logical(), stringsAsFactors = FALSE))
      }
      do.call(rbind, rows)
    }

    ## ---- build defaults (original) & calibrated (updated) -------------------
    defaults_crop    <- flatten_params(origCropParameters,         "crop")
    defaults_disease <- flatten_params(origDiseaseParameters,      "disease")
    calib_crop       <- flatten_params(updatedCropParameters,      "crop")
    calib_disease    <- flatten_params(updatedDiseaseParameters,   "disease")

    # rename their 'value' columns to default / calibrated
    if (nrow(defaults_crop))    names(defaults_crop)   [names(defaults_crop)   == "value"] <- "default"
    if (nrow(defaults_disease)) names(defaults_disease)[names(defaults_disease)== "value"] <- "default"
    if (nrow(calib_crop))       names(calib_crop)      [names(calib_crop)      == "value"] <- "calibrated"
    if (nrow(calib_disease))    names(calib_disease)   [names(calib_disease)   == "value"] <- "calibrated"

    # join defaults + calibrated on common keys
    join_two <- function(a, b) {
      dplyr::full_join(a, b, by = c("Model","Parameter","unit","min","max","calibration"))
    }

    bounds_list <- list(
      if (nrow(defaults_crop) || nrow(calib_crop))          join_two(defaults_crop,    calib_crop)    else NULL,
      if (nrow(defaults_disease) || nrow(calib_disease))    join_two(defaults_disease, calib_disease) else NULL
    )
    bounds_df <- dplyr::bind_rows(Filter(Negate(is.null), bounds_list))

    ## ---- optionally merge CSV columns (if present) --------------------------
    # only do the join if param_df has the needed columns
    if (nrow(param_df) && all(c("model","param") %in% names(param_df))) {
      merged <- dplyr::left_join(bounds_df, param_df, by = c("Model"="model","Parameter"="param"))
    } else {
      merged <- bounds_df
    }

    # ensure unique per Model-Parameter (keep first if duplicates)
    merged <- dplyr::group_by(merged, Model, Parameter)
    merged <- dplyr::slice_head(merged, n = 1)
    merged <- dplyr::ungroup(merged)

    # facet label = "Parameter (unit)"
    merged$facet_label <- ifelse(!is.na(merged$unit) & nzchar(merged$unit),
                                 paste0(merged$Parameter, " (", merged$unit, ")"),
                                 merged$Parameter)

    # split for downstream compatibility
    crop_dat    <- subset(merged, tolower(Model) == "crop")
    disease_dat <- subset(merged, tolower(Model) == "disease")

    ## ---- plotting function (define BEFORE using it) -------------------------
    plot_params_facets <- function(df, title) {
      if (!nrow(df)) return(NULL)

      # build a flag column for plotting
      df$point_color <- ifelse(df$calibration, "Calibration", "Default")

      ggplot2::ggplot(df, ggplot2::aes(x = 1)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = min,     color = "Min"),     linewidth = 1.2, na.rm = TRUE) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = max,     color = "Max"),     linewidth = 1.2, na.rm = TRUE) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = default, color = "Default"), linewidth = 1.2, na.rm = TRUE) +
        ggplot2::geom_point(
          ggplot2::aes(y = calibrated, color = point_color),
          shape = 16, size = 3, na.rm = TRUE
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(
          name = "Legend",
          values = c(
            "Min"         = "black",
            "Max"         = "grey50",
            "Default"     = "blue",
            "Calibration" = "red"
          ),
          breaks = c("Min","Max","Default","Calibration")
        ) +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text.y  = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        ) +
        ggplot2::facet_wrap(~facet_label, scales = "free")
    }


    ## ---- build plots once ---------------------------------------------------
    crop_plot    <- plot_params_facets(crop_dat,    "CROP parameters: default vs calibrated")
    disease_plot <- plot_params_facets(disease_dat, "DISEASE parameters: default vs calibrated")

    ## ---- expose in result ---------------------------------------------------
    if (!exists("result")) result <- list()
    result$diagnostics$calibration <- list(
      tables = list(crop = crop_dat, disease = disease_dat),
      plots  = list(crop = crop_plot, disease = disease_plot)
    )

    ## ---- cleanup calibrated param CSVs quietly ------------------------------
    if (length(list_files_out_param)) {
      try(suppressWarnings(file.remove(list_files_out_param)), silent = TRUE)
    }

    ## ---- return updated parameter lists for reuse ---------------------------
    result$parameters <- list(
      crop    = updatedCropParameters,   # will be NULL if disease-only calibration
      disease = updatedDiseaseParameters
    )
  }


  # --- OPTIONAL SPOOKY MESSAGE ---------------------------------------------
  if (isTRUE(franchy_message)) {
    tryCatch({
      # ---------------- personality setup ----------------
      personality <- match.arg(personality, choices = c("scientist","extensionist","farmer"))

      system_role <- switch(personality,
                            "scientist"    = "You are a concise agrometeorology & plant pathology scientist. Neutral, precise, scientific. Use mm/dd dates, no DOY, no emojis.",
                            "extensionist" = "You are a farm advisor. Be clear, supportive, and practical. Use mm/dd dates, no jargon, no emojis.",
                            "farmer"       = "You are a seasoned farmer. Use simple words, warm tone, and memorable phrasing. Use mm/dd dates, no DOY, no emojis."
      )

      # ---------------- helpers ----------------
      safe_mean  <- function(x, k = 1) round(mean(x, na.rm = TRUE), k)
      safe_round <- function(x, k = 1) if (is.null(x)) NA_real_ else round(x, k)
      .parse_output_date <- function(x) as.Date(x, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
      .year_doy_to_date  <- function(year, doy) {
        y <- suppressWarnings(as.integer(year)); d <- suppressWarnings(as.integer(doy))
        as.Date(d - 1L, origin = sprintf("%04d-01-01", y))
      }
      fmt_mmdd <- function(dates) ifelse(is.na(dates), NA_character_, format(as.Date(dates), "%m/%d"))
      .find_doy_col <- function(df) {
        nms <- names(df)
        i <- which(tolower(nms) %in% c("doy","dayofyear"))[1]
        if (is.na(i)) stop("DOY column not found.", call. = FALSE)
        nms[i]
      }

      mmdd_to_date <- function(mmdd, ref_year = 2001) {
        x <- ifelse(is.na(mmdd) | mmdd == "", NA_character_, mmdd)
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

      phases <- detect_epidemic_phases_dates(outputs_df)
      seasons  <- sort(unique(summary_df$GrowingSeason))
      spray_df <- first_treatment_dates(management_data, seasons, phases)

      onset_med_mmdd <- median_mmdd_from_dates(mmdd_to_date(phases$onset_date))
      rapid_med_mmdd <- median_mmdd_from_dates(mmdd_to_date(phases$rapid_date))
      peak_med_mmdd  <- median_mmdd_from_dates(mmdd_to_date(phases$peak_date))

      gs_len <- outputs_df |>
        dplyr::mutate(.Date = .parse_output_date(Date)) |>
        dplyr::group_by(GrowingSeason) |>
        dplyr::summarise(start = min(.Date, na.rm = TRUE), end = max(.Date, na.rm = TRUE), .groups="drop") |>
        dplyr::mutate(length_days = as.numeric(end - start) + 1)
      dur_med <- safe_round(stats::median(gs_len$length_days, na.rm = TRUE), 0)

      no_epidemic <- !any(summary_df$DiseaseSeverity > 0, na.rm = TRUE)
      avg_tr_num <- suppressWarnings(mean(apply(spray_df, 1, function(x) sum(!is.na(x)))))

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
      .llm_enabled <- function(apikey) {is.character(apikey) && nzchar(apikey)}

      # personality mapper → textual style
      .llm_personality_style <- function(persona) {
        switch(tolower(persona[1]),
               "scientist"    = "neutral, scientific, concise, with technical precision",
               "extensionist" = "practical, advisory, farmer-facing tone; clear but non-technical",
               "farmer"       = "informal, field-wise tone with plain language",
               "neutral, scientific" # fallback
        )
      }

      # build system role for chat
      .llm_system_role <- function(persona) {
        switch(tolower(persona[1]),
               "scientist"    = "You are a concise agrometeorology & plant pathology scientist. Neutral, precise, scientific. Use mm/dd dates, no DOY, no emojis.",
               "extensionist" = "You are a farm advisor. Be clear, supportive, and practical. Use mm/dd dates, no jargon, no emojis.",
               "farmer"       = "You are a seasoned farmer. Use simple words, warm tone, and memorable phrasing. Use mm/dd dates, no DOY, no emojis.",
               "You are a neutral assistant. Use mm/dd dates, no DOY, no emojis."
        )
      }

      # generic chat wrapper
      .llm_chat <- function(prompt,
                            apikey,
                            persona = "scientist",
                            model = "openai/gpt-4o-mini",
                            max_tokens = 300,
                            temperature = 0.3) {
        if (!.llm_enabled(apikey)) return(NA_character_)
        if (!requireNamespace("httr2", quietly = TRUE)) {
          warning("Package 'httr2' is required for LLM features. Falling back to rule-based text.")
          return(NA_character_)
        }

        system_role <- .llm_system_role(persona)

        req <- httr2::request("https://openrouter.ai/api/v1/chat/completions") |>
          httr2::req_headers(
            "Authorization" = paste("Bearer", apikey),
            "Content-Type"  = "application/json"
          ) |>
          httr2::req_body_json(list(
            model = model,
            messages = list(
              list(role = "system", content = system_role),
              list(role = "user",   content = prompt)
            ),
            max_tokens  = max_tokens,
            temperature = temperature
          ))

        resp <- try(httr2::req_perform(req), silent = TRUE)
        if (inherits(resp, "try-error")) return(NA_character_)
        out <- try(httr2::resp_body_json(resp), silent = TRUE)
        if (inherits(out, "try-error")) return(NA_character_)
        txt <- try(out$choices[[1]]$message$content, silent = TRUE)
        if (inherits(txt, "try-error") || is.null(txt)) return(NA_character_)
        as.character(txt)
      }


      # --- Prompt builders ----------------------------------------------------------

      .llm_prompt_disease_dyn <- function(no_epidemic,
                                          onset_med_mmdd,
                                          rapid_med_mmdd,
                                          peak_med_mmdd,
                                          dur_med,
                                          persona = "scientist") {
        style <- .llm_personality_style(persona)
        if (isTRUE(no_epidemic)) {
          sprintf("Write a 2–3 sentence block. Context: no epidemic observed (severity≈0). Typical season length ~%s days.
Style: %s. Use mm/dd only, no DOY.",
                  dur_med, style)
        } else {
          sprintf("Write a 2–3 sentence block. Median timings: onset %s, rapid-rise %s, peak %s; season length ~%s days.
Style: %s. Use mm/dd only, no DOY.",
                  onset_med_mmdd, rapid_med_mmdd, peak_med_mmdd, dur_med, style)
        }
      }

      .llm_prompt_sprays <- function(no_epidemic,
                                     avg_treatments,
                                     pct_prev,
                                     pct_on,
                                     pct_late,
                                     onset_med_mmdd,
                                     rapid_med_mmdd,
                                     persona = "scientist") {
        style <- .llm_personality_style(persona)
        if (isTRUE(no_epidemic)) {
          sprintf("Write a 2–3 sentence block. Inputs: average applications=%s; no epidemic observed (onset unavailable).
Guidance: emphasize scouting and weather-based risk; suggest reducing prophylactic sprays.
Style: %s. Use mm/dd only, no DOY.",
                  ifelse(is.finite(avg_treatments), round(avg_treatments, 1), "NA"),
                  style)
        } else {
          sprintf("Write a 2–3 sentence block. Inputs: average applications=%s; first-spray vs onset: preventive=%s%%, on-time=%s%%, late=%s%%.
Benchmarks: onset≈%s, rapid-rise≈%s (mm/dd).
Provide timing guidance. Style: %s. Use mm/dd only, no DOY.",
                  ifelse(is.finite(avg_treatments), round(avg_treatments, 1), "NA"),
                  ifelse(is.na(pct_prev), "NA", pct_prev),
                  ifelse(is.na(pct_on),   "NA", pct_on),
                  ifelse(is.na(pct_late), "NA", pct_late),
                  ifelse(is.na(onset_med_mmdd), "NA", onset_med_mmdd),
                  ifelse(is.na(rapid_med_mmdd), "NA", rapid_med_mmdd),
                  style)
        }
      }

      # --- Persona-aware section headers --------------------------------------------
      .section_headers <- function(persona) {
        switch(tolower(persona[1]),
               "scientist" = list(
                 overview   = "🧐 OVERVIEW",
                 weather    = "🌧️ WEATHER–DISEASE ASSOCIATIONS",
                 dynamics   = "🍄 DISEASE DYNAMICS (CALENDAR DATES)",
                 sprays     = "⚕️ FUNGICIDE PROGRAM & TIMING",
                 metrics    = "🤖 MODEL PERFORMANCE",
                 yearly     = "📅 YEARLY HIGHLIGHTS",
                 proverb    = "💬 PROVERB"
               ),
               "extensionist" = list(
                 overview   = "📋 ADVISORY NOTES",
                 weather    = "🌦️ WEATHER & DISEASE LINK",
                 dynamics   = "🌱 CROP & DISEASE CYCLE",
                 sprays     = "💊 SPRAY PROGRAM",
                 metrics    = "📊 MODEL CHECK",
                 yearly     = "📅 FIELD SEASON HIGHLIGHTS",
                 proverb    = "💬 FIELD WISDOM"
               ),
               "farmer" = list(
                 overview   = "🌱 FIELD NOTES",
                 weather    = "🌦️ WEATHER & DISEASE",
                 dynamics   = "🍂 SEASON’S DISEASE STORY",
                 sprays     = "💉 SPRAYING PRACTICES",
                 metrics    = "🔍 HOW THE MODEL DID",
                 yearly     = "📅 SEASON-BY-SEASON HIGHLIGHTS",
                 proverb    = "💬 FARM SAYING"
               ),
               # fallback
               list(
                 overview   = "🧐 OVERVIEW",
                 weather    = "🌧️ WEATHER–DISEASE ASSOCIATIONS",
                 dynamics   = "🍄 DISEASE DYNAMICS (CALENDAR DATES)",
                 sprays     = "⚕️ FUNGICIDE PROGRAM & TIMING",
                 metrics    = "🤖 MODEL PERFORMANCE",
                 yearly     = "📅 YEARLY HIGHLIGHTS",
                 proverb    = "💬 PROVERB"
               )
        )
      }

      # --- WEATHER–DISEASE ASSOCIATIONS ---------------------------------------------
      .llm_prompt_weather_assoc <- function(cors, no_epidemic, persona="scientist") {
        style <- .llm_personality_style(persona)

        if (isTRUE(no_epidemic)) {
          sprintf("Write 1–2 sentences. Context: no epidemic detected, so weather–disease correlations are negligible.
Style: %s. Be concise, mm/dd dates if needed, no DOY.",
                  style)
        } else {
          cors_text <- paste(cors$var, "(r=", sprintf("%.2f", cors$r), ")", collapse = "; ")
          sprintf("Write 1–2 sentences summarizing weather–disease associations.
Inputs: %s.
Style: %s. Be concise, no DOY.",
                  cors_text, style)
        }
      }

      get_llm_weather_assoc <- function(apikey, cors, no_epidemic, persona="scientist") {
        prompt <- .llm_prompt_weather_assoc(cors, no_epidemic, persona)
        .llm_chat(prompt, apikey, persona=persona, max_tokens=120)
      }

      # --- MODEL PERFORMANCE / METRICS ----------------------------------------------
      .llm_prompt_metrics <- function(metrics_df, persona="scientist") {
        style <- .llm_personality_style(persona)

        if (is.null(metrics_df) || !nrow(metrics_df)) {
          sprintf("Write 1 short sentence: validation metrics not available. Style: %s.", style)
        } else {
          # Extract a human-readable string
          nm <- tolower(names(metrics_df))
          vcol_idx <- which(nm %in% c("variable","var","name","metric"))[1]
          vcol <- if (!is.na(vcol_idx)) names(metrics_df)[vcol_idx] else "Variable"

          num_df <- metrics_df[sapply(metrics_df, is.numeric)]
          agg <- stats::aggregate(num_df, list(Variable = metrics_df[[vcol]]),
                                  FUN = function(x) suppressWarnings(median(as.numeric(x), na.rm=TRUE)))

          lines <- apply(agg, 1, function(r) {
            var  <- as.character(r[["Variable"]])
            rmse <- suppressWarnings(as.numeric(r[["RMSE"]]))
            r2   <- suppressWarnings(as.numeric(r[["R2"]]))
            bias <- suppressWarnings(as.numeric(r[["Bias"]]))
            if (is.na(rmse) || is.na(r2)) {
              paste0(var, ": insufficient validation signal")
            } else {
              dir <- if (!is.na(bias) && bias > 0) "overestimates"
              else if (!is.na(bias)) "underestimates" else "bias unclear"
              paste0(var, ": RMSE≈", round(rmse,2), ", R²≈", round(r2,2), " (model ", dir, ")")
            }
          })
          metrics_text <- paste(lines, collapse = "; ")

          sprintf("Write a 1–2 sentence summary of model–observation performance.
Inputs: %s.
Style: %s. Be concise, no DOY.",
                  metrics_text, style)
        }
      }

      get_llm_weather_assoc <- function(apikey, cors, no_epidemic, persona="scientist") {
        prompt <- .llm_prompt_weather_assoc(cors, no_epidemic, persona)
        .llm_chat(prompt, apikey, persona=persona, max_tokens=120)
      }


      get_llm_metrics <- function(apikey, metrics_df, persona="scientist") {
        prompt <- .llm_prompt_metrics(metrics_df, persona)
        .llm_chat(prompt, apikey, persona=persona, max_tokens=120)
      }

      get_llm_overview <- function(apikey, stats, no_epidemic, persona="scientist") {
        style <- .llm_personality_style(persona)
        base_prompt <- if (no_epidemic) {
          sprintf("Write 1–2 sentences (no emojis). Dataset: no epidemic signal.
Include mean yield≈%s kg/ha, Tx/Tn≈%s/%s °C, rainfall≈%s mm, leaf-wetness≈%s h.
Style: %s.",
                  stats$avg_yield, stats$mean_tx, stats$mean_tn, stats$mean_pr, stats$mean_lw, style)
        } else {
          sprintf("Write 1–2 sentences (no emojis). Dataset: crop–disease dataset.
Include avg severity≈%s%%, mean yield≈%s kg/ha, Tx/Tn≈%s/%s °C, rainfall≈%s mm, leaf-wetness≈%s h.
Style: %s.",
                  stats$avg_sev, stats$avg_yield, stats$mean_tx, stats$mean_tn, stats$mean_pr, stats$mean_lw, style)
        }
        .llm_chat(base_prompt, apikey, persona = persona, max_tokens = 90)
      }

      get_llm_proverb <- function(apikey, persona="scientist") {
        style <- .llm_personality_style(persona)
        prompt <- sprintf("Invent a single-line, field-savvy proverb about crop disease timing and moisture.
Max 10 words, no emojis. Style: %s.", style)
        .llm_chat(prompt, apikey, persona = persona, max_tokens = 25)
      }


      # --- Overview / proverb -------------------------------------------------------
      get_llm_overview <- function(apikey, stats, no_epidemic, persona="scientist") {
        style <- .llm_personality_style(persona)

        base_prompt <- if (no_epidemic) {
          sprintf(
            "Write 1–2 sentences (no emojis). Dataset: no epidemic signal.
Include mean yield≈%s kg/ha, Tx/Tn≈%s/%s °C, rainfall≈%s mm, leaf-wetness≈%s h.
Style: %s.",
            stats$avg_yield, stats$mean_tx, stats$mean_tn,
            stats$mean_pr, stats$mean_lw, style
          )
        } else {
          sprintf(
            "Write 1–2 sentences (no emojis). Dataset: crop–disease dataset.
Include avg severity≈%s%%, mean yield≈%s kg/ha, Tx/Tn≈%s/%s °C, rainfall≈%s mm, leaf-wetness≈%s h.
Style: %s.",
            stats$avg_sev, stats$avg_yield, stats$mean_tx, stats$mean_tn,
            stats$mean_pr, stats$mean_lw, style
          )
        }

        .llm_chat(base_prompt, apikey, persona = persona, max_tokens = 90)
      }

      # WEATHER–DISEASE ASSOCIATIONS
      llm_weather <- get_llm_weather_assoc(apikey, cors, no_epidemic, persona = personality)
      block_climate_cor <- if (nzchar(llm_weather)) {
        llm_weather
      } else if (no_epidemic) {
        "No disease was detected across seasons; weather–disease correlations were not computed."
      } else {
        paste0("Weather–disease associations: ",
               paste(cors$var, "(r=", sprintf("%.2f", cors$r), ")", collapse="; "),
               ".")
      }

      # MODEL PERFORMANCE
      llm_metrics <- get_llm_metrics(apikey, metrics_df, persona = personality)
      block_metrics <- if (nzchar(llm_metrics)) {
        llm_metrics
      } else {
        paste0("Model–observation performance:", summarize_metrics(metrics_df))
      }

      # --- DISEASE DYNAMICS via LLM (fallback to rule-based) ---

        dd_prompt <- .llm_prompt_disease_dyn(
          no_epidemic = no_epidemic,
          onset_med_mmdd = onset_med_mmdd,
          rapid_med_mmdd = rapid_med_mmdd,
          peak_med_mmdd  = peak_med_mmdd,
          dur_med        = dur_med
        )
        dd_txt <- .llm_chat(dd_prompt, apikey,persona = personality, max_tokens = 180)


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

        sp_prompt <- .llm_prompt_sprays(
          no_epidemic       = no_epidemic,
          avg_treatments    = avg_tr_num,
          pct_prev          = pct_prev,
          pct_on            = pct_on,
          pct_late          = pct_late,
          onset_med_mmdd    = onset_med_mmdd,
          rapid_med_mmdd    = rapid_med_mmdd
        )
        sp_txt <- .llm_chat(sp_prompt, apikey, persona = personality, max_tokens = 180)

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

      llm_overview <- get_llm_overview(apikey, persona = personality, stats_list, no_epidemic)

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

      # --- In report assembly -------------------------------------------------------
      headers <- .section_headers(personality)

      # ---------------- Final message with persona headers ----------------
      msg <- paste(
        "\n==================== 📊 FraNchEstYN Decision-support diagnostic 📊 ====================\n",
        headers$overview, "\n",   block_overview,  "\n\n",
        headers$weather,  "\n",   block_climate_cor,"\n\n",
        headers$dynamics, "\n",   block_disease_dyn,"\n\n",
        headers$sprays,   "\n",   block_sprays, "\n", timing_advice, "\n\n",
        headers$metrics,  "\n",   block_metrics, "\n\n",
        headers$yearly,   "\n",   paste(yearly_lines, collapse="\n"), "\n\n",
        headers$proverb,  "\n",   proverb, "\n",
        "======================================================================================\n",
        sep = ""
      )

      cat(msg)
      result$decision_support_message <- msg
    })
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

