#' Run the FraNchEstYN crop–disease simulation or calibration
#'
#' Runs the FraNchEstYN C# model from R. The function prepares inputs
#' (weather, management, parameters, reference), writes them next to the
#' package executable, builds a JSON config, launches the EXE, and streams
#' live console output (RMSE updates overwrite on one line). Users do not
#' manage paths or config files manually.
#'
#' @param weather_data A data frame for **one site only** (the function stops if
#'   multiple sites are present). A `Site` column (case-insensitive) is required.
#'   Date fields must exist via any of these aliases (case-insensitive; spaces,
#'   underscores and dashes ignored):
#'   \itemize{
#'     \item Year: \code{year}, \code{yr}, \code{yyyy}
#'     \item Month: \code{month}, \code{mo}, \code{mn}
#'     \item Day: \code{day}, \code{dy}, \code{dd}
#'     \item Hour (only if hourly): \code{hour}, \code{hr}, \code{hh}
#'   }
#'   The time step is **auto-detected** from column names: if any of
#'   \code{hour}, \code{hours}, \code{hourly}, \code{hr}, \code{hod} is present
#'   (case/space/underscore-insensitive), the model assumes **hourly**;
#'   otherwise **daily**. Other meteorological variables are passed through as-is.
#'
#' @param management_data A data frame with management information for the same
#'   site. Must include columns: \code{crop}, \code{variety}, \code{resistance},
#'   \code{sowingDOY}, \code{year}. The function adds a \code{site} column
#'   internally and writes \code{sowing.csv} for the EXE.
#'
#' @param reference_data Optional data frame with observed data, **required when**
#'   \code{calibration != "none"}. Column requirements (case-insensitive; spaces,
#'   underscores and dashes ignored) follow the C# loader:
#'   \describe{
#'     \item{Required date & key fields}{
#'       \itemize{
#'         \item \strong{Variety}: one of \code{variety}, \code{cultivar}, \code{cv}
#'         \item \strong{Year}: one of \code{year}, \code{yr}
#'         \item \strong{DOY}: one of \code{doy}, \code{day_of_year}, \code{dy}, \code{d}
#'       }
#'       Date is reconstructed as \code{as.Date(paste0(year, "-01-01")) + doy}.
#'     }
#'     \item{Optional measurements (read if present; empty or \code{"NA"} are skipped)}{
#'       \itemize{
#'         \item \strong{Light interception (fint)}: \code{fint}, \code{f_int},
#'               \code{lightInterception}, \code{lightint}, \code{lightinterception}
#'         \item \strong{Grain yield}: \code{yield}, \code{grain_yield}, \code{yld}, \code{wgrn}
#'         \item \strong{Above-ground biomass (AGB)}: \code{agb},
#'               \code{above_ground_biomass}, \code{biomass}, \code{abovegroundbiomass}, \code{wtop}
#'       }
#'     }
#'     \item{Disease severity (required if \code{calibration} is \code{"disease"},
#'       \code{"disease:NAME"}, or \code{"all"})}{
#'       A column matching the \code{disease} argument, or
#'       \code{"<disease>_sev"}, \code{"<disease>_severity"}.
#'     }
#'   }
#'
#' @param cropParameters A nested list of crop parameters (typically from
#'   \code{FraNchEstYN::cropParameters}, e.g. \code{cropParameters$Wheat}).
#'   Each parameter is a list with fields: \code{description}, \code{unit},
#'   \code{min}, \code{max}, \code{value}, \code{calibration} (logical).
#'
#' @param diseaseParameters A nested list of disease parameters (e.g.
#'   \code{diseaseParameters$Septoria}). Same structure as \code{cropParameters}.
#'
#' @param calibration Character; target of the run. One of:
#'   \code{"none"}, \code{"crop"}, \code{"disease"}, \code{"disease:NAME"}, \code{"all"}.
#'   Matching is case-insensitive; \code{"disease:NAME"} overrides the \code{disease}
#'   argument with \code{NAME}.
#'
#' @param disease Character; disease name to simulate/calibrate (default
#'   \code{"Septoria"}). If disease calibration is requested, a column with this
#'   name (or its \code{_sev}/\code{_severity} variants) must exist in
#'   \code{reference_data}.
#'
#' @param start_end Numeric length-2 vector: start and end years
#'   (default \code{c(2000, 2025)}). Must satisfy \code{start_end[1] <= start_end[2]}.
#'
#' @param ... Advanced options (not shown to typical users). Currently:
#'   \code{iterations} (integer; default 100). Used only when
#'   \code{calibration != "none"}; otherwise ignored.
#'
#' @details
#' - The function enforces a single site in \code{weather_data}.
#' - Column name checks are case-insensitive and tolerant to spaces/underscores/dashes.
#' - Input files are written under the installed package’s \code{inst/bin} tree near
#'   \code{FraNchEstYN.exe} (weather, parameters, reference, management).
#' - A JSON config is generated and passed to the EXE.
#' - Previous CSV outputs in the \code{outputs} folder are removed before running.
#' - Console output from the EXE is streamed live; progress lines use \code{\r} to
#'   update in place, and the run ends with a newline + summary message.
#'
#' @return (Invisibly) a list with metadata (detected \code{timestep}, \code{mode},
#'   \code{calibrationModel}, \code{site}, paths used, output directory). Model result
#'   files are produced by the EXE in \code{outputs/}.
#'
#' @examples
#' \dontrun{
#' franchestyn(
#'   weather_data      = weather_df,      # one site; Year/Month/Day (+ Hour if hourly)
#'   management_data   = mgmt_df,         # crop/variety/resistance/sowingDOY/year
#'   reference_data    = ref_df,          # required when calibrating
#'   cropParameters    = cropParameters$Wheat,
#'   diseaseParameters = diseaseParameters$Septoria,
#'   calibration       = "all",
#'   disease           = "Septoria",
#'   start_end         = c(2010, 2020),
#'   iterations        = 200              # via ...
#' )
#' }
#' @export
franchestyn <- function(weather_data, management_data, reference_data = NULL,
                        cropParameters = NULL, diseaseParameters = NULL,
                        calibration  = 'disease',
                        start_end = c(2000,2025),
                        apikey = NULL,                 # optional API key
                        franchy_message = FALSE,
                        ...)# whether to request spooky LLM summary)
{
  #VALIDATION----

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


  # cat("🔍 Git root:", pkg_path, "\n")
  # cat("⚙️ Executable found at:", exe_path, "\n")


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
  #TODO: uncomment
  #exe_path <- system.file("bin", "FraNchEstYN.exe", package = "FraNchEstYN")
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

  # ---- normalize/validate columns when calibration == "disease" ----
  # Create a numeric column named exactly `disease_name`
  # using values from a known disease severity alias.
  prepare_reference_data <- function(reference_data,
                                     calibration,
                                     disease_name = "thisDisease",
                                     verbose = TRUE) {
    # Normalize calibration
    calib <- tolower(trimws(as.character(calibration)))

    # Only run when calibration is 'disease' or 'all'
    # if (!(calib %in% c("disease", "all"))) {
    #   return(reference_data)
    # }

    # Trim spaces from column names
    names(reference_data) <- trimws(names(reference_data))

    # Accepted aliases (case-insensitive)
    disease_aliases <- c("diseaseseverity", "dissev", "disease")

    nms <- names(reference_data)
    low <- tolower(nms)

    # Find first matching alias
    hit <- match(disease_aliases, low, nomatch = 0)
    hit <- hit[hit > 0]

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

    # Rename the first match
    idx <- hit[1]
    old_name <- nms[idx]
    names(reference_data)[idx] <- disease_name

    reference_data
  }


  ref_file <- file.path(input_reference_dir, "referenceData.csv")

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

  man_file <- file.path(input_management_dir,"sowing.csv")
  management_data$site<-sites
  management_data<-management_data |>
    dplyr::select(site,crop,variety,resistance,sowingDOY,year)
  # Write the synthetic file
  write.table(
    management_data,
    file = man_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  # WRITE WEATHER FILE ----
  # Handle 'all' sites logic
  # Validate required columns
  if (!"Site" %in% names(weather_data)) stop("Missing 'Site' column in weather_data")

  site_names <- sites  # This will be used in the JSON config

  # WRITE PARAMETERS FILE----
  # Detect a leaf parameter node
  # helper: detect a leaf node
  # helper: detect a leaf node
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

  param_file <- file.path(input_parameters_dir, "franchestynParameters.csv")
  readr::write_csv(rbind(params_crop_df,params_disease_df),param_file)

  # --- WEATHER FILE WRITING ---
  # Aliases for date/time columns
  year_aliases  <- c("year", "yr", "yyyy")
  month_aliases <- c("month", "mo", "mn")
  day_aliases   <- c("day", "dy", "dd")
  hour_aliases  <- c("hour", "hr", "hh")

    nms <- tolower(names(weather_data))

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
      simplexes = 1,
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
                    "🩻" , "🕯","👹" , "🕸")
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

  # Leggi tutti i file in una lista di data.frame
  # Leggi e combina in un unico data frame
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

    # ---- build bounds table (with unit + Model) --------------------------------
    ## ---- FLATTEN PARAMETER LISTS (incl. calibration flag) ----------------------
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
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = min, color = "Min"),
          linewidth = 1.5
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = max, color = "Max"),
          linewidth = 1.5
        ) +
        # Calibration value
        ggplot2::geom_point(
          ggplot2::aes(y = value, color = "Calibration", shape = "Calibration"),
          size = 4, na.rm = TRUE
        ) +
        # Default value
        ggplot2::geom_point(
          ggplot2::aes(y = default, color = "Default", shape = "Default"),
          size = 2.8, na.rm = TRUE
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_color_manual(
          name = "Legend",
          values = c(
            "Min" = "black",
            "Max" = "grey50",
            "Calibration" = "red",
            "Default" = "blue"
          ),
          breaks = c("Min", "Max", "Calibration", "Default")
        ) +
        ggplot2::scale_shape_manual(
          name = "Legend",
          values = c(
            "Calibration" = 16,  # solid circle
            "Default" = 2        # triangle
          ),
          breaks = c("Calibration", "Default")
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
    if (is.null(apikey)) {
      warning("☠️ Franchy message requested but no API key provided — skipping.")
    } else {
      try({
        library(httr)
        library(jsonlite)

        # Prepare year-by-year summary
        yearly_summary <- summary_df %>%
          dplyr::group_by(GrowingSeason) %>%
          dplyr::summarise(
            avg_severity = round(mean(DiseaseSeverity, na.rm = TRUE) * 100, 1),
            yield_attainable = round(mean(YieldAttainable, na.rm = TRUE), 1),
            yield_actual = round(mean(YieldActual, na.rm = TRUE), 1),
            yield_loss = round(mean(YieldLossPerc, na.rm = TRUE), 1),
            .groups = "drop"
          )

        # Turn into a text block for the prompt
        yearly_text <- paste0(
          apply(yearly_summary, 1, function(row) {
            paste0(
              "Year ", row[["GrowingSeason"]], ": ",
              row[["avg_severity"]], "% severity, ",
              row[["yield_actual"]], " kg/ha actual yield, ",
              row[["yield_loss"]], "% yield loss."
            )
          }),
          collapse = "\n"
        )

        prompt <- paste0(
          "You are FraNchEstYN, the monster wheat growth model from Transylvania.\n",
          "Write a short summary in scientific style of this wheat rust simulation.\n",
          "Describe in a single paragraph (max 4 sentences) the current simulation.\n\n",
          yearly_text,
          "\nEnd with a creepy agricultural proverb."
        )

        res <- POST(
          url = "https://openrouter.ai/api/v1/chat/completions",
          add_headers(
            Authorization = paste("Bearer", apikey),
            "Content-Type" = "application/json"
          ),
          body = toJSON(list(
            model = "z-ai/glm-4.5-air:free",
            messages = list(list(role = "user", content = prompt))
          ), auto_unbox = TRUE)
        )

        parsed <- content(res, as = "parsed")

        if (!is.null(parsed$choices[[1]]$message$content)) {
          output <- parsed$choices[[1]]$message$content
          cat("\n================= 🌾💀 FRANCHY RESPONSE 💀🌾 =================\n\n")
          for (line in strsplit(output, "\n")[[1]]) {
            if (nchar(trimws(line)) > 0) {
              cat(sample(c("🧟", "🦇", "👻", "💀", "🎃"), 1), line, "\n")
            }
          }
          cat("\n=================================================================\n")
          result$spooky_message <- output
        }

      }, silent = TRUE)
    }
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
