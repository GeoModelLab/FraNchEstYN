  # ════════════════════════════════════════════════════════════════════════════════
  #  Indiana – Septoria tritici blotch (STB)
  #  FraNchEstYN | COMPAG revision
  #  Analysis: full calibration, LOOCV, parameter uncertainty, ribbon, figures
  #  Data: Shaner & Finney (1977); reference_indiana (FraNchEstYN package)
  #  Period: 1971–1991 (19 growing seasons with observations, sown 1972–1990)
  # ════════════════════════════════════════════════════════════════════════════════

  # ── 0. FLAGS ──────────────────────────────────────────────────────────────────
  #  Set to TRUE to re-run the long computations.
  #  FALSE (default) loads pre-computed results from cache/.
  rm(list = ls())
  RUN_CALIBRATION <- FALSE   # ~5 min   → cache/indiana_calib_predictions.csv
  RUN_LOOCV       <- FALSE   # ~45 min  → cache/indiana_loocv_*.csv
  RUN_RIBBON      <- FALSE   # ~45 min  → cache/indiana_loocv_ribbon.csv


  # ── 1. Setup ──────────────────────────────────────────────────────────────────

  library(tidyverse)
  library(patchwork)
  library(cowplot)
  library(FraNchEstYN)

  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  CACHE <- "cache"
  dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)


  # ── 2. Helper: performance metrics ───────────────────────────────────────────
  compute_metrics <- function(obs, sim, label = "") {
    obs <- as.numeric(obs); sim <- as.numeric(sim)
    idx <- complete.cases(obs, sim)
    obs <- obs[idx]; sim <- sim[idx]
    n   <- length(obs)
    if (n < 3) { warning("Too few obs/sim for: ", label); return(NULL) }
    rmse <- sqrt(mean((sim - obs)^2))
    mbe  <- mean(sim - obs)
    d    <- 1 - sum((obs - sim)^2) /
                sum((abs(sim - mean(obs)) + abs(obs - mean(obs)))^2)
    ct   <- cor.test(obs, sim, method = "pearson")
    tibble(Phase = label, N = n,
           d    = round(d,            3),
           r    = round(ct$estimate,  3),
           RMSE = round(rmse,         3),
           MBE  = round(mbe,          4))
  }


  # ── 3. Data & model parameters ────────────────────────────────────────────────
  weather_data    <- weather_indiana
  management_data <- management_indiana
  reference_data  <- reference_indiana

  # Crop: keep canopy-shape parameters only (no biomass/yield formation)
  thisCropParam <- set_calibration_flags(
    cropParameters$wheat,
    disable = c("PartitioningMaximum", "TbaseCrop", "TmaxCrop", "ToptCrop",
                "FloweringStart", "RadiationUseEfficiency",
                "CycleLength", "HalfIntSenescence")   # fixed: CV < 5% across LOOCV folds
  )

  # Disease: disable damage mechanisms (severity only, no yield loss)
  thisDiseaseParam <- set_calibration_flags(
    diseaseParameters$septoria,
    disable = c("RUEreducerDamage", "SenescenceAcceleratorDamage",
                "AssimilateSappersDamage", "LightStealerDamage",
                "Topt", "Tmax", "RelativeHumidityCritical", "RelativeHumidityNotLimiting")  # fixed: CV < 5% across LOOCV folds
  )
  thisDiseaseParam$OuterInoculumShapeRelease$value <- 2

  START_END  <- c(1971, 1991)
  ITER_FULL  <- 999
  ITER_LOOCV <- 333
  years      <- sort(unique(reference_data$year))   # reference 'year' = HARVEST year
  # Simulation outputs are labelled by GrowingSeason = SOWING year (harvest year - 1).
  # LOOCV folds are defined on growing seasons, and the matching reference rows
  # are those with year == season + 1.
  seasons    <- years - 1
  # Folds and figure panels are LABELLED by harvest year (holdout_year = season + 1).


  # ── 4. Full calibration ───────────────────────────────────────────────────────
  if (RUN_CALIBRATION) {
    message("=== Full calibration (all years, ", ITER_FULL, " iterations) ===")
    df_full <- franchestyn(
      weather_data      = weather_data,
      management_data   = management_data,
      reference_data    = reference_data,
      cropParameters    = thisCropParam,
      diseaseParameters = thisDiseaseParam,
      start_end         = START_END,
      calibration       = "all",
      iterations        = ITER_FULL
    )

    calib_params_crop    <- df_full$parameters$crop
    calib_params_disease <- df_full$parameters$disease

    # Obs-filtered: for metrics and observed points
    calib_preds <- df_full$outputs$simulation %>%
      filter(!is.na(as.numeric(DiseaseSeverityRef))) %>%
      transmute(obs = as.numeric(DiseaseSeverityRef),
                sim = DiseaseSeverity, GrowingSeason, Doy)

    # Full trajectory: for continuous simulated line in figure
    calib_sims <- df_full$outputs$simulation %>%
      transmute(sim = DiseaseSeverity, GrowingSeason, Doy)

    write.csv(calib_preds,
              file.path(CACHE, "indiana_calib_predictions.csv"),
              row.names = FALSE)
    write.csv(calib_sims,
              file.path(CACHE, "indiana_calib_simulations.csv"),
              row.names = FALSE)

    # Save calibrated parameter values (full dataset)
    bind_rows(
      parameters_to_df(calib_params_crop)    %>% mutate(holdout_year = "full", module = "crop"),
      parameters_to_df(calib_params_disease) %>% mutate(holdout_year = "full", module = "disease")
    ) %>%
      write.csv(file.path(CACHE, "indiana_calib_parameters.csv"), row.names = FALSE)

    message("✓ Full calibration complete. Results in cache/")
  }


  # ── 5. LOOCV ─────────────────────────────────────────────────────────────────
  if (RUN_LOOCV) {
    message("=== LOOCV (", length(seasons), " folds, ", ITER_LOOCV, " iterations each) ===")

    loocv_preds   <- list()
    loocv_sims    <- list()
    loocv_metrics <- list()
    fold_params   <- list()

    for (holdout in seasons) {
      message("  -- Fold: holdout season = ", holdout, " (harvest ", holdout + 1, ") --")

      # 5a. Calibrate on (n-1) years
      df_train <- tryCatch(
        franchestyn(
          weather_data      = weather_data,
          management_data   = management_data,
          reference_data    = reference_data %>% filter(year != holdout + 1),   # drop the held-out SEASON
          cropParameters    = thisCropParam,
          diseaseParameters = thisDiseaseParam,
          start_end         = START_END,
          calibration       = "all",
          iterations        = ITER_LOOCV
        ),
        error = function(e) { message("  ERROR calib fold ", holdout, ": ", e$message); NULL }
      )
      if (is.null(df_train)) next

      # Save calibrated parameters for this fold
      fold_params[[as.character(holdout)]] <- bind_rows(
        parameters_to_df(df_train$parameters$crop)    %>% mutate(holdout_year = holdout + 1, module = "crop"),
        parameters_to_df(df_train$parameters$disease) %>% mutate(holdout_year = holdout + 1, module = "disease")
      )

      # 5b. Validate on holdout year (full weather range)
      df_valid <- tryCatch(
        franchestyn(
          weather_data      = weather_data,
          management_data   = management_data,
          reference_data    = reference_data,
          cropParameters    = disable_all_calibration(df_train$parameters$crop),
          diseaseParameters = disable_all_calibration(df_train$parameters$disease),
          start_end         = START_END,
          calibration       = "none"
        ),
        error = function(e) { message("  ERROR valid fold ", holdout, ": ", e$message); NULL }
      )
      if (is.null(df_valid)) next

      # Obs-filtered: for metrics and observed points
      sim_v <- df_valid$outputs$simulation %>%
        filter(GrowingSeason == holdout, !is.na(as.numeric(DiseaseSeverityRef))) %>%
        transmute(obs = as.numeric(DiseaseSeverityRef), sim = DiseaseSeverity,
                  GrowingSeason, holdout_year = holdout + 1, Doy)

      # Full trajectory: for continuous simulated line in figure
      sim_full <- df_valid$outputs$simulation %>%
        filter(GrowingSeason == holdout) %>%
        transmute(sim = DiseaseSeverity, GrowingSeason, holdout_year = holdout + 1, Doy)

      if (nrow(sim_v) == 0) { message("  No reference data for holdout year ", holdout); next }
      loocv_preds[[as.character(holdout)]]   <- sim_v
      loocv_sims[[as.character(holdout)]]    <- sim_full
      loocv_metrics[[as.character(holdout)]] <- compute_metrics(
        sim_v$obs, sim_v$sim, sprintf("LOOCV_fold_%d", holdout + 1)
      )
    }

    # Aggregate predictions and fold-level metrics
    loocv_df      <- bind_rows(loocv_preds)
    loocv_fold_df <- bind_rows(loocv_metrics)

    # Parameter variability across folds
    params_df <- bind_rows(fold_params)
    params_summary <- params_df %>%
      group_by(parameter = Parameter, module) %>%
      summarise(mean_val = mean(Value, na.rm = TRUE),
                sd_val   = sd(Value,   na.rm = TRUE),
                cv_pct   = abs(sd(Value, na.rm=TRUE) / mean(Value, na.rm=TRUE)) * 100,
                min_val  = min(Value,  na.rm = TRUE),
                max_val  = max(Value,  na.rm = TRUE),
                .groups  = "drop") %>%
      arrange(desc(cv_pct))

    loocv_sims_df <- bind_rows(loocv_sims)
    write.csv(loocv_df,       file.path(CACHE, "indiana_loocv_predictions.csv"),        row.names = FALSE)
    write.csv(loocv_sims_df,  file.path(CACHE, "indiana_loocv_simulations.csv"),         row.names = FALSE)
    write.csv(loocv_fold_df,  file.path(CACHE, "indiana_loocv_fold_metrics.csv"),        row.names = FALSE)
    write.csv(params_df,      file.path(CACHE, "indiana_loocv_parameters_raw.csv"),      row.names = FALSE)
    write.csv(params_summary, file.path(CACHE, "indiana_loocv_parameters_summary.csv"),  row.names = FALSE)
    message("✓ LOOCV complete. Results in cache/")
  }


  # ── 6. Parameter uncertainty ribbon ──────────────────────────────────────────
  #  For each season, run the model with parameters from each LOOCV fold.
  #  The ribbon (min-max across folds) captures parameter uncertainty.
  if (RUN_RIBBON) {
    message("=== Parameter uncertainty ribbon ===")
    params_raw <- read.csv(file.path(CACHE, "indiana_loocv_parameters_raw.csv"))
    folds_all  <- sort(unique(params_raw$holdout_year))
    all_ribbon <- list()

    for (yr in seasons) {
      message("  Season: ", yr)
      ref_yr  <- reference_data %>% filter(year == yr + 1)
      yr_sims <- list()

      for (fold in folds_all) {
        fp_crop <- params_raw %>% filter(holdout_year == fold, module == "crop")
        fp_dis  <- params_raw %>% filter(holdout_year == fold, module == "disease")

        cp <- thisCropParam
        dp <- thisDiseaseParam
        for (i in seq_len(nrow(fp_crop))) {
          pn <- fp_crop$Parameter[i]
          if (pn %in% names(cp)) cp[[pn]]$value <- fp_crop$Value[i]
        }
        for (i in seq_len(nrow(fp_dis))) {
          pn <- fp_dis$Parameter[i]
          if (pn %in% names(dp)) dp[[pn]]$value <- fp_dis$Value[i]
        }

        df_sim <- tryCatch(
          franchestyn(
            weather_data      = weather_data,
            management_data   = management_data,
            reference_data    = ref_yr,
            cropParameters    = disable_all_calibration(cp),
            diseaseParameters = disable_all_calibration(dp),
            start_end         = START_END,
            calibration       = "none"
          ),
          error = function(e) { message("    ERROR fold ", fold, ": ", e$message); NULL }
        )

        if (!is.null(df_sim)) {
          sim_out <- df_sim$outputs$simulation %>%
            filter(GrowingSeason == yr) %>%       # full trajectory, not obs-filtered
            mutate(fold = fold) %>%
            select(GrowingSeason, Doy, sim = DiseaseSeverity, fold)
          if (nrow(sim_out) > 0)
            yr_sims[[as.character(fold)]] <- sim_out
        }
      }

      if (length(yr_sims) > 0) {
        all_ribbon[[as.character(yr)]] <- bind_rows(yr_sims) %>%
          group_by(GrowingSeason, Doy) %>%
          summarise(sim_min  = min(sim,  na.rm = TRUE),
                    sim_max  = max(sim,  na.rm = TRUE),
                    sim_mean = mean(sim, na.rm = TRUE),
                    .groups  = "drop")
      }
    }

    ribbon_df <- bind_rows(all_ribbon)
    write.csv(ribbon_df, file.path(CACHE, "indiana_loocv_ribbon.csv"), row.names = FALSE)
    message("✓ Ribbon saved: ", nrow(ribbon_df), " rows in cache/")
  }


  # ── 7. Load cached results ────────────────────────────────────────────────────
  calib_preds  <- read.csv(file.path(CACHE, "indiana_calib_predictions.csv"))
  calib_sims   <- read.csv(file.path(CACHE, "indiana_calib_simulations.csv"))
  loocv_df     <- read.csv(file.path(CACHE, "indiana_loocv_predictions.csv"))
  loocv_sims_df <- read.csv(file.path(CACHE, "indiana_loocv_simulations.csv"))
  fold_m      <- read.csv(file.path(CACHE, "indiana_loocv_fold_metrics.csv"))
  ribbon      <- read.csv(file.path(CACHE, "indiana_loocv_ribbon.csv")) %>%
                   mutate(holdout_year = GrowingSeason + 1)   # label by harvest year

  # Compute pooled metrics from data (not hardcoded)
  metrics_calib <- compute_metrics(calib_preds$obs, calib_preds$sim, "Calibration")
  metrics_loocv <- compute_metrics(loocv_df$obs,    loocv_df$sim,    "LOOCV validation")
  metrics_all   <- bind_rows(metrics_calib, metrics_loocv)
  write.csv(metrics_all, file.path(CACHE, "indiana_metrics_loocv.csv"), row.names = FALSE)


  # ── 8. Figures ────────────────────────────────────────────────────────────────
  #  Shared aesthetics
  BASE_SIZE <- 9
  PAL_SIM   <- "#d6604d"
  PAL_OBS   <- "grey20"

  # ── 8a. Figure 5 (paper) – canopy dynamics (full calibration) ─────────────────
  #  (kept for reference; this was the original Figure 5)
  if (RUN_CALIBRATION) {
    sim_all <- df_full$outputs$simulation %>% filter(GrowingSeason < 1990) %>%
      mutate(HarvestYear = GrowingSeason + 1)   # label by harvest year

    fig5_calib <- ggplot(sim_all,
      aes(x = as.Date(Date, format = "%m/%d/%Y"))) +
      geom_area(aes(y = Dead,        fill = "Removed"),     alpha = 0.50) +
      geom_area(aes(y = Sporulating, fill = "Sporulating"), alpha = 0.30) +
      geom_area(aes(y = Latent,      fill = "Latent"),      alpha = 0.80) +
      geom_line(aes(y = LightInterception, colour = "Attainable LI"),
                linewidth = 0.6, linetype = "dashed") +
      geom_line(aes(y = LightIntHealthy,   colour = "Actual LI"),      linewidth = 0.6) +
      geom_line(aes(y = Susceptible,       colour = "Susceptible"),     linewidth = 0.6) +
      geom_line(aes(y = DiseaseSeverity,   colour = "Disease severity"),linewidth = 0.7) +
      geom_point(aes(y = as.numeric(DiseaseSeverityRef), colour = "Observed"),
                 shape = 21, fill = NA, size = 1.8, stroke = 0.6) +
      facet_wrap(~ HarvestYear, scales = "free_x", ncol = 6) +
      scale_y_continuous(name = "Proportion (0–1)") +
      scale_x_date(date_breaks = "30 days", date_labels = "%b") +
      scale_fill_manual(name = "Canopy fractions",
        values = c(Removed = "red4", Sporulating = "orange", Latent = "pink4")) +
      scale_colour_manual(name = "Variables",
        values = c("Attainable LI" = "green4", "Actual LI" = "darkgreen",
                   "Susceptible" = "black", "Disease severity" = "steelblue",
                   "Observed" = "steelblue")) +
      labs(x = NULL) +
      theme_classic(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            legend.position = "bottom", legend.box = "horizontal",
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 13))

    ggsave("Figure5_calibration.png", fig5_calib, height = 7, width = 12, dpi = 300)
    ggsave("Figure5_calibration.pdf", fig5_calib, height = 7, width = 12)
    message("✓ Figure5_calibration saved")
  }

  # ── 8b. Figure 5 (revision) – LOOCV: ribbon + scatter + metrics table ─────────

  # Panel A: one facet per season (19), ribbon + simulated line + observed points
  # loocv_df  = obs-filtered (points + metrics)
  # loocv_sims_df = full trajectory (line)
  # ribbon     = full trajectory across folds (ribbon)

  # ── Clip lines at epidemic peak (avoid senescence artefact) ───────────────────
  loocv_sims_plot <- loocv_sims_df %>%
    group_by(GrowingSeason, holdout_year) %>%
    filter(Doy <= Doy[which.max(sim)]) %>%
    ungroup()
  ribbon_plot <- ribbon %>%
    group_by(holdout_year) %>%
    filter(Doy <= Doy[which.max(sim_mean)]) %>%
    ungroup()

  # ── Panel A: per-year LOOCV time series ──────────────────────────────────────
  year_labs <- setNames(
    as.character(sort(unique(loocv_df$holdout_year))),
    as.character(sort(unique(loocv_df$holdout_year)))
  )

  pA <- ggplot() +
    geom_ribbon(data = ribbon_plot,
                aes(x = Doy, ymin = sim_min, ymax = sim_max, group = holdout_year),
                fill = PAL_SIM, alpha = 0.20) +
    geom_line(data  = loocv_sims_plot,
              aes(x = Doy, y = sim, group = holdout_year),
              colour = PAL_SIM, linewidth = 0.8) +
    geom_point(data = loocv_df,
               aes(x = Doy, y = obs, group = holdout_year),
               shape = 21, fill = NA, colour = PAL_OBS,
               size = 1.8, stroke = 0.9) +
    facet_wrap(~holdout_year, nrow = 2, ncol = 10,
               labeller = labeller(holdout_year = year_labs)) +
    coord_cartesian(xlim = c(90, 180)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1),
                       labels = c("0", "0.5", "1")) +
    scale_x_continuous(breaks = c(110, 140, 170)) +
    labs(x = "Day of year", y = "Disease severity", tag = "A") +
    theme_bw(base_size = BASE_SIZE) +
    theme(strip.background = element_blank(),
          strip.text        = element_text(face = "plain", size = 11),
          panel.grid.minor  = element_blank(),
          panel.grid.major  = element_blank(),
          axis.text         = element_text(size = 10),
          axis.title      = element_text(size = 12) )
  pA
  # ── Panel B: observed vs simulated scatter — wider than tall ──────────────────
  scatter_df <- bind_rows(
    calib_preds %>% transmute(obs, sim, Phase = "Calibration"),
    loocv_df    %>% transmute(obs, sim, Phase = "LOOCV")
  ) %>% mutate(Phase = factor(Phase, levels = c("Calibration", "LOOCV")))

  # Regression equations — 2 decimal places, placed top-left
  lm_eqs <- scatter_df %>%
    group_by(Phase) %>%
    summarise(fit = list(lm(sim ~ obs)), .groups = "drop") %>%
    mutate(
      b0   = sapply(fit, function(m) coef(m)[1]),
      b1   = sapply(fit, function(m) coef(m)[2]),
      r2   = sapply(fit, function(m) summary(m)$r.squared),
      pval = sapply(fit, function(m) summary(m)$coefficients[2, 4]),
      sig  = dplyr::case_when(pval < 0.001 ~ "***",
                              pval < 0.01  ~ "**",
                              pval < 0.05  ~ "*",
                              TRUE         ~ "ns"),
      lbl  = sprintf("y = %.2f + %.2f x%s    R² = %.2f", b0, b1, sig, r2),
      obs  = 0.02,
      sim  = c(0.97, 0.87)
    )

  pB <- ggplot(scatter_df, aes(x = obs, y = sim, colour = Phase, shape = Phase)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", colour = "grey60", linewidth = 0.6) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, fullrange = FALSE) +
    geom_point(alpha = 0.65, size = 2.0) +
    geom_text(data = lm_eqs,
              aes(x = obs, y = sim, label = lbl, colour = Phase),
              size = 4.0, hjust = 0, lineheight = 1.4, show.legend = FALSE) +
    scale_colour_manual(values = c("Calibration" = "#2166ac", "LOOCV" = PAL_SIM)) +
    scale_shape_manual(values  = c("Calibration" = 16, "LOOCV" = 1)) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    labs(x = "Observed disease severity",
         y = "Simulated disease severity",
         tag = "B", colour = NULL, shape = NULL) +
    theme_classic(base_size = 11) +
    theme(legend.position  = c(0.80, 0.12),
          legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3),
          legend.text       = element_text(size = 10),
          legend.key.size   = unit(0.45, "cm"),
          aspect.ratio      = 0.70,
          axis.text         = element_text(size = 10),
          axis.title      = element_text(size = 12))
  pB

  # ── Panel C: table — metrics × phase ─────────────────────────────────────────
  tbl_data <- metrics_all %>%
    mutate(Phase = recode(Phase, "LOOCV validation" = "LOOCV")) %>%
    transmute(Phase,
              d      = round(d,       3),
              r      = round(r,       3),
              RMSE   = round(RMSE,    3),
              MBE    = round(MBE,    3)) %>%
    pivot_longer(-Phase, names_to = "metric", values_to = "value") %>%
    mutate(
      metric = factor(metric, levels = c("d", "r", "RMSE", "MBE")),
      Phase  = factor(Phase,  levels = c("LOOCV", "Calibration")),  # top=LOOCV
      label  = sprintf("%.3f", value)
    )

  # alternating row shading
  row_shade <- data.frame(
    Phase = factor(c("Calibration","LOOCV"), levels = c("LOOCV","Calibration")),
    fill  = c("white","grey94")
  )

  pC <- ggplot(tbl_data, aes(x = metric, y = Phase)) +
    geom_tile(data = row_shade %>%
                tidyr::crossing(metric = factor(c("d","r","RMSE","MBE"),
                                               levels = c("d","r","RMSE","MBE"))),
              aes(fill = fill), colour = "grey70", linewidth = 0.4) +
    scale_fill_identity() +
    geom_text(aes(label = label, colour = Phase),
              size = 4, fontface = "bold") +
    scale_colour_manual(values = c("Calibration" = "#2166ac", "LOOCV" = PAL_SIM),
                        guide  = "none") +
    labs(x = NULL, y = NULL, tag = "C") +
    theme_minimal(base_size = 11) +
    theme(panel.grid   = element_blank(),
          axis.text.x  = element_text(face = "bold", size = 12),
          axis.text.y  = element_text(size = 11, face = "bold"),
          plot.tag     = element_text(face = "bold", size = 13))
  pC
  # ── Assemble ──────────────────────────────────────────────────────────────────
  bottom_row <- (pB | pC) + plot_layout(widths = c(1.3, 1.3))

  fig5_loocv <- (pA / bottom_row) +
    plot_layout(heights = c(1.1, 1)) &
    theme(plot.tag = element_text(face = "plain", size = 17))

  ggsave("Figure5_loocv.png", fig5_loocv, width = 26, height = 16, units = "cm", dpi = 600)
  ggsave("Figure5_loocv.pdf", fig5_loocv, width = 26, height = 16, units = "cm")
  message("✓ Figure5_loocv.png / .pdf saved")
