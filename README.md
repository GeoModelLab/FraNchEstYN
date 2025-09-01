<div align="center">

**FraNchEstYN**  
*Fra*mework for sy*Nch*ronous *Est*imates of *Y*ield reductio*N*

<img src="man/figures/logo.png" alt="FraNchEstYN logo" width="250" align="top"/>

</div>	  

[![License: CC BY-NC 3.0](https://img.shields.io/badge/License-CC%20BY--NC%203.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/3.0/)
[![Platform](https://img.shields.io/badge/platform-Windows--only-blue)](https://microsoft.com)  
[![Language](https://img.shields.io/badge/language-R%20%7C%20C%23-purple)](https://cran.r-project.org/)  
![Status](https://img.shields.io/badge/status-active-brightgreen)

---

## 📖 Overview
**FraNchEstYN** is a **crop–disease–fungicide simulation and calibration framework**, designed to support **quantitative plant disease epidemiology and crop modeling research**.  

It couples a **process-based crop–disease model in C#** with an **R interface** for:
- input preparation,
- experiment setup,
- simulation and calibration,  
- and automated post-analysis with optional AI-driven narratives.  

FraNchEstYN can run **standalone crop–disease–fungicide simulations** or be **coupled to any external crop model** (e.g. APSIM, DSSAT, STICS, WOFOST), provided daily LAI or fInt is supplied.  

The framework integrates:
- 🌱 **Crop growth & yield formation** (internal or externally provided)  
- 🦠 **Disease epidemiology and host–pathogen dynamics**  
- 💊 **Fungicide effects** (timing, dose, decay, efficacy)  
- 🔬 **Calibration routines** for crop, disease, and fungicide parameters  
- 📊 **Diagnostics & metrics** (RMSE, R², Bias, NSE)  
- 🧙 **Optional narrative summaries** via Large Language Models (LLMs) in **scientist**, **extensionist**, or **farmer** styles  

> ⚠️ Currently **Windows-only** due to the C# core. Cross-platform support is planned.  

---

## Highlights

- Flexible: use FraNchEstYN **with or without an external crop model**  
- Automated handling of weather, management, reference, and parameter data  
- Multiple **calibration modes** (`"crop"`, `"disease"`, `"all"`, `"none"`)  
- Integrated **fungicide programs** with multi-spray schedules  
- Built-in **validation diagnostics** (summary tables, plots, and metrics)  
- 🔮 LLM-based summaries: context-aware **scientist**, **extensionist**, or **farmer** commentary  

---

## Installation

Install directly from GitHub (**Windows only**):

```r
install.packages("devtools")
devtools::install_github("GeoModelLab/FraNchEstYN")
```

## Getting Started

Minimal run example:
```
library(FraNchEstYN)

res <- franchestyn(
  weather_data        = weather_df,       # one site only
  management_data     = mgmt_df,          # crop/variety/sowing/treatments/year
  reference_data      = ref_df,           # required if calibration != "none"
  cropParameters      = cropParameters$Wheat,
  diseaseParameters   = diseaseParameters$Septoria,
  fungicideParameters = fungicideParameters$protectant,
  calibration         = "all",            # "none", "crop", "disease", "all"
  start_end           = c(2010, 2020),
  iterations          = 200,              # Monte Carlo runs
  apikey              = "sk-or-v1-xxxx",  # OpenRouter key
  franchy_message     = TRUE,             # enables LLM commentary
  personality         = "farmer"          # or "scientist" / "extensionist"
)

str(res$outputs$summary)

```

## 🔬 Scientific Purpose

FraNchEstYN is designed for research on:

- Quantifying yield losses caused by plant diseases
- Testing management strategies (sowing dates, fungicide timings, host resistance)
- Exploring climate impacts on epidemics and yield formation
- Model calibration & validation using observed reference data
- Decision-support narratives with audience-specific styles

## 🔗 Coupling with External Crop Models

FraNchEstYN can be run **standalone** (using its internal crop growth routines) or **coupled with any external crop model** (e.g., APSIM, DSSAT, STICS, WOFOST, CropSyst, etc.).  

When coupling, you must provide a `cropModel_data` dataframe with at least:

- `year`, `doy` — growing season identifiers  
- `agb` — above-ground biomass (g/m²)  
- `yield` — attainable/potential yield (kg/ha or g/m²)  
- `fint` (fraction of intercepted radiation, 0–1) **or** `lai` (leaf area index, m²/m²)  
  - If only `lai` is provided, FraNchEstYN converts it internally:  
    \[
    f_{int} = 1 - \exp(-k \times LAI), \quad k=0.6 \ (\text{default})
    \]

Optional: `gdd` (growing degree days), `thermaltime`


```r
# External crop model outputs (simplified WOFOST run)
wofost_out <- data.frame(
  year = rep(2020, 5),
  doy  = c(100, 120, 140, 160, 180),
  lai  = c(0.5, 1.2, 3.5, 5.0, 3.8),
  agb  = c(100, 350, 1200, 2500, 3800),
  yield = c(0, 0, 0, 1000, 3000)
)

# Run FraNchEstYN with external LAI
res <- franchestyn(
  weather_data      = weather_df,
  cropModel_data    = wofost_out,           # external crop model coupling
  management_data   = mgmt_df,
  reference_data    = NULL,                 # no calibration
  diseaseParameters = diseaseParameters$Septoria,
  fungicideParameters = fungicideParameters$protectant,
  calibration       = "none",               # simulation only
  start_end         = c(2020, 2020),
  iterations        = 1,
  franchy_message   = TRUE,
  personality       = "extensionist"        # options: farmers/scientist/extensionist
)

👉 In this mode, no cropParameters are required — FraNchEstYN directly uses your crop model dynamics, while simulating disease progress and yield reduction.

```

## 🩺 Example diagnostic output

When franchy_message = TRUE, FraNchEstYN generates an LLM-based summary tailored to the selected persona.

Example (persona = "farmer"):

```
==================== 📊 FraNchEstYN Decision-support diagnostic 📊 ====================
🧑‍🌾 OVERVIEW
In our fields, we’re seeing an average disease severity of about 4.3%, which isn’t too bad, while our crops are yielding around 8095 kg per hectare. With temperatures swinging between 10.8°C and 1.5°C, and rainfall at 617.8 mm, we’re keeping a close eye on leaf wetness, which has been around 1124.7 hours.

🌦️ WEATHER–DISEASE LINK
Rainfall (r=0.26), leaf-wetness (r=0.16), t max (r=0.13), t min (r=0.11), rh max (r=0.23), rh min (r=0.17).

🍄 DISEASE DYNAMICS
The growing season begins around 04/22, with a rapid rise in growth observed by 04/29, reaching its peak on 06/08. The overall length of the season is ~210 days.

💊 FUNGICIDE PROGRAM
On average 4.4 sprays. First spray near 04/22 (onset). 60% were preventive, 40% late.  
Advice: Shift sprays closer to onset (≈04/22) and keep intervals tight around 04/29.

📈 MODEL PERFORMANCE
DisSev: RMSE≈0.32, R²≈0.84 (model underestimates)  
LightInterception: insufficient validation signal

📅 YEARLY HIGHLIGHTS
• 1980: 9.9% severity; 7718 kg/ha (10.2% loss). Onset 04/17. First spray 04/14 (preventive) [highest severity]  
• 1986: 0% severity; 8723 kg/ha (0% loss). No epidemic detected. First spray 04/15 [lowest severity]  
...

💬 PROVERB
Moisture at the wrong time invites disease to thrive.

```

## Documentation
Full documentation and vignettes at:
📚 https://geomodellab.github.io/FraNchEstYN

## License
Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0)

## Authors
Roberto Ferrise & Simone Bregaglio 
