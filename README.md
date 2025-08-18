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

It combines a **process-based crop–disease model in C#** with an **R interface** for data preprocessing, experiment setup, and automated post-analysis.  

The framework integrates:
- **Phenology-driven crop growth and yield formation**
- **Disease epidemiology and plant–pathogen interactions through damage mechanisms**
- **Fungicide dynamics and efficacy modeling**
- **Calibration routines** for crop, disease, and fungicide parameters  
- 🔮 *Optional*: a **Large Language Model (LLM) post-processor** that generates **narrative summaries** of outputs (`franchy_message = TRUE`), using an API key from [OpenRouter](https://openrouter.ai/)  

The result is a flexible tool for **dynamic simulation of disease severity to quantify yield losses** caused by plant diseases under different management and climatic scenarios.  

> **Note:** FraNchEstYN is currently **Windows-only** due to its C# computation core. Cross-platform support is in progress.

---

## Highlights

- 🌱 **Crop growth & yield dynamics** with environmental drivers and "genetic" modulators (parameters)   
- 🦠 **Epidemic development** driven by weather variables, pathogen characteristics and host resistance  
- 💊 **Fungicide modeling** (timing, dose, decay, efficacy classes)  
- 🔬 **Parameter calibration** (`"crop"`, `"disease"`, or `"all"`) with automated optimization  
- 📊 **Performance metrics** (RMSE, MAE, NSE, R²) calculated automatically  
- 🖥 **Hybrid architecture**: R handles inputs/outputs, C# runs simulations  
- 🧙 **LLM narrative summaries** for results interpretation (optional)  

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
  weather_data        = weather_df,      # one site only
  management_data     = mgmt_df,         # crop/variety/sowing/treatments/year
  reference_data      = ref_df,          # required if calibration != "none"
  cropParameters      = cropParameters$Wheat,
  diseaseParameters   = diseaseParameters$Septoria,
  fungicideParameters = fungicideParameters$protectant,
  calibration         = "all",           # "none", "crop", "disease", "fungicide", or "all"
  start_end           = c(2010, 2020),
  api_key             = "your-openrouter-api-key", #start with sk-or-v1-xxxxxxxxxxxxxxxxxxx....
  franchy_message     = TRUE, # enables the LLM message
  iterations          = 200
)

str(res$outputs$summary)

```

## 🔬 Scientific Purpose

FraNchEstYN is developed as a research framework for:

- Assessing disease-induced yield reduction across environments
- Supporting epidemiological and crop modeling experiments
- Testing management strategies (e.g., sowing dates, fungicide timings, resistance levels)
- Providing a transparent and reproducible modeling environment for the scientific community

## 🩺 Example diagnostic output

Running `franchestyn()` with weather, management, and crop–disease parameter inputs will produce a structured decision-support report.  
Below is an illustrative excerpt:

```
==================== 📊 FraNchEstYN Decision-support diagnostic 📊 ====================
🧐 OVERVIEW
The crop-disease dataset reveals an average severity of approximately 65% and a mean yield of about 2905 kg/ha, with temperature ranges of Tx/Tn approximately 13.5/4.2 °C, total rainfall of around 798.6 mm, and cumulative leaf wetness of approximately 1603.8 hours, indicating critical infection windows conducive to disease development. These factors collectively inform the understanding of disease dynamics and potential

🌧️ WEATHER–DISEASE ASSOCIATIONS
Weather–disease associations: rainfall (r= 0.83 ); leaf-wetness (r= 0.71 ); t max (r= -0.03 ); t min (r= 0.11 ); rh max (r= 0.42 ); rh min (r= 0.45 ).

🍄 DISEASE DYNAMICS (CALENDAR DATES)
The median timings for the observed season are as follows: onset on 05/26, rapid-rise on 06/09, and peak on 07/16. The total season length is approximately 280 days.

⚕️ FUNGICIDE PROGRAM & TIMING
Apply the first spray by 05/26 to align with the onset of disease. Given the late application pattern, ensure subsequent sprays are administered every 7–10 days, with the next application by 06/09.
Applications often lagged the epidemic; advance the first spray to just before onset (≈ 05/26) and avoid long gaps during the rapid increase window (≈ 06/09).

🤖 MODEL PERFORMANCE
Model–observation performance:
• DisSev: RMSE≈0.22, R²≈0.89 (model underestimates)
• LightInterception: RMSE≈0.13, R²≈0.95 (model underestimates)

📅 YEARLY HIGHLIGHTS
• 1973: 85.8% severity; 840.6 kg/ha (87.4% loss). Onset 05/17, rapid rise ~05/23, peak 07/07 (85.8%). First spray 12/12 (late) [highest severity]
• 1986: 19.3% severity; 7794.5 kg/ha (1.8% loss). Onset 06/28, rapid rise ~06/28, peak 07/28 (19.3%). First spray 12/12 (late) [lowest severity]
• 1989: 83.3% severity; 710.3 kg/ha (90.9% loss). Onset 05/22, rapid rise ~05/25, peak 07/15 (83.3%). First spray 12/12 (late)
• 1990: 51.5% severity; 3713.1 kg/ha (50.6% loss). Onset 05/09, rapid rise ~06/09, peak 07/09 (51.5%). First spray 12/12 (late)
```

## Documentation
Full documentation and vignettes at:
📚 https://geomodellab.github.io/FraNchEstYN

## License
Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0)

## Authors
Roberto Ferrise & Simone Bregaglio 
