<div align="center">



**FraNchEstYN**  
*Fra*mework for sy*Nch*ronous *Est*imation of the *Y*ield reductio*N*

<img src="man/figures/logo.png" alt="FraNchEstYN logo" width="250" align="top"/>

</div>	  

[![License: CC BY-NC 3.0](https://img.shields.io/badge/License-CC%20BY--NC%203.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/3.0/)
[![Platform](https://img.shields.io/badge/platform-Windows--only-blue)](https://microsoft.com)  
[![Language](https://img.shields.io/badge/language-R%20%7C%20C%23-purple)](https://cran.r-project.org/)  
![Status](https://img.shields.io/badge/status-active-brightgreen)

---

## 📖 Overview
**FraNchEstYN** is a hybrid **crop–disease simulation and calibration framework** that couples a **process-based C# model** with an **R interface** for input preparation, model launching, and result analysis.

It integrates **phenology-driven crop growth**, **disease epidemiology**, and **eco-physiological processes** to predict yield, biomass, and disease impacts, with modes for **simulation** and **parameter calibration**.

> **Note:** FraNchEstYN is currently **Windows-only** due to its C# computation core. Cross-platform support is in progress.

---

## Highlights

- 🧟 Simulates crop growth and yield with **disease interactions**
- 🦠 Calibrates crop and/or disease parameters (`crop`, `disease`, `all`)
- 🦇 Computes performance metrics (RMSE, MAE, NSE, R²) automatically
- 👹 Hybrid architecture: **R interface + C# executable**
- 🕸 **Windows-only** (Mac/Linux support under development)

---

## Installation

From GitHub (Windows):

```r
install.packages("devtools")
devtools::install_github("GeoModelLab/FraNchEstYN")
```

## Getting Started

Minimal run example:
```
library(FraNchEstYN)

res <- franchestyn(
  weather_data      = weather_df,      # one site only
  management_data   = mgmt_df,         # crop/variety/resistance/sowingDOY/year
  reference_data    = ref_df,          # required if calibration != "none"
  cropParameters    = cropParameters$Wheat,
  diseaseParameters = diseaseParameters$Septoria,
  calibration       = "all",           # "none", "crop", "disease", or "all"
  start_end         = c(2010, 2020),
  iterations        = 200
)

str(res$outputs$summary)

```

## Documentation
Full documentation and vignettes at:
📚 https://geomodellab.github.io/FraNchEstYN

## License
Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0)

## Authors
Simone Bregaglio & Roberto Ferrise
