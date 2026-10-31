# Disease parameter sets for FraNchEstYN

A named list of predefined disease parameter sets used in the
FraNchEstYN model. The top-level names correspond to diseases (e.g.,
\`"brown_rust"\`, \`"black_rust"\`,\`"yellow_rust"\`, \`"septoria"\`).

## Usage

``` r
diseaseParameters
```

## Format

A named list of disease parameter sets:
`diseaseParameters[[disease]][[parameter]]` → parameter details list.

## Details

**Structure of each disease set:**

- Each set is a named list of parameters.

- Each parameter is itself a list with fields:

  - `description` — description of the parameter

  - `unit` — unit of measurement

  - `min`, `max` — allowed bounds

  - `value` — default value

  - `calibration` — logical; if `TRUE`, the parameter can be optimized
    during calibration

## Examples

``` r
data(diseaseParameters)
names(diseaseParameters)               # available disease sets
#> [1] "black_rust"  "brown_rust"  "septoria"    "wheat_blast" "yellow_rust"
names(diseaseParameters$septoria)      # parameters in Septoria set
#>  [1] "IsSplashBorne"               "OuterInoculumMax"           
#>  [3] "PathogenSpread"              "WetnessDurationOptimum"     
#>  [5] "WetnessDurationMinimum"      "DryCriticalInterruption"    
#>  [7] "Tmin"                        "Topt"                       
#>  [9] "Tmax"                        "RelativeHumidityCritical"   
#> [11] "Rain50Detachment"            "RelativeHumidityNotLimiting"
#> [13] "HydroThermalTimeOnset"       "CyclePercentageOnset"       
#> [15] "LatencyDuration"             "SporulationDuration"        
#> [17] "LightStealerDamage"          "RUEreducerDamage"           
#> [19] "SenescenceAcceleratorDamage" "AssimilateSappersDamage"    
#> [21] "OuterInoculumShapeRelease"   "OuterInoculumShapeParameter"
diseaseParameters$septoria$Tmin        # details for a single parameter
#> $description
#> [1] "tmin for pathogen processes"
#> 
#> $unit
#> [1] "°C"
#> 
#> $min
#> [1] 5
#> 
#> $max
#> [1] 10
#> 
#> $value
#> [1] 7.5
#> 
#> $calibration
#> [1] TRUE
#> 
```
