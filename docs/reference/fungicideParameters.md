# Fungicide parameter sets for FraNchEstYN

A named list of predefined fungicide parameter sets used in the
FraNchEstYN model. The top-level names correspond to fungicide modes or
classes (e.g., \`"protectant"\`).

## Usage

``` r
fungicideParameters
```

## Format

A named list of fungicide parameter sets:
`fungicideParameters[[set]][[parameter]]` → parameter details list.

## Details

**Structure of each fungicide set:**

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
data(fungicideParameters)
names(fungicideParameters)                   # available fungicide sets
#> [1] "protectant"
names(fungicideParameters$protectant)        # parameters in the protectant set
#> [1] "InitialDose"     "DegradationRate" "TenacityFactor"  "InitialEfficacy"
#> [5] "AShapeParameter" "BShapeParameter"
fungicideParameters$protectant$InitialDose   # details for a single parameter
#> $description
#> [1] "initial fungicide dose"
#> 
#> $unit
#> [1] "fraction"
#> 
#> $min
#> [1] 0
#> 
#> $max
#> [1] 1
#> 
#> $value
#> [1] 1
#> 
#> $calibration
#> [1] FALSE
#> 
```
