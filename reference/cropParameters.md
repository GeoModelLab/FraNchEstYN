# Crop parameter sets for FraNchEstYN

A named list of predefined crop parameter sets used in the FraNchEstYN
model. The top-level names correspond to crop types (e.g., \`"wheat"\`,
\`"rice"\`).

## Usage

``` r
cropParameters
```

## Format

A named list of crop parameter sets:
`cropParameters[[crop]][[parameter]]` → parameter details list.

## Details

**Structure of each crop set:**

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
data(cropParameters)
names(cropParameters)             # available crop sets
#> [1] "rice"  "wheat"
names(cropParameters$wheat)       # parameters in Wheat set
#>  [1] "TbaseCrop"              "ToptCrop"               "TmaxCrop"              
#>  [4] "CycleLength"            "FloweringStart"         "HalfIntGrowth"         
#>  [7] "HalfIntSenescence"      "SlopeGrowth"            "SlopeSenescence"       
#> [10] "RadiationUseEfficiency" "PartitioningMaximum"    "VarietalResistance"    
cropParameters$wheat$TbaseCrop    # details for a single parameter
#> $description
#> [1] "base temperature for crop growth and development"
#> 
#> $unit
#> [1] "°C"
#> 
#> $min
#> [1] -1
#> 
#> $max
#> [1] 5
#> 
#> $value
#> [1] 1
#> 
#> $calibration
#> [1] TRUE
#> 
```
