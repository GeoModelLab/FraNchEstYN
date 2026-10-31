# Default crop parameters for BRILHANTE wheat (Brazil)

A nested list of crop parameter values calibrated for the BRILHANTE
variety of wheat in Brazil, to be used as input for
[`FraNchEstYN::franchestyn()`](https://geomodellab.github.io/FraNchEstYN/reference/franchestyn.md).

## Usage

``` r
cropParameters_brazil
```

## Format

A named list with parameter entries.

## Source

Santos, G.B. dos, Coelho, M.A.O., & Del Ponte, E.M. (2023).
Critical-point yield loss models based on incidence and severity of
wheat head blast epidemics in the Brazilian Cerrado. \*European Journal
of Plant Pathology, 165\*, 421–431.
[doi:10.1007/s10658-022-02614-7](https://doi.org/10.1007/s10658-022-02614-7)

## Details

Each element corresponds to one model parameter with fields:

- description:

  Character. Parameter description.

- unit:

  Character. Measurement unit.

- min:

  Numeric. Minimum allowed value.

- max:

  Numeric. Maximum allowed value.

- value:

  Numeric. Default calibrated value.

- calibration:

  Logical. Whether the parameter is estimated in calibration.

## Examples

``` r
data(cropParameters_brazil)
names(cropParameters_brazil)[1:5]
#> [1] "TbaseCrop"      "ToptCrop"       "TmaxCrop"       "CycleLength"   
#> [5] "FloweringStart"
```
