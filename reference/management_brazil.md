# Management information for BRILHANTE wheat trials in Brazil

A dataset with management information derived from EPAMIG experimental
trials in Brazil, filtered for the cultivar BRILHANTE. These data were
processed in \`data-raw/reference_egypt.R\`.

## Usage

``` r
management_brazil
```

## Format

A data frame with one row per planting period × year, containing:

- crop:

  character. Crop name (always \`"wheat"\`).

- variety:

  character. Cultivar name (always \`"BRILHANTE"\`).

- sowingDOY:

  integer. Sowing date expressed as day-of-year (DOY).

- year:

  integer. Growing season year.

- planting_period:

  integer. Planting period index within each year.

## Source

Santos, G.B. dos, Coelho, M.A.O., & Del Ponte, E.M. (2023).
Critical-point yield loss models based on incidence and severity of
wheat head blast epidemics in the Brazilian Cerrado. \*European Journal
of Plant Pathology, 165\*, 421–431.
[doi:10.1007/s10658-022-02614-7](https://doi.org/10.1007/s10658-022-02614-7)

## Details

These records were derived from EPAMIG trial data
(\`data-raw/data_epamig.csv\`) by summarising heading, harvest, disease
severity, and yield across replicates, then extracting only the sowing
information relevant for management simulation.

## Examples

``` r
data(management_brazil)
head(management_brazil)
#> # A tibble: 6 × 5
#>   crop  variety   sowingDOY  year planting_period
#>   <chr> <chr>         <dbl> <int>           <int>
#> 1 wheat BRILHANTE        49  2013               1
#> 2 wheat BRILHANTE        56  2013               2
#> 3 wheat BRILHANTE        63  2013               3
#> 4 wheat BRILHANTE        70  2013               4
#> 5 wheat BRILHANTE        77  2013               5
#> 6 wheat BRILHANTE        84  2013               6
```
