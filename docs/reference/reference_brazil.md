# Reference wheat data from Brazil (BRILHANTE cultivar, Sertaozinho site)

A dataset with experimental reference observations for the BRILHANTE
variety grown in Sertaozinho, Brazil. These data come from EPAMIG trials
and were processed in \`data-raw/build_referenceBrazil.R\`.

## Usage

``` r
reference_brazil
```

## Format

A data frame with one row per planting period × year, containing:

- year:

  integer. Growing season year.

- variety:

  character. Cultivar name (always \`"BRILHANTE"\`).

- planting_period:

  integer. Planting period index.

- Disease:

  numeric. Disease severity (DHS), if available; otherwise \`NA\`.

- yieldActual:

  numeric. Yield (kg/ha), if available; otherwise \`NA\`.

- site:

  character. Site name (always \`"Sertaozinho"\`).

- doy:

  numeric. Day of year of the event (heading or harvest).

- fint:

  numeric. Indicator of event type: `1` for heading (no yield/disease),
  `0.1` for harvest (with yield and disease observations).

## Source

Santos, G.B. dos, Coelho, M.A.O., & Del Ponte, E.M. (2023).
Critical-point yield loss models based on incidence and severity of
wheat head blast epidemics in the Brazilian Cerrado. \*European Journal
of Plant Pathology, 165\*, 421–431.
[doi:10.1007/s10658-022-02614-7](https://doi.org/10.1007/s10658-022-02614-7)

## Details

The dataset includes two kinds of rows:

- Heading observations (`fint = 1`), with only `doy` filled.

- Harvest observations (`fint = 0.1`), with `yieldActual` and `Disease`
  values from trial data.

## Examples

``` r
data(reference_brazil)
head(reference_brazil)
#> # A tibble: 6 × 8
#>    year variety   planting_period Disease yieldActual site          doy  fint
#>   <int> <chr>               <int>   <dbl>       <dbl> <chr>       <dbl> <dbl>
#> 1  2013 BRILHANTE               1      NA          NA Sertaozinho    92     1
#> 2  2013 BRILHANTE               2      NA          NA Sertaozinho   100     1
#> 3  2013 BRILHANTE               3      NA          NA Sertaozinho   107     1
#> 4  2013 BRILHANTE               4      NA          NA Sertaozinho   116     1
#> 5  2013 BRILHANTE               5      NA          NA Sertaozinho   124     1
#> 6  2013 BRILHANTE               6      NA          NA Sertaozinho   131     1
```
