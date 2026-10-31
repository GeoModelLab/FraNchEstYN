# Indiana reference disease observations

Plot- or site-level reference observations from Indiana used for model
calibration/validation and retrospective analyses.

## Usage

``` r
reference_indiana
```

## Format

A data frame with 5 variables:

- site:

  Character. Site identifier (e.g., station or field name).

- year:

  Integer. Calendar year (\>= 1972).

- DOY:

  Integer. Day of year (1–366) for the observation.

- FINT:

  Numeric. Fungal infection index (or other model-specific
  infection/intensity metric).

- Disease:

  Numeric or integer. Observed disease value (e.g., severity or
  incidence; unit depends on the study protocol).

## Source

Shaner, G., Buechley, G. (1995). Epidemiology of leaf blotch of soft red
winter wheat caused by \*Septoria tritici\* and \*Stagonospora
nodorum\*. \*Plant Disease\*, 79(9), 928–938.
[doi:10.1094/PD-79-0928](https://doi.org/10.1094/PD-79-0928) Available
at
<https://www.apsnet.org/publications/plantdisease/backissues/Documents/1995Articles/PlantDisease79n09_928.PDF>

## Details

Filtered from the original source to include only records from 1972
onward. Columns are renamed so that \`site\` corresponds to \`sName\` in
the raw data. Data were digitized from the original publication by
Shaner & Buechley (1995).

## Examples

``` r
data(reference_indiana)
head(reference_indiana)
#>      site year DOY FINT Disease
#> 1 Indiana 1973 144 1.00      NA
#> 2 Indiana 1973 187 0.25      NA
#> 3 Indiana 1974 141 1.00      NA
#> 4 Indiana 1974 189 0.25      NA
#> 5 Indiana 1975 143 1.00      NA
#> 6 Indiana 1975 188 0.25      NA
```
