# Indiana management practices

Management metadata used by the FraNchEstYN pipeline for Indiana sites.

## Usage

``` r
management_data
```

## Format

A data frame with one row per site-year combination and the following
columns:

- site:

  Character. Site identifier (here always \`"Indiana"\`).

- crop:

  Character. Crop grown (here \`"wheat"\`).

- variety:

  Character. Cultivar/variety name (here \`"Generic"\`).

- year:

  Integer. Calendar year of the management record.

- sowingDOY:

  Integer. Sowing day of year (1–366).

## Source

Internal CSV
(\`src_csharp/FraNchEstYN/FraNchEstYN/files/management/mgt_indiana.csv\`).
Shaner, G., Buechley, G. (1995). Epidemiology of leaf blotch of soft red
winter wheat caused by \*Septoria tritici\* and \*Stagonospora
nodorum\*. \*Plant Disease\*, 79(9), 928–938.
[doi:10.1094/PD-79-0928](https://doi.org/10.1094/PD-79-0928) Available
at
<https://www.apsnet.org/publications/plantdisease/backissues/Documents/1995Articles/PlantDisease79n09_928.PDF>

## Details

Read from \`files/management/mgt_indiana.csv\` and stored in the package
via \`usethis::use_data(management_data, overwrite = TRUE)\`. Data were
digitized from the original publication by Shaner & Buechley (1995).

## Examples

``` r
data(management_data)
head(management_data)
#>      site  crop variety year sowingDOY
#> 1 Indiana wheat Generic 1972       278
#> 2 Indiana wheat Generic 1973       271
#> 3 Indiana wheat Generic 1974       275
#> 4 Indiana wheat Generic 1975       276
#> 5 Indiana wheat Generic 1976       277
#> 6 Indiana wheat Generic 1977       286
```
