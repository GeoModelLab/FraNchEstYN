# Ethiopia management practices

Management metadata associated with Ethiopia fungicide trial sites.

## Usage

``` r
management_ethiopia
```

## Format

A data frame with one row per site-treatment combination:

- site:

  Character. Site identifier.

- fungicide:

  Character. Fungicide treatment (\`"no"\`, \`"2"\`, \`"3"\`, \`"7"\`).

- crop:

  Character. Crop grown (here \`"wheat"\`).

- variety:

  Character. Cultivar/variety name (often \`"All"\`).

- year:

  Integer. Calendar year of the record.

- sowingDOY:

  Integer. Sowing day of year (1–366).

- treatment:

  Character. Application dates for fungicide treatments
  (comma-separated, e.g., \`"Sep 23, Oct 23"\`).

## Source

Data digitized from the Ethiopia fungicide trial report
(<https://core.ac.uk/download/pdf/234661852.pdf>).

## Details

Management data were read from \`tests/fungicide/managementData.csv\`
and stored in the package via \`usethis::use_data()\`.

## Examples

``` r
data(management_ethiopia)
head(management_ethiopia)
#>      site fungicide  crop variety year sowingDOY             treatment
#> 1 Hossana        no wheat     All 2012       201                     0
#> 2  Agecha        no wheat     All 2012       204                     0
#> 3 Hossana         2 wheat     All 2012       201        Sep 23, Oct 23
#> 4  Agecha         2 wheat     All 2012       204          Oct 8, Nov 8
#> 5 Hossana         3 wheat     All 2012       201 Sep 16, Oct 6, Oct 26
#> 6  Agecha         3 wheat     All 2012       204 Oct 7, Oct 27, Nov 16
```
