# Ethiopia reference disease observations

Plot-level reference observations from Ethiopia used for model
calibration/validation and retrospective analyses.

## Usage

``` r
reference_ethiopia
```

## Format

A data frame with the following variables:

- site:

  Character. Site identifier (e.g., \`"Hossana"\`, \`"Agecha"\`).

- variety:

  Character. Wheat variety name (e.g., \`"Alidoro"\`).

- fungicide:

  Factor. Fungicide treatment (\`"no"\`, \`"2"\`, \`"3"\`, \`"7"\`).

- year:

  Integer. Calendar year of observation.

- DOY:

  Integer. Day of year (1–366).

- Disease:

  Numeric. Observed disease severity (percent).

- YieldActual:

  Numeric. Actual measured yield (kg/ha).

- YieldAttainable:

  Numeric. Attainable yield (kg/ha) under control, fixed at 6000 kg
  ha-1.

## Source

Trial data compiled from
<https://core.ac.uk/download/pdf/234661852.pdf>.

## Details

Data digitized from fungicide trial results in Ethiopia. \`YieldActual\`
and \`YieldAttainable\` values may be \`NA\` where not reported.

## Examples

``` r
data(reference_ethiopia)
head(reference_ethiopia)
#>      site variety fungicide year DOY Disease YieldActual YieldAttainable
#> 1 Hossana Alidoro        no 2012 201    0.00          NA              NA
#> 2 Hossana Alidoro        no 2012 246    0.78          NA              NA
#> 3 Hossana Alidoro        no 2012 253    2.63          NA              NA
#> 4 Hossana Alidoro        no 2012 260    5.60          NA              NA
#> 5 Hossana Alidoro        no 2012 267   11.81          NA              NA
#> 6 Hossana Alidoro        no 2012 274   22.51          NA              NA
```
