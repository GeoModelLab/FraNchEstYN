# Daily weather dataset for Sharkiya, Egypt (2013–2015)

This dataset provides daily weather observations for Sharkiya, Egypt,
covering the same period as the wheat yellow rust severity and yield
trials (\`reference_egypt\`). It includes maximum and minimum
temperatures, rainfall, and metadata such as site and latitude.

## Usage

``` r
weather_egypt
```

## Format

A data frame with 8 variables:

- Site:

  Trial site ("Sharkiya")

- TMAX:

  Daily maximum temperature (°C)

- TMIN:

  Daily minimum temperature (°C)

- RAIN:

  Daily rainfall (mm)

- year:

  Calendar year (numeric)

- month:

  Calendar month (1–12)

- day:

  Calendar day of month (1–31)

- lat:

  Latitude of trial site (decimal degrees)

## Source

Digitized from Sharkiya daily weather records (2013–2015).

## Examples

``` r
data(weather_egypt)
head(weather_egypt)
#>       Site  TMAX TMIN RAIN year month day      lat
#> 1 Sharkiya 20.49 7.10 0.00 2013     1   1 30.61502
#> 2 Sharkiya 21.39 6.21 0.00 2013     1   2 30.61502
#> 3 Sharkiya 22.32 9.08 0.07 2013     1   3 30.61502
#> 4 Sharkiya 20.83 8.06 0.05 2013     1   4 30.61502
#> 5 Sharkiya 18.65 8.09 0.81 2013     1   5 30.61502
#> 6 Sharkiya 18.66 7.34 0.01 2013     1   6 30.61502
```
