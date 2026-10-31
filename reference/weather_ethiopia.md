# Daily weather for Ethiopia fungicide trial sites

A dataset containing hourly weather observations for two sites in
Ethiopia, sourced from NASA POWER.

## Usage

``` r
weather_ethiopia
```

## Format

A data frame with one row per hour:

- Site:

  Character. Site name (e.g., \`"Agecha"\`, \`"Hossana"\`).

- lat:

  Numeric. Site latitude (decimal degrees).

- YEAR:

  Integer. Calendar year of the observation.

- MO:

  Integer. Month (1–12).

- DY:

  Integer. Day of month (1–31).

- HR:

  Integer. Hour of the day (0–23).

- T2M:

  Numeric. Air temperature at 2 m height (°C).

- RH2M:

  Numeric. Relative humidity at 2 m height (percent).

- PRECTOTCORR:

  Numeric. Precipitation (mm).

## Source

NASA POWER project (<https://power.larc.nasa.gov/>). Original trial
context documented in
[doi:10.22004/ag.econ.234661852](https://doi.org/10.22004/ag.econ.234661852)
(open-access PDF available at
<https://core.ac.uk/download/pdf/234661852.pdf>).

## Details

Weather data prepared from NASA POWER reanalysis, extracted for Ethiopia
fungicide trial locations, and bundled with this package. Raw files were
located under \`tests/fungicide/weather/\`.

## Examples

``` r
data(weather_ethiopia)
dplyr::glimpse(weather_ethiopia)
#> Rows: 17,568
#> Columns: 9
#> $ Site        <chr> "Agecha", "Agecha", "Agecha", "Agecha", "Agecha", "Agecha"…
#> $ lat         <dbl> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
#> $ YEAR        <int> 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012…
#> $ MO          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ DY          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ HR          <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ T2M         <dbl> 9.24, 8.75, 8.26, 7.74, 7.25, 6.70, 6.30, 10.10, 14.68, 19…
#> $ RH2M        <dbl> 63.62, 60.29, 58.47, 58.81, 59.50, 60.31, 60.23, 44.76, 31…
#> $ PRECTOTCORR <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```
