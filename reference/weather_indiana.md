# Daily weather for Indiana

A dataset containing daily weather observations for Indiana sites,
sourced from NASA POWER.

## Usage

``` r
weather_indiana
```

## Format

A data frame with one row per day:

- site:

  Site name (here always \`"Indiana"\`).

- year:

  Year (numeric).

- month:

  Month (1–12).

- day:

  Day of month (1–31).

- tx:

  Daily maximum temperature (°C).

- tn:

  Daily minimum temperature (°C).

- p:

  Daily precipitation (mm).

- rad:

  Daily global radiation (MJ m\\^{-2}\\ d\\^{-1}\\).

## Details

Data were prepared from FraNchEstYN’s bundled weather files, located in
\`src_csharp/FraNchEstYN/FraNchEstYN/files/weather/daily/Indiana.csv\`.
The original source is the NASA POWER project
(<https://power.larc.nasa.gov/>).

## Examples

``` r
data(weather_indiana)
dplyr::glimpse(weather_indiana)
#> Rows: 7,304
#> Columns: 8
#> $ site  <chr> "Indiana", "Indiana", "Indiana", "Indiana", "Indiana", "Indiana"…
#> $ year  <int> 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972…
#> $ month <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ day   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 1…
#> $ tx    <dbl> 6.623, 2.450, 5.360, 1.002, -5.355, -3.036, 0.775, 4.655, 7.593,…
#> $ tn    <dbl> -2.085, -3.212, -2.956, -4.786, -17.217, -13.748, -3.964, -10.63…
#> $ p     <dbl> 0.000000, 3.313559, 0.000000, 5.407608, 1.776768, 0.000000, 0.00…
#> $ rad   <dbl> 7.45, 9.44, 8.31, 2.37, 9.63, 9.93, 9.50, 9.51, 4.15, 9.46, 6.81…
```
