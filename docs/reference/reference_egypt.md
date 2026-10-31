# Reference dataset for Egypt, yellow rust severity and yield loss on seven wheat varieties (Sharkiya, 2013–2015)

This dataset provides reference observations for wheat stripe rust
trials conducted in Sharkiya, Egypt. It includes disease severity
assessments and attainable vs. actual yield data digitized from Omara et
al. (2016), available at \[Losses Assessment in some Egyptian Wheat
Cultivars caused by Stripe Rust Pathogen \*Puccinia
striiformis\*\](https://ejp.journals.ekb.eg/article_92381_3f2e26b0341fd17294835a460fad0c50.pdf).

## Usage

``` r
reference_egypt
```

## Format

A data frame with 8 variables:

- site:

  Trial site (e.g. "Sharkiya")

- variety:

  Cultivar identifier (e.g. "sids12")

- year:

  Year of trial (numeric)

- DOY:

  Day of year (numeric)

- disease:

  Observed disease severity (%), may be NA

- fint:

  Field intensity score (numeric, may be NA)

- yieldActual:

  Measured actual yield (kg/ha)

- yieldAttainable:

  Measured attainable yield (kg/ha)

## Source

Omara et al. (2016).
<https://ejp.journals.ekb.eg/article_92381_3f2e26b0341fd17294835a460fad0c50.pdf>

## Examples

``` r
data(reference_egypt)
head(reference_egypt)
#> # A tibble: 6 × 8
#>   site     variety  year   DOY disease  fint yieldActual yieldAttainable
#>   <chr>    <chr>   <dbl> <dbl>   <dbl> <dbl>       <dbl>           <dbl>
#> 1 Sharkiya sids12   2014    38       5    NA          NA              NA
#> 2 Sharkiya sids12   2014    45      10    NA          NA              NA
#> 3 Sharkiya sids12   2014    52      20    NA          NA              NA
#> 4 Sharkiya sids12   2014    60      30    NA          NA              NA
#> 5 Sharkiya sids12   2014    66      40    NA          NA              NA
#> 6 Sharkiya sids12   2014    73      60    NA          NA              NA
```
