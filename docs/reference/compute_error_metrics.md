# Compute error metrics for model vs reference variables (internal)

Compute error metrics for model vs reference variables (internal)

## Usage

``` r
compute_error_metrics(
  outputs_df,
  group_cols = c("GrowingSeason", "Site", "Variety", "file")
)
```

## Arguments

- outputs_df:

  data.frame with simulation and reference columns

- group_cols:

  character vector of grouping columns

## Value

tibble with one row per group/variable

tibble with one row per group/variable
