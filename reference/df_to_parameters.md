# Convert parameter summary dataframe to FraNchEstYN parameter list

This function converts a dataframe of parameter estimates into the
nested list format used by FraNchEstYN (\`cropParameters\` or
\`diseaseParameters\`).

## Usage

``` r
df_to_parameters(
  df,
  atomic_if_missing_meta = FALSE,
  range_pct = 0.1,
  zero_buffer = 1e-06
)
```

## Arguments

- df:

  A \`data.frame\` with at least columns \`Parameter\` and \`Value\`.

- atomic_if_missing_meta:

  Logical, optional. If \`TRUE\`, parameters without metadata are stored
  as scalars instead of lists.

- range_pct:

  Numeric, optional. Percentage of the mean value to define min/max
  ranges (default = 0.2, i.e. \\\pm 20\\\\).

- zero_buffer:

  Numeric, optional. Small positive buffer when value = 0 (default =
  1e-6).

## Value

A \*\*named list\*\* of parameters. Each element is either: A single
numeric value (if \`atomic_if_missing_meta = TRUE\` and no metadata are
provided). Otherwise, a list compatible \`cropParameters\` or
\`diseaseParameters\`

## Details

The ranges are computed as `value * (1 ± range_pct)`. This ensures each
parameter has a plausible min and max around the central estimate. If
\`value\` is 0, a buffer of \`\[0, zero_buffer\]\` is used.

## Examples

``` r
param_summary <- data.frame(
  Parameter = c("RUEreducerDamage","LatentPeriod"),
  Value     = c(0.5, 120)
)
df_to_parameters(param_summary, range_pct = 0.1)
#> $RUEreducerDamage
#> $RUEreducerDamage$description
#> [1] NA
#> 
#> $RUEreducerDamage$unit
#> [1] NA
#> 
#> $RUEreducerDamage$min
#> [1] 0.45
#> 
#> $RUEreducerDamage$max
#> [1] 0.55
#> 
#> $RUEreducerDamage$value
#> [1] 0.5
#> 
#> $RUEreducerDamage$calibration
#> [1] NA
#> 
#> 
#> $LatentPeriod
#> $LatentPeriod$description
#> [1] NA
#> 
#> $LatentPeriod$unit
#> [1] NA
#> 
#> $LatentPeriod$min
#> [1] 108
#> 
#> $LatentPeriod$max
#> [1] 132
#> 
#> $LatentPeriod$value
#> [1] 120
#> 
#> $LatentPeriod$calibration
#> [1] NA
#> 
#> 
```
