# Convert a nested parameter list back into a tidy data.frame

Convert a nested parameter list back into a tidy data.frame

## Usage

``` r
parameters_to_df(param_list)
```

## Arguments

- param_list:

  A nested parameter list (whole model, or a single set)

## Value

A tidy data.frame with columns: Parameter, Description, Unit, Min, Max,
Value, Calibration
