# Run the FraNchEstYN crop–disease simulation or calibration

Runs the FraNchEstYN model. The function prepares inputs (weather,
management, parameters, reference) and launches the executable. Users do
not manage paths or config files manually.

## Usage

``` r
franchestyn_batch(
  weather_data,
  management_data,
  cropParameters = NULL,
  diseaseParameters = NULL,
  fungicideParameters = NULL,
  start_end = c(2000, 2025),
  ...,
  out_folder
)
```

## Arguments

- weather_data:

  A data frame of daily or hourly weather for **one site only**. The
  function will detect if input is daily or hourly. Column names are
  matched case-insensitively, ignoring spaces, underscores, and dashes.

  **Date columns (mandatory):**

  Provide the combination of `year`, `month`, `day` (and optionally
  `hour` for hourly data).

  **Meteorological variables:**

  *Mandatory:*

  - Daily inputs: `tmax`, `tx`, `t2mmax`, `maxtemp` (max temperature,
    °C), `tmin`, `tn`, `t2mmin`, `mintemp` (min temperature, °C),
    `precipitation`, `prec`, `p`, `rainfall`, `rain` (mm d\\^{-1}\\).

  - Hourly inputs: `temp`, `temperature`, `t2m` (air temperature, °C),
    `precipitation`, `prec`, `precip`, `prectotcorr`, `rainfall`, `rain`
    (mm h\\^{-1}\\).

  *Radiation or Latitude (one required):*

  - Radiation: `rad`, `solar`, `solarrad` \[MJ m\\^{-2}\\ d\\^{-1}\\\]

  - Latitude: `lat`, `latitude`, `site_lat` \[decimal degrees\]

  If radiation is missing, it will be estimated from latitude and day
  length.

  *Optional variables (used if present, estimated otherwise):*

  - Relative humidity: `rh`, `humidity`, `relhumidity`,
    `relativehumidity` (hourly), `rhmax`, `rhx` and `rhmin`, `rhn`
    (daily).

  - Leaf wetness: not required — computed internally from humidity \>
    90% or rainfall ≥ 0.2 mm/h.

- management_data:

  A data frame with management information for the **same site** as
  `weather_data`. Column matching is case-insensitive;
  spaces/underscores/dashes are ignored and normalized to snake_case.

  **Required columns:**

  - `crop` — character (e.g., "Wheat").

  - `sowingDOY` — integer DOY in `[1, 366]`.

  - `year` — either an ISO year (YYYY) or the string `"All"`.

  **Optional:**

  - `treatment` — character with one or more fungicide dates separated
    by commas/semicolons (e.g., `"12 Feb; 28 Feb"`).

- cropParameters:

  A named list of crop parameters (see `data(cropParameters)`).

- diseaseParameters:

  A named list of disease parameters (see `data(diseaseParameters)`).

- fungicideParameters:

  Optional list of fungicide parameters (see
  `data(fungicideParameters)`).

- start_end:

  Numeric vector of length 2. Start and end years for simulation
  (default: `c(2000, 2025)`).

- out_folder:

  output folder path

## Value

A list with elements: `outputs`, `diagnostics`, `parameters`,
`spooky_message`.

## Details

\- Only one site per run. - Column names matched case-insensitively,
tolerant to spaces, underscores, and dashes.

## Examples

``` r
if (FALSE) { # \dontrun{
franchestyn(
  weather_data        = weather_df,
  management_data     = mgmt_df,
  reference_data      = ref_df,
  cropParameters      = cropParameters$wheat,
  diseaseParameters   = diseaseParameters$septoria,
  fungicideParameters = fungicideParameters$protectant,
  calibration         = "all",
  start_end           = c(2010, 2020),
  apikey              = "sk-or-v1-xxxx",
  franchy_message     = TRUE,
  iterations          = 200
)
} # }
```
