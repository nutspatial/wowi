# Run SaTScan for Bernoulli purely spatial scan to detect statistically significant clusters of acute malnutrition

Detect statistically significant spatial clusters of acute malnutrition
rates, including high-only or high-and-low clusters. `ww_run_satscan()`
is a wrapper function that interacts with the SaTScan GUI via the
`{rsatscan}` package. It internally calls both
[`ww_wrangle_data()`](https://tiwowi.github.io/wowi/reference/ww_wrangle_data.md)
and
[`ww_configure_satscan()`](https://tiwowi.github.io/wowi/reference/ww_configure_satscan.md),
allowing users to skip these two steps in the workflow and instead call
`ww_run_satscan()` directly.

## Usage

``` r
ww_run_satscan(
  .data,
  filename = NULL,
  dir = character(),
  params_dir = dir,
  sslocation = character(),
  ssbatchfilename = character(),
  satscan_version,
  .by_area = FALSE,
  .scan_for = c("high-rates", "high-low-rates"),
  .gam_based = c("wfhz", "muac", "combined"),
  latitude,
  longitude,
  area = NULL,
  cleanup = TRUE,
  verbose = FALSE
)
```

## Arguments

- .data:

  A data frame object that has been wrangled using
  `mwana::mw_wrangle_*()` functions.

- filename:

  Optional. Used only in single-area analysis. Used to identify the
  analysis area. The string should be quoted.

- dir:

  A quoted string of the folder or directory in which the files should
  be saved.

- params_dir:

  A quoted string of the folder or directory in which the parameters
  file (produced by this function) should be saved. Defaults to the same
  value as `dir`.

- sslocation:

  A quoted string indicating the path to the SaTScan GUI installation.
  This varies depending on the operating system (OS). For macOS, it is
  typically `"/Applications/SaTScan.app/Contents/app"`; for Windows,
  `"C:/Program Files/SaTScan"`.

- ssbatchfilename:

  A quoted string specifying the SaTScan batch file name. For macOS, use
  `"satscan"`; for Windows, use `"SaTScanBatch64"`.

- satscan_version:

  A quoted string indicating the version of SaTScan installed on the
  user's computer. See
  [`ww_configure_satscan()`](https://tiwowi.github.io/wowi/reference/ww_configure_satscan.md)
  for details.

- .by_area:

  Logical. If `TRUE`, area-wise scan is done. Defaults to `FALSE`.

- .scan_for:

  A quoted string indicating the type of clusters to scan for. To scan
  for clusters of high rates only, set `.scan_for = "high-rates"`. To
  scan for both high and low rates, set `.scan_for = "high-low-rates"`.

- .gam_based:

  A quoted string indicating the criterion used to define acute
  malnutrition. This is used to identify the right vector where flagged
  values are identified, and for which should be excluded from the
  analysis. Defaults to `wfhz`.

- latitude:

  Geographical coordinates. An unquoted string for the variable
  containing the x-axis, also known as latitude (east-west direction).
  The variable must be named "latitude".

- longitude:

  Geographical coordinates. An unquoted string for the variable
  containing the y-axis, also know as longitude (north-south direction).
  The variable must be named "longitude".

- area:

  An unquoted string for the variable containing the analysis areas for
  iteration.

- cleanup:

  Logical. If `TRUE`, deletes all SaTScan output files from the
  directory after the function runs.

- verbose:

  Logical. If `TRUE`, displays the SaTScan results in the R console as
  if run in batch mode. This is especially useful if the scan takes a
  long time.

## Value

A set of SaTScan output files, saved in the specified directory. The
full file names depend on the `filename` argument:

- `filename.txt`: A plain-text summary of the results.

- `filename.clustermap.html`: An interactive HTML map showing detected
  clusters, with red bubbles for high-rate clusters and blue for
  low-rate clusters.

- Shapefiles: A collection of spatial files suitable for use in GIS
  software.

## Details

The geographical coordinates must be provided as latitude and longitude
values. If the input data uses different variable names, they must be
renamed accordingly; otherwise, the analysis will be aborted. Latitude
corresponds to the X-axis (east-west direction), and longitude the to
X-axis (north-south direction).

## Examples

``` r
## Wrangle data with `{mwana}` ----
x <- anthro |>
  dplyr::rename(longitude = y, latitude = x) |>
  mwana::mw_wrangle_wfhz(
    sex = sex,
    .recode_sex = TRUE,
    weight = weight,
    height = height
  ) |>
  mwana::define_wasting(
    zscores = wfhz,
    .by = "zscores",
    edema = oedema
  )
#> ================================================================================

#' ## Given a temporary directory ----
tmp <- withr::local_tempdir()
directory <- file.path(tmp, "input-files")

## Run satscan ----
library(rsatscan) # important to make `{wowi}` access `{rsatscan}`-specific eviroment

if (file.exists("/Applications/SaTScan.app/Contents/app/satscan")) {
  results <- ww_run_satscan(
    .data = x,
    filename = "Locality",
    dir = directory,
    sslocation = "/Applications/SaTScan.app/Contents/app",
    ssbatchfilename = "satscan",
    satscan_version = "10.3.2",
    .scan_for = "high-low-rates",
    .gam_based = "wfhz",
    latitude = latitude,
    longitude = longitude,
    .by_area = FALSE,
    area = NULL,
    verbose = FALSE,
    cleanup = FALSE
  )
}
```
