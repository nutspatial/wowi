# Prepare SaTScan-required input data files for Bernoulli spatial scan analysis and save them in a user-defined working directory

SaTScan's Bernoulli-based spatial scan requires the input data to be
split into cases, controls, and geographical coordinates files, then
saved in a format readable by the software, and placed in a directory it
can access.

`ww_wrangle_data()` is a convenient function designed for this task. It
assumes that the input anthropometric data has been pre-processed using
the `{mwana}` data wrangling functions.

## Usage

``` r
ww_wrangle_data(
  .data,
  latitude,
  longitude,
  filename = character(),
  dir = character(),
  .gam_based = c("wfhz", "muac", "combined")
)
```

## Arguments

- .data:

  A data frame object that has been wrangled using
  `mwana::mw_wrangle_*()` functions.

- latitude:

  Geographical coordinates. An unquoted string for the variable
  containing the Y-axis, also known as latitude (north-south direction).
  The variable must be named "latitude".

- longitude:

  Geographical coordinates. An unquoted string for the variable
  containing the X-axis, also know as longitude (east-west direction).
  The variable must be named "latitude".

- filename:

  A quoted string identifying the analysis area.

- dir:

  A quoted string of the folder or directory in which the files should
  be saved.

- .gam_based:

  A quoted string indicating the criterion used to define acute
  malnutrition. This is used to identify the right vector where flagged
  values are identified, and for which should be excluded from the
  analysis. Defaults to `wfhz`.

## Value

Three files are created and saved in the user-defined directory as
specifiie in the `dir` argument: a `.cas` file for cases, a `.ctl` for
controls, and a `.geo` file for geographical coordinates. The full
filenames will incorporate the use-defined `filename` string.

The `.cas` and `.ctl` files will each have two columns: the first
containing survey cluster or enumeration area IDs, and the second
containing only `1`s, representing either cases or controls,
respectively. The length of the `.cas` file depends on the number of
positive acute malnutrition cases (`gam == 1`), and the `.ctl` file on
the number of negative cases (`gam == 0`).

The `.geo` file will have three columns: cluster or enumeration area
IDs, latitude, and longitude.

## Examples

``` r
## Given a temporary directory ----
tmp <- withr::local_tempdir()
directory <- file.path(tmp, "input-files")

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

## Apply the function ----
ww_wrangle_data(
  .data = x,
  filename = "Locality",
  dir = directory,
  .gam_based = "wfhz",
  latitude = latitude,
  longitude = longitude
)
#> [1] "/tmp/RtmpbpnR9V/file1fad709f599e/input-files/Locality"

## Show created files ----
list.files(file.path(tmp, "input-files"))
#> [1] "Locality.cas" "Locality.ctl" "Locality.geo"

## Display each files' content ----
file.show(file.path(tmp, "input-files/Locality.cas"))
file.show(file.path(tmp, "input-files/Locality.ctl"))
file.show(file.path(tmp, "input-files/Locality.geo"))
```
