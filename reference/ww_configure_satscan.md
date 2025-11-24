# Configure SaTScan for Bernoulli purely spatial scan

Define the analysis parameters required by SaTScan's GUI to conduct a
Bernoulli-based purely spatial scan to detect either clusters of high
rates of acute malnutrition or both high and low rates.

User's input is limited to specifying the analysis area filename, the
destination directory for the parameters file, the SaTScan version in
use, and the type of clusters to be detected. All other parameters are
pre-defined by this function.

## Usage

``` r
ww_configure_satscan(
  filename = character(),
  params_dir = character(),
  satscan_version = character(),
  .scan_for = c("high-rates", "high-low-rates")
)
```

## Arguments

- filename:

  A quoted string identifying the analysis area.

- params_dir:

  A quoted string of the folder or directory in which the parameters
  file (produced by this function) should be saved. This can be the same
  directory as that specified in
  [`ww_wrangle_data()`](https://nutspatial.github.io/wowi/reference/ww_wrangle_data.md).

- satscan_version:

  A quoted string indicating the version of SaTScan installed on the
  user's computer. Internally, this value is checked against the latest
  available version. If it is older, a warning is issued with a link to
  SaTScan’s website. Although the analysis is not interrupted, it is
  recommended to use the latest version.

- .scan_for:

  A quoted string indicating the type of clusters to scan for. To scan
  for clusters of high rates only, set `.scan_for = "high-rates"`. To
  scan for both high and low rates, set `.scan_for = "high-low-rates"`.

## Details

For more information on Bernoulli purely spatial scans, refer to the
SaTScan technical documentation available at:
<https://www.satscan.org/techdoc.html>.

## References

Kulldorff, M. (2022) *SaTScan user guide for version 10.1*. Available
at: <https://www.satscan.org/>.

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
#> [1] "/tmp/RtmpCNKfp8/file21453039a9ac/input-files/Locality"

library(rsatscan) # important to make `{wowi}` access `{rsatscan}`-specific eviroment
#> rsatscan only does anything useful if you have SaTScan
#> See http://www.satscan.org/ for free access

#### Configure SaTScan ----
do.call(
  what = ww_configure_satscan,
  args = list(
    filename = "Locality",
    params_dir = directory,
    satscan_version = "10.3.2",
    .scan_for = "high-low-rates"
  )
)

## Show file's content ----
file.show(file.path(tmp, "input-files/Locality.prm"))
```
