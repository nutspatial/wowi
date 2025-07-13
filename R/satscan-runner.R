#'
#'
#' Run SaTScan for Bernoulli purely spatial scan to detect statistically
#' significant clusters of acute malnutrition
#'
#' @description
#' Detect statistically significant spatial clusters of acute malnutrition rates,
#' including high-only or high-and-low clusters. `ww_run_satscan()` is a wrapper
#' function that interacts with the SaTScan GUI via the `{rsatscan}` package.
#' It internally calls both [ww_wrangle_data()] and [ww_configure_satscan()],
#' allowing users to skip these two steps in the workflow and instead call
#' `ww_run_satscan()` directly.
#'
#' @param .data A data frame object that has been wrangled using
#'  `mwana::mw_wrangle_*()` functions.
#'
#' @param filename A quoted string identifying the analysis area.
#'
#' @param dir A quoted string of the folder or directory in which the files
#' should be saved.
#'
#' @param params_dir A quoted string of the folder or directory where the
#' parameters file (produced by this function) should be saved.
#' Defaults to the same value as `dir`.
#'
#' @param sslocation A quoted string indicating the path to the SaTScan
#' GUI installation. This varies by operating system (OS). For macOS, it is
#' typically `"/Applications/SaTScan.app/Contents/app"`; for Windows,
#' `"C:/Program Files/SaTScan"`.
#'
#' @param ssbatchfilename A quoted string specifying the SaTScan batch file
#' name. For macOS, use `"satscan"`; for Windows, use `"SaTScanBatch64"`.
#'
#' @param satscan_version A quoted string indicating the version of SaTScan
#' installed on the user's computer. See [ww_configure_satscan()] for details.
#'
#' @param cleanup Logical. If `TRUE`, deletes all SaTScan output files from
#' the directory after the function runs.
#'
#' @param verbose Logical. If `TRUE`, displays the SaTScan results in the R
#' console as if run in batch mode. This is especially useful if the scan
#' takes a long time.
#'
#' @param .scan_for A quoted string indicating the type of clusters to scan for.
#' To scan for high-rate clusters only, set `.scan_for = "high-rates"`.
#' To scan for both high and low rates, set `.scan_for = "high-low-rates"`.
#'
#' @param .gam_based A quoted string indicating the criterion used to define acute
#' malnutrition. This is used to identify the right vector where flagged values
#' are identified, and for which should be excluded from the analysis. Default
#' is `wfhz`.
#'
#' @returns
#' A set of SaTScan output files, saved in the specified directory. The full
#' file names depend on the `filename` argument:
#'
#' - `filename.txt`: A plain-text summary of the results.
#' - `filename.clustermap.html`: An interactive HTML map showing detected
#'   clusters, with red bubbles for high-rate clusters and blue for low-rate clusters.
#' - Shapefiles: A collection of spatial files suitable for use in GIS software.
#'
#'
#' @examples
#'
#' ## Wrangle data with `{mwana}` ----
#' x <- df |>
#'   mwana::mw_wrangle_wfhz(
#'     sex = sex,
#'     .recode_sex = TRUE,
#'     weight = weight,
#'     height = height
#'   ) |>
#'   mwana::define_wasting(
#'     zscores = wfhz,
#'     .by = "zscores",
#'     edema = edema
#'   )
#'
#' #' ## Given a temporary directory ----
#' tmp <- withr::local_tempdir()
#' directory <- file.path(tmp, "input-files")
#'
#' ## Run satscan ----
#' library(rsatscan) # important to make `{wowi}` access `{rsatscan}`-specific eviroment
#'
#' results <- ww_run_satscan(
#'   .data = x,
#'   filename = "Locality",
#'   dir = directory,
#'   sslocation = "/Applications/SaTScan.app/Contents/app",
#'   ssbatchfilename = "satscan",
#'   satscan_version = "10.3.2",
#'   .scan_for = "high-low-rates",
#'   .gam_based = "wfhz",
#'   verbose = FALSE,
#'   cleanup = FALSE
#' )
#'
#' results
#'
#' @export
#'
ww_run_satscan <- function(
    .data,
    filename = character(),
    dir = character(),
    params_dir = dir,
    sslocation = character(),
    ssbatchfilename = character(),
    satscan_version,
    cleanup = TRUE,
    verbose = FALSE,
    .scan_for = c("high-rates", "high-low-rates"),
    .gam_based = c("wfhz", "muac", "combined")) {
  ## Enforce options in `.scan_for` ----
  .scan_for <- match.arg(.scan_for)

  ## Get SaTScan input data ready for the job ----
  do.call(
    what = ww_wrangle_data,
    args = list(
      .data = .data,
      filename = filename,
      dir = dir,
      .gam_based = .gam_based
    )
  )

  ## ConfigureConfigure SaTScan for a Bernoulli purely spatial scan ----
  do.call(
    what = ww_configure_satscan,
    args = list(
      filename = "Locality",
      params_dir = params_dir,
      satscan_version = satscan_version,
      .scan_for = .scan_for
    )
  )

  ## Run de facto SaTScan ----
  results <- do.call(
    what = rsatscan::satscan,
    args = list(
      prmlocation = params_dir,
      prmfilename = do.call(basename, list(filename)),
      sslocation = sslocation,
      ssbatchfilename = ssbatchfilename,
      verbose = verbose,
      cleanup = cleanup
    )
  )

  ## Return
  results
}