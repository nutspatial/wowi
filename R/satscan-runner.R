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
#' @param filename Optional. Used only in single-area analysis.
#' Used to identify the analysis area. The string should be quoted.
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
#' @param .by_area Logical. If `TRUE`, area-wise scan is done. Defaults to `FALSE`.
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
#' @param area An unquoted string for the variable containing the analysis areas
#' for iteration.
#'
#' @param cleanup Logical. If `TRUE`, deletes all SaTScan output files from
#' the directory after the function runs.
#'
#' @param verbose Logical. If `TRUE`, displays the SaTScan results in the R
#' console as if run in batch mode. This is especially useful if the scan
#' takes a long time.
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
#' @details
#' The geographical coordinates must be provided as latitude and longitude values.
#' If the input data uses different variable names, they must be renamed accordingly;
#' otherwise, the analysis will be aborted. Latitude corresponds to the Y-axis
#' (north-south direction), and longitude the to X-axis (east-west direction).
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
#' if (file.exists("/Applications/SaTScan.app/Contents/app/satscan")) {
#'   results <- ww_run_satscan(
#'     .data = x,
#'     filename = "Locality",
#'     dir = directory,
#'     sslocation = "/Applications/SaTScan.app/Contents/app",
#'     ssbatchfilename = "satscan",
#'     satscan_version = "10.3.2",
#'     .scan_for = "high-low-rates",
#'     .gam_based = "wfhz",
#'     .by_area = FALSE,
#'     area = NULL,
#'     verbose = FALSE,
#'     cleanup = FALSE
#'   )
#' }
#' results
#'
#' @export
#'
ww_run_satscan <- function(
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
    area = NULL,
    cleanup = TRUE,
    verbose = FALSE) {
  # Capture column symbol for tidy eval
  area <- rlang::enquo(area)
  # ---- MULTIPLE-AREA MODE ----------------------------------------------------
  if (.by_area) {
    if (is.null(rlang::eval_tidy(area, .data))) {
      stop("`area` must be provided when `.by_area = TRUE`.")
    }

    # Get unique area values
    unique_areas <- dplyr::pull(dplyr::distinct(.data, !!area))

    # Initialize results list
    results <- list()

    for (i in seq_along(unique_areas)) {
      a <- unique_areas[i]
      df <- dplyr::filter(.data, !!area == a)
      area_filename <- a

      # Wrangle data
      do.call(
        ww_wrangle_data,
        list(
          .data = df,
          filename = area_filename,
          dir = dir,
          .gam_based = .gam_based
        )
      )

      # Configure SaTScan
      do.call(
        ww_configure_satscan,
        list(
          filename = area_filename,
          params_dir = params_dir,
          satscan_version = satscan_version,
          .scan_for = .scan_for
        )
      )

      # Run SaTScan
      res <- do.call(
        rsatscan::satscan,
        list(
          prmlocation = params_dir,
          prmfilename = area_filename,
          sslocation = sslocation,
          ssbatchfilename = ssbatchfilename,
          verbose = verbose,
          cleanup = cleanup
        )
      )

      # Store in list by area name
      results[[area_filename]] <- res
    }

    return(results)
  }

  # ---- SINGLE-AREA MODE ------------------------------------------------------
  # Wrangle data
  do.call(
    ww_wrangle_data,
    list(
      .data = .data,
      filename = filename,
      dir = dir,
      .gam_based = .gam_based
    )
  )

  # Configure SaTScan
  do.call(
    ww_configure_satscan,
    list(
      filename = filename,
      params_dir = params_dir,
      satscan_version = satscan_version,
      .scan_for = .scan_for
    )
  )

  # Run SaTScan
  result <- do.call(
    rsatscan::satscan,
    list(
      prmlocation = params_dir,
      prmfilename = filename,
      sslocation = sslocation,
      ssbatchfilename = ssbatchfilename,
      verbose = verbose,
      cleanup = cleanup
    )
  )

  result
}
