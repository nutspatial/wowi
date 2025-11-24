#'
#'
#' Configure SaTScan for Bernoulli purely spatial scan
#'
#' @description
#' Define the analysis parameters required by SaTScan's GUI to conduct a
#' Bernoulli-based purely spatial scan to detect either clusters of high rates
#' of acute malnutrition or both high and low rates.
#'
#' User's input is limited to specifying the analysis area filename, the destination
#' directory for the parameters file, the SaTScan version in use, and the type of
#' clusters to be detected. All other parameters are pre-defined by this function.
#'
#' @param filename A quoted string identifying the analysis area.
#'
#' @param params_dir A quoted string of the folder or directory in which the
#' parameters file (produced by this function) should be saved. This can be
#' the same directory as that specified in [ww_wrangle_data()].
#'
#' @param satscan_version A quoted string indicating the version of SaTScan
#' installed on the user's computer. Internally, this value is checked against
#' the latest available version. If it is older, a warning is issued with a link
#' to SaTScan’s website. Although the analysis is not interrupted, it is
#' recommended to use the latest version.
#'
#' @param .scan_for A quoted string indicating the type of clusters to scan for.
#' To scan for clusters of high rates only, set `.scan_for = "high-rates"`.
#' To scan for both high and low rates, set `.scan_for = "high-low-rates"`.
#'
#' @details
#' For more information on Bernoulli purely spatial scans, refer to the
#' SaTScan technical documentation available at: <https://www.satscan.org/techdoc.html>.
#'
#' @references
#' Kulldorff, M. (2022) *SaTScan user guide for version 10.1*. Available at: <https://www.satscan.org/>.
#'
#' @examples
#' ## Given a temporary directory ----
#' tmp <- withr::local_tempdir()
#' directory <- file.path(tmp, "input-files")
#'
#' ## Wrangle data with `{mwana}` ----
#' x <- anthro |>
#'   dplyr::rename(longitude = y, latitude = x) |>
#'   mwana::mw_wrangle_wfhz(
#'     sex = sex,
#'     .recode_sex = TRUE,
#'     weight = weight,
#'     height = height
#'   ) |>
#'   mwana::define_wasting(
#'     zscores = wfhz,
#'     .by = "zscores",
#'     edema = oedema
#'   )
#'
#' ## Apply the function ----
#' ww_wrangle_data(
#'   .data = x,
#'   filename = "Locality",
#'   dir = directory,
#'   .gam_based = "wfhz",
#'   latitude = latitude,
#'   longitude = longitude
#' )
#'
#' library(rsatscan) # important to make `{wowi}` access `{rsatscan}`-specific eviroment
#'
#' #### Configure SaTScan ----
#' do.call(
#'   what = ww_configure_satscan,
#'   args = list(
#'     filename = "Locality",
#'     params_dir = directory,
#'     satscan_version = "10.3.2",
#'     .scan_for = "high-low-rates"
#'   )
#' )
#'
#' ## Show file's content ----
#' file.show(file.path(tmp, "input-files/Locality.prm"))
#'
#' @export
#'
ww_configure_satscan <- function(
    filename = character(),
    params_dir = character(),
    satscan_version = character(),
    .scan_for = c("high-rates", "high-low-rates")) {
  ## Enforce options in `.scan_for` ----
  .scan_for <- match.arg(.scan_for)

  ## Set start and end dates ----
  startdate <- format(Sys.Date(), "%Y/%m/%d")
  enddate <- format(Sys.Date(), "%Y/%m/%d")

  ## Check SaTScan software version ----
  satscan_june2025 <- "10.3.2"
  if (satscan_version < satscan_june2025) {
    warning(
      "Your version of SaTScan is older than the latest available (v10.3.2).\nThis may cause errors. Consider updating to the latest version: https://www.satscan.org"
    )
  }

  ## Configure SaTScan ----
  ### Set the corresponding SaTScan parameter for high-rates and high-low ----
  scan_areas <- if (.scan_for == "high-rates") 1 else 3

  ### Reset parameter file ----
  invisible(ss.options(reset = TRUE, version = satscan_version))

  ### Set parameters as in SaTScan input tab ----
  do.call(
    what = ss.options,
    args = list(
      invals = list(
        CaseFile = do.call(what = paste0, args = list(filename, ".cas")),
        ControlFile = do.call(what = paste0, args = list(filename, ".ctl")),
        CoordinatesFile = do.call(what = paste0, args = list(filename, ".geo")),
        CoordinatesType = 1 # Latitude/Longitude
      )
    )
  )

  ### Set parameters as in SaTScan analysis tab ----
  do.call(
    what = ss.options,
    args = list(
      invals = list(
        AnalysisType = 1, # Purely Spatial
        ModelType = 1, # Bernoulli
        ScanAreas = scan_areas,
        MaxSpatialSizeInPopulationAtRisk = 50,
        SpatialWindowShapeType = 0,
        PrecisionCaseTimes = 0,
        StartDate = startdate,
        EndDate = enddate
      )
    )
  )

  ### Set parameters as in SaTScan output tab ----
  do.call(
    what = ss.options,
    args = list(
      invals = list(
        ResultsTitle = "results",
        LaunchMapViewer = "n",
        CompressKMLtoKMZ = "n",
        IncludeClusterLocationsKML = "y",
        ReportHierarchicalClusters = "y" # To get nested clusters, if any.
      )
    )
  )

  ### Write parameter file ----
  do.call(
    what = write.ss.prm,
    args = list(
      location = params_dir,
      filename = filename,
      matchout = TRUE
    )
  )
}
