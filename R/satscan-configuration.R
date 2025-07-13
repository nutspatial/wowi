#'
#'
#' Configure SaTScan for a Bernoulli purely spatial scan
#'
#'

ww_configure_satscan <- function(
    filename = character(),
    params_destfile = character(),
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
      "⚠️ Your version of SaTScan is older than the latest available (v10.3.2). \nThis may cause errors. Consider updating to the latest version: https://www.satscan.org"
    )
  }

  ## Configure SaTScan ----

  ### Set the corresponding SaTScan parameter for high-rates and high-low ----
  scan_areas <- if (.scan_for == "high-rates") 1 else 3
  print(scan_areas)

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
      location = params_destfile,
      filename = filename,
      matchout = TRUE
    )
  )
}