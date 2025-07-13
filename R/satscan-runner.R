ww_run_satscan <- function(
    .data,
    filename = character(),
    dir = character(),
    params_dir = dir,
    sslocation = character(),
    satscan_version,
    cleanup = TRUE,
    verbose = FALSE,
    .scan_for = c("high-rates", "high-low-rates"),
    .gam_based = c("wfhz", "muac", "combined")
  ) {
  
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
      ssbatchfilename = "satscan",
      verbose = verbose,
      cleanup = cleanup
    )
  )

  ## Return 
  results
}