# ==============================================================================
# 📦 Functions: ww_configure_satscan()
# ==============================================================================

# ------------------------------------------------------------------------------
# 🌐 External
# ------------------------------------------------------------------------------

## ---- Check if SaTScan params file is created --------------------------------
testthat::test_that(
  "Check if SaTScan parameter file has been created and placed in the working dir",
  {
    library(rsatscan)
    ### Sample data ----
    x <- df |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = TRUE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = edema
      )

    ### Create a temporary directory ----
    tmp <- withr::local_tempdir() # ensures cleanup after test
    out_dir <- file.path(tmp, "input-files") # this will be the destfile

    ### Observed results ----
    #### Create input files ----
    do.call(
      what = ww_wrangle_data,
      args = list(
        .data = x,
        filename = "localityA",
        destfile = out_dir,
        .gam_based = "wfhz"
      )
    )

    #### Configure SaTScan ----
    do.call(
      what = ww_configure_satscan,
      args = list(
        filename = "Locality",
        params_destfile = out_dir,
        satscan_version = "10.3.2",
        .scan_for = "high-low-rates"
      )
    )

    ### The tests ----
    testthat::expect_true(file.exists(file.path(out_dir, "Locality.prm")))
    testthat::expect_warning(
      object =  do.call(
      what = ww_configure_satscan,
      args = list(
        filename = "Locality",
        params_destfile = out_dir,
        satscan_version = "10.3.1",
        .scan_for = "high-rates"
      )
    ), 
    regexp = "Your version of SaTScan is older than the latest available (v10.3.2).\nThis may cause errors. Consider updating to the latest version: https://www.satscan.org",
    fixed = TRUE
    )
  }
)