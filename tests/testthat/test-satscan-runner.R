# ==============================================================================
# 📦 Functions: ww_run_satscan()
# ==============================================================================

# ------------------------------------------------------------------------------
# 🌐 External
# ------------------------------------------------------------------------------

## ---- Check if SaTScan params file is created --------------------------------
testthat::test_that(
  "Check if the function returns all expected outputs",
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
    out_dir <- file.path(tmp, "input-files") # this will be the dir

    ### Observed results ----
    results <- do.call(
      what = ww_run_satscan,
      args = list(
        .data = x,
        filename = "Locality",
        dir = out_dir,
        sslocation = "/Applications/SaTScan.app/Contents/app",
        satscan_version = "10.3.2",
        .scan_for = "high-low-rates",
        .gam_based = "wfhz",
        verbose = FALSE,
        cleanup = FALSE
      )
    )

    ## Expected files in the directory ----
    expected_files <- c(
      "Locality.cas", "Locality.clustermap.html", "Locality.col.dbf",
      "Locality.col.prj", "Locality.col.shp", "Locality.col.shx",
      "Locality.ctl", "Locality.geo", "Locality.gis.dbf", "Locality.gis.prj",
      "Locality.gis.shp", "Locality.gis.shx", "Locality.prm",
      "Locality.rr.dbf", "Locality.sci.dbf", "Locality.txt"
    )

    ### The tests ----
    testthat::expect_true(class(results) == "satscan")
    testthat::expect_false(is.null(results))
    testthat::expect_type(results, "list")
    testthat::expect_equal(
      object = length(list.files(file.path(tmp, "input-files"))),
      expected = 16
    )
    testthat::expect_setequal(
      object = list.files(file.path(tmp, "input-files")),
      expected = expected_files
    )
  }
)