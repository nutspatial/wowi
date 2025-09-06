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
    ### Observed results ----
    skip_if_no_satscan() # Skip test if SaTScan GUI is not found during GitHub Actions
    ### Sample data ----
    w <- anthro |>
      dplyr::filter(district == "Kotido") |>
      dplyr::rename(longitude = y, latitude = x) |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = FALSE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = oedema
      )

    ### Create a temporary directory ----
    tmp <- withr::local_tempdir() # ensures cleanup after test
    out_dir <- file.path(tmp, "input-files") # this will be the dir

    ### Observed results ----
    skip_if_no_satscan() # Skip test if SaTScan GUI is not found during GitHub Actions
    results <- ww_run_satscan(
      .data = w,
      filename = "Kotido",
      dir = out_dir,
      sslocation = "/Applications/SaTScan.app/Contents/app",
      ssbatchfilename = "satscan",
      satscan_version = "10.3.2",
      .scan_for = "high-low-rates",
      .gam_based = "wfhz",
      verbose = FALSE,
      cleanup = FALSE,
      .by_area = FALSE,
      area = NULL
    )

    ## Expected files in the directory ----
    expected_files <- c(
      "Kotido.cas", "Kotido.clustermap.html", "Kotido.col.dbf",
      "Kotido.col.prj", "Kotido.col.shp", "Kotido.col.shx",
      "Kotido.ctl", "Kotido.geo", "Kotido.gis.dbf", "Kotido.gis.prj",
      "Kotido.gis.shp", "Kotido.gis.shx", "Kotido.prm",
      "Kotido.rr.dbf", "Kotido.sci.dbf", "Kotido.txt"
    )

    ### The tests ----
    testthat::expect_true(inherits(results$.df, "tbl_df"))
    testthat::expect_true(inherits(results$.txt, "character"))
    testthat::expect_false(is.null(results[[2]]))
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


## ---- Does the function iterate over a multiple-area -------------------------
testthat::test_that(
  "Check if the function returns all expected outputs from a multiple-area analysis",
  {
    library(rsatscan)
    ### Sample data ----
    w <- anthro |>
      dplyr::filter(district == "Abim" | district == "Kotido") |>
      dplyr::rename(longitude = y, latitude = x) |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = FALSE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = oedema
      )

    ### Create a temporary directory ----
    tmp <- withr::local_tempdir() # ensures cleanup after test
    out_dir <- file.path(tmp, "uga-files") # this will be the dir

    ### Observed results ----
    skip_if_no_satscan() # Skip test if SaTScan GUI is not found during GitHub Actions
    results <- ww_run_satscan(
      .data = w,
      filename = NULL,
      dir = out_dir,
      sslocation = "/Applications/SaTScan.app/Contents/app",
      ssbatchfilename = "satscan",
      satscan_version = "10.3.2",
      .scan_for = "high-low-rates",
      .gam_based = "wfhz",
      verbose = FALSE,
      cleanup = FALSE,
      .by_area = TRUE,
      area = district
    )

    ## Expected files in the directory ----
    expected_kotido <- c(
      "Kotido.cas", "Kotido.clustermap.html", "Kotido.col.dbf",
      "Kotido.col.prj", "Kotido.col.shp", "Kotido.col.shx",
      "Kotido.ctl", "Kotido.geo", "Kotido.gis.dbf", "Kotido.gis.prj",
      "Kotido.gis.shp", "Kotido.gis.shx", "Kotido.prm",
      "Kotido.rr.dbf", "Kotido.sci.dbf", "Kotido.txt",
      "Abim.cas", "Abim.clustermap.html", "Abim.col.dbf",
      "Abim.col.prj", "Abim.col.shp", "Abim.col.shx",
      "Abim.ctl", "Abim.geo", "Abim.gis.dbf", "Abim.gis.prj",
      "Abim.gis.shp", "Abim.gis.shx", "Abim.prm",
      "Abim.rr.dbf", "Abim.sci.dbf", "Abim.txt"
    )

    ### The tests ----
    testthat::expect_true(inherits(results[[2]], "list"))
    testthat::expect_true(inherits(results[[1]], "tbl"))
    testthat::expect_false(is.null(results))
    testthat::expect_type(results, "list")
    testthat::expect_equal(
      object = length(list.files(file.path(tmp, "uga-files"))),
      expected = 32
    )
    testthat::expect_setequal(
      object = list.files(file.path(tmp, "uga-files")),
      expected = expected_kotido
    )
  }
)

## ---- Does the function throw an error ---------------------------------------
testthat::test_that(
  "Check if the function throws an error when `area = NULL` while `.by_area = TRUE`",
  {
    library(rsatscan)
    ### Sample data ----
    w <- anthro |>
      dplyr::filter(district == "Abim" | district == "Kotido") |>
      dplyr::rename(longitude = y, latitude = x) |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = FALSE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = oedema
      )

    ### Create a temporary directory ----
    tmp <- withr::local_tempdir() # ensures cleanup after test
    out_dir <- file.path(tmp, "uga-files") # this will be the dir

    ### Observed results ----
    skip_if_no_satscan() # Skip test if SaTScan GUI is not found during GitHub Actions

    ### The tests ----
    testthat::expect_error(
      ww_run_satscan(
        .data = w,
        filename = NULL,
        dir = out_dir,
        sslocation = "/Applications/SaTScan.app/Contents/app",
        ssbatchfilename = "satscan",
        satscan_version = "10.3.2",
        .scan_for = "high-low-rates",
        .gam_based = "wfhz",
        verbose = FALSE,
        cleanup = FALSE,
        .by_area = TRUE,
        area = NULL
      ),
      regexp = "`area` must be provided when `.by_area = TRUE`.",
      fixed = FALSE
    )
  }
)
