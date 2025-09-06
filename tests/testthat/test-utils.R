# ==============================================================================
# 📦 Functions: parse_clusters()
# ==============================================================================

# ------------------------------------------------------------------------------
# 🔒 Internal:
# ------------------------------------------------------------------------------

testthat::test_that(
  "All cluster-identified details get parsed into a data.frame",
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
    r <- ww_run_satscan(
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


    ### The tests ----
    testthat::expect_type(r, "list")
    testthat::expect_true(length(r) == 2)
    testthat::expect_true(inherits(r$.df, "tbl_df"))
    testthat::expect_true(inherits(r$.txt, "list"))
    testthat::expect_true(unique(dplyr::pull(r[[1]][1]) %in% c("Kotido", "Abim")))
    testthat::expect_true(all(names(r[[1]]) %in% c(
      c(
        "survey_area", "nr_EAs", "total_children", "total_cases", "%_cases",
        "location_ids", "geo", "radius", "span", "children", "n_cases", "expected_cases",
        "observedExpected", "relative_risk", "%_cases_in_area", "log_lik_ratio",
        "pvalue", "ipc_amn", "area"
      )
    )))
    testthat::expect_true(is.character(dplyr::pull(r[[1]][1])))
    testthat::expect_true(is.integer(dplyr::pull(r[[1]][2])))
    testthat::expect_true(is.integer(dplyr::pull(r[[1]][3])))
    testthat::expect_true(is.integer(dplyr::pull(r[[1]][4])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][5])))
    testthat::expect_true(is.character(dplyr::pull(r[[1]][6])))
    testthat::expect_true(is.character(dplyr::pull(r[[1]][7])))
    testthat::expect_true(is.character(dplyr::pull(r[[1]][8])))
    testthat::expect_true(is.character(dplyr::pull(r[[1]][9])))
    testthat::expect_true(is.integer(dplyr::pull(r[[1]][10])))
    testthat::expect_true(is.integer(dplyr::pull(r[[1]][11])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][12])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][13])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][14])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][15])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][16])))
    testthat::expect_true(is.double(dplyr::pull(r[[1]][17])))
    testthat::expect_true(is.character(dplyr::pull(r[[1]][18])))
  }
)
