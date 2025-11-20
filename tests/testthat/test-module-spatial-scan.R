# ==============================================================================
#  Test Suite: Spatial Scan
# ==============================================================================

## ---- Spatial Scan: Single-area analysis for high-rates ----------------------

testthat::test_that(
  desc = "Module spatial scan works great for single-area analysis for high rates",
  code = {
    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 20000,
      wait = TRUE
    )

    ### Click on Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    data <- read.csv(
      file = system.file("app", "anthro_.csv", package = "wowi"),
      sep = ","
    )
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(data, tmpfile, row.names = FALSE)

    ### Upload to Data Uploading Tab, InputId = "upload" ----
    app$upload_file("upload_data-upload" = tmpfile, wait_ = TRUE)
    ### Wait for the upload to be processed ----
    app$wait_for_idle(timeout = 15000)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 5000)

    ### Now set the variable selectors ----
    app$set_inputs("wrangle-sex" = "sex", wait_ = FALSE)
    app$set_inputs("wrangle-weight" = "weight", wait_ = FALSE)
    app$set_inputs("wrangle-height" = "height", wait_ = FALSE)
    app$set_inputs("wrangle-oedema" = "", wait_ = FALSE)

    ### Wait before clicking apply
    app$wait_for_idle(timeout = 5000)

    ### Click apply button
    app$click("wrangle-apply_wrangle", wait_ = FALSE)
    Sys.sleep(5)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Spatial Scan']")
    app$wait_for_idle(timeout = 5000)

    ### Set wowi hyperparamters: defaults to single-area analysis----
    Sys.sleep(2)

    ### Set parameters ----
    app$set_inputs("scan-filename" = "Kotido", wait_ = FALSE)
    app$set_inputs("scan-directory" = withr::local_tempdir(), wait_ = FALSE)
    app$set_inputs("scan-latitude" = "latitude", wait_ = FALSE)
    app$set_inputs("scan-longitude" = "longitude", wait_ = FALSE)
    app$set_inputs(
      "scan-sslocation" = "/Applications/SaTScan.app/Contents/app",
      wait_ = FALSE
    )
    app$set_inputs("scan-ssbatchfilename" = "satscan", wait_ = FALSE)
    app$set_inputs("scan-satscan_version" = "10.3.2", wait_ = FALSE)
    app$set_inputs("scan-scan_for" = "high-rates", wait_ = FALSE)

    ### Wait before clicking apply ----
    app$wait_for_idle(timeout = 5000)

    ### Click apply button ----
    app$click("scan-run_scan", wait_ = FALSE)
    Sys.sleep(5)

    testthat::expect_true(!is.null(app$get_values(output = TRUE)))
    cols <- app$get_js("
    $('#scan-clusters thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()
    Sys.sleep(5)

    testthat::expect_gt(length(cols), 0)
    testthat::expect_true(any(cols == "geo"))

    #### Stop the app ----
    app$stop()
  }
)

## ---- Spatial Scan: Single-area analysis for high and low rates --------------

testthat::test_that(
  desc = "Module spatial scan works great for single-area analysis for high and low",
  code = {
    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 20000,
      wait = TRUE
    )

    ### Click on Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    data <- read.csv(
      file = system.file("app", "anthro_.csv", package = "wowi"),
      sep = ","
    )
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(data, tmpfile, row.names = FALSE)

    ### Upload to Data Uploading Tab, InputId = "upload" ----
    app$upload_file("upload_data-upload" = tmpfile, wait_ = TRUE)
    ### Wait for the upload to be processed ----
    app$wait_for_idle(timeout = 15000)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 5000)

    ### Now set the variable selectors ----
    app$set_inputs("wrangle-sex" = "sex", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-weight" = "weight", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-height" = "height", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-oedema" = "", wait_ = FALSE, timeout_ = 10000)

    ### Wait before clicking apply
    app$wait_for_idle(timeout = 5000)

    ### Click apply button
    app$click("wrangle-apply_wrangle", wait_ = TRUE, timeout_ = 15000)
    Sys.sleep(5)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Spatial Scan']")
    app$wait_for_idle(timeout = 5000)

    ### Set wowi hyperparamters: defaults to single-area analysis----
    Sys.sleep(2)

    ### Set parameters ----
    app$set_inputs("scan-filename" = "Kotido", wait_ = FALSE)
    app$set_inputs("scan-directory" = withr::local_tempdir(), wait_ = FALSE)
    app$set_inputs("scan-latitude" = "latitude", wait_ = FALSE)
    app$set_inputs("scan-longitude" = "longitude", wait_ = FALSE)
    app$set_inputs(
      "scan-sslocation" = "/Applications/SaTScan.app/Contents/app",
      wait_ = FALSE
    )
    app$set_inputs("scan-ssbatchfilename" = "satscan", wait_ = FALSE)
    app$set_inputs("scan-satscan_version" = "10.3.2", wait_ = FALSE)
    app$set_inputs("scan-scan_for" = "high-low-rates", wait_ = FALSE)

    ### Wait before clicking apply ----
    app$wait_for_idle(timeout = 5000)

    ### Click apply button ----
    app$click("scan-run_scan", wait_ = FALSE)
    Sys.sleep(5)

    testthat::expect_true(!is.null(app$get_values(output = TRUE)))
    cols <- app$get_js("
    $('#scan-clusters thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()
    Sys.sleep(5)

    testthat::expect_gt(length(cols), 0)
    testthat::expect_true(any(cols == "geo"))

    #### Stop the app ----
    app$stop()
  }
)


## ---- Spatial Scan: Single-area analysis for high-rates ----------------------

testthat::test_that(
  desc = "Module spatial scan works great for multiple-area analysis for high rates",
  code = {
    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 20000,
      wait = TRUE
    )

    ### Click on Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    data <- read.csv(
      file = system.file("app", "anthro_.csv", package = "wowi"),
      sep = ","
    )
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(data, tmpfile, row.names = FALSE)

    ### Upload to Data Uploading Tab, InputId = "upload" ----
    app$upload_file("upload_data-upload" = tmpfile, wait_ = TRUE)
    ### Wait for the upload to be processed ----
    app$wait_for_idle(timeout = 15000)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 5000)

    ### Now set the variable selectors ----
    app$set_inputs("wrangle-sex" = "sex", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-weight" = "weight", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-height" = "height", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-oedema" = "", wait_ = FALSE, timeout_ = 10000)

    ### Wait before clicking apply
    app$wait_for_idle(timeout = 5000)

    ### Click apply button
    app$click("wrangle-apply_wrangle", wait_ = TRUE, timeout_ = 15000)
    Sys.sleep(5)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Spatial Scan']")
    app$wait_for_idle(timeout = 5000)

    ### Set wowi hyperparamters: defaults to single-area analysis----
    app$set_inputs("scan-analysis_scope" = "multiple-area", wait_ = FALSE)
    Sys.sleep(2)

    ### Set parameters ----
    app$set_inputs("scan-area" = "district", wait_ = FALSE)
    app$set_inputs("scan-directory" = withr::local_tempdir(), wait_ = FALSE)
    app$set_inputs("scan-latitude" = "latitude", wait_ = FALSE)
    app$set_inputs("scan-longitude" = "longitude", wait_ = FALSE)
    app$set_inputs(
      "scan-sslocation" = "/Applications/SaTScan.app/Contents/app",
      wait_ = FALSE
    )
    app$set_inputs("scan-ssbatchfilename" = "satscan", wait_ = FALSE)
    app$set_inputs("scan-satscan_version" = "10.3.2", wait_ = FALSE)
    app$set_inputs("scan-scan_for" = "high-rates", wait_ = FALSE)

    ### Wait before clicking apply ----
    app$wait_for_idle(timeout = 5000)

    ### Click apply button ----
    app$click("scan-run_scan", wait_ = FALSE)
    Sys.sleep(5)

    testthat::expect_true(!is.null(app$get_values(output = TRUE)))
    cols <- app$get_js("
    $('#scan-clusters thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()
    Sys.sleep(5)

    testthat::expect_gt(length(cols), 0)
    testthat::expect_true(any(cols == "geo"))

    #### Stop the app ----
    app$stop()
  }
)

## ---- Spatial Scan: Multiple-area analysis for high and low rates --------------

testthat::test_that(
  desc = "Module spatial scan works great for multiple-area analysis for high and low",
  code = {
    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 20000,
      wait = TRUE
    )

    ### Click on Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    data <- read.csv(
      file = system.file("app", "anthro_.csv", package = "wowi"),
      sep = ","
    )
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(data, tmpfile, row.names = FALSE)

    ### Upload to Data Uploading Tab, InputId = "upload" ----
    app$upload_file("upload_data-upload" = tmpfile, wait_ = TRUE)
    ### Wait for the upload to be processed ----
    app$wait_for_idle(timeout = 15000)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 5000)

    ### Now set the variable selectors ----
    app$set_inputs("wrangle-sex" = "sex", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-weight" = "weight", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-height" = "height", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs("wrangle-oedema" = "", wait_ = FALSE, timeout_ = 10000)

    ### Wait before clicking apply
    app$wait_for_idle(timeout = 5000)

    ### Click apply button
    app$click("wrangle-apply_wrangle", wait_ = TRUE, timeout_ = 15000)
    Sys.sleep(5)

    ### Click on the "Data Wrangling" tab
    app$click(selector = "a[data-value='Spatial Scan']")
    app$wait_for_idle(timeout = 5000)

    ### Set wowi hyperparamters: defaults to single-area analysis----
    app$set_inputs("scan-analysis_scope" = "multiple-area", wait_ = FALSE)
    Sys.sleep(2)

    ### Set parameters ----
    app$set_inputs("scan-area" = "district", wait_ = FALSE)
    app$set_inputs("scan-directory" = withr::local_tempdir(), wait_ = FALSE)
    app$set_inputs("scan-latitude" = "latitude", wait_ = FALSE)
    app$set_inputs("scan-longitude" = "longitude", wait_ = FALSE)
    app$set_inputs(
      "scan-sslocation" = "/Applications/SaTScan.app/Contents/app",
      wait_ = FALSE
    )
    app$set_inputs("scan-ssbatchfilename" = "satscan", wait_ = FALSE)
    app$set_inputs("scan-satscan_version" = "10.3.2", wait_ = FALSE)
    app$set_inputs("scan-scan_for" = "high-low-rates", wait_ = FALSE)

    ### Wait before clicking apply ----
    app$wait_for_idle(timeout = 5000)

    ### Click apply button ----
    app$click("scan-run_scan", wait_ = FALSE)
    Sys.sleep(5)

    testthat::expect_true(!is.null(app$get_values(output = TRUE)))
    cols <- app$get_js("
    $('#scan-clusters thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()
    Sys.sleep(5)

    testthat::expect_gt(length(cols), 0)
    testthat::expect_true(any(cols == "geo"))

    #### Stop the app ----
    app$stop()
  }
)
