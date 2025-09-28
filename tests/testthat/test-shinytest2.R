# ==============================================================================
# 📦 App
# ==============================================================================

testthat::test_that("App works as expected", {
  app <- AppDriver$new(
    app_dir = test_path("fixtures"),
    name = "data-uploading",
    load_timeout = 30000
  )

  # Wait for app to fully load
  app$wait_for_idle(timeout = 10000)

  ## ---- Navigate to Data Uploading Tab ---------------------------------------

  ### Click on the "Data Uploading" tab ----
  app$click(selector = "a[data-value='<strong>Data Uploading</strong>']")
  app$wait_for_idle(timeout = 5000)

  ### Get input data into data uploading tab ----
  #### Read file ----
  data <- read.csv(file = system.file("app", "anthro2.csv", package = "wowi"))
  tmpfile <- tempfile(fileext = ".csv")
  write.csv(data, tmpfile, row.names = FALSE)

  ### Upload to Data Uploading Tab, InputId = "upload" ----
  app$upload_file(
    upload = tmpfile,
    wait_ = TRUE
  )
  ### Wait for the upload to be processed ----
  app$wait_for_idle(timeout = 15000)

  ### Get a screenshot of output values to be compared with ----
  app$expect_values(output = TRUE)

  ### Get values ----
  vals <- app$get_values(
    input = "upload",
    output = c("fileUploaded", "showProgress")
  )
print(vals)
  ### Assert expectactions ----
  testthat::expect_true(vals$output$fileUploaded)
  testthat::expect_false(vals$output$showProgress)
  testthat::expect_gt(vals$input$upload$size, 219000)
  testthat::expect_equal(vals$input$upload$type, "text/csv")

  ## ---- Navigate to Data Wrangling Tab -----------------------------------------

  ### Click on the "Data Wrangling" tab
  app$click(selector = "a[data-value='<strong>Data Wrangling</strong>']")
  app$wait_for_idle(timeout = 5000)

  ### Set wrangle for wfhz ----
  app$set_inputs(wrangle = "wfhz", wait_ = FALSE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5000)

  ### Wait a bit more for the variable selectors to render
  Sys.sleep(2)

  ### Now set the variable selectors ----
  app$set_inputs(sex = "sex", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(weight = "weight", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(height = "height", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(oedema = "", wait_ = FALSE, timeout_ = 10000)

  ### Wait before clicking apply
  app$wait_for_idle(timeout = 5000)

  ### Click apply button
  app$click("apply_wrangle", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_idle(timeout = 10000)

  wfhz <- app$get_values()

  ### Assert expectectations ----
  testthat::expect_true("wrangled_data" %in% names(wfhz$output))
  testthat::expect_equal(wfhz$input$wrangle, expected = "wfhz")
  testthat::expect_equal(length(wfhz$input$wrangled_data_rows_all), 30)
  testthat::expect_equal(length(wfhz$input$wrangled_data_search_columns), 17)

  ### Set wrangle for MUAC ----
  app$set_inputs(wrangle = "muac", wait_ = FALSE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5000)

  ### Wait a bit more for the variable selectors to render ----
  Sys.sleep(5)

  ### Set variable selectors ----
  app$set_inputs(sex = "sex", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(age = "age", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(muac = "muac", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(oedema = "oedema", wait_ = FALSE, timeout_ = 10000)

  ### Wait before clicking apply ----
  app$wait_for_idle(timeout = 5000)

  ### Click apply button ----
  app$click("apply_wrangle", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_idle(timeout = 10000)

  ### Get values ----
  muac <- app$get_values()

  ### Assert expectations ----
  testthat::expect_equal(muac$input$wrangle, expected = "muac")
  testthat::expect_equal(length(muac$input$wrangled_data_rows_all), 30)
  testthat::expect_equal(length(muac$input$wrangled_data_search_columns), 18)

  ### Set wrangle for Combined ----
  app$set_inputs(wrangle = "combined", wait_ = FALSE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5000)

  ### Wait a bit more for the variable selectors to render ----
  Sys.sleep(5)

  ### Set variable selectors ----
  app$set_inputs(sex = "sex", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(age = "age", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(muac = "muac", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(weight = "weight", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(height = "height", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(oedema = "oedema", wait_ = FALSE, timeout_ = 10000)

  ### Wait before clicking apply ----
  app$wait_for_idle(timeout = 5000)

  ### Click apply button ----
  app$click("apply_wrangle", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_idle(timeout = 10000)

  ### Get values ----
  combined <- app$get_values()

  ### Assert expectations ----
  testthat::expect_equal(combined$input$wrangle, expected = "combined")
  testthat::expect_equal(length(combined$input$wrangled_data_rows_all), 30)
  testthat::expect_equal(length(combined$input$wrangled_data_search_columns), 20)

  ## ---- Navigate to Run Spatial Scan tab -------------------------------------

  ### Click on the "Data Wrangling" tab
  app$click(selector = "a[data-value='<strong>Run Spatial Scan</strong>']")
  app$wait_for_idle(timeout = 5000)

  ### Set wowi hyperparamters ----
  app$set_inputs(analysis_scope = "single-area", wait_ = FALSE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5000)

  ### Wait a bit more for the variable selectors to render
  Sys.sleep(2)

  ### Set parameters ----
  app$set_inputs(filename = "Kotido", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(directory = withr::local_tempdir(), wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(latitude = "latitude", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(longitude = "longitude", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(
    sslocation = "/Applications/SaTScan.app/Contents/app",
    wait_ = FALSE,
    timeout_ = 10000
  )
  app$set_inputs(ssbatchfilename = "satscan", wait_ = FALSE, timeout_ = 10000)
  app$set_inputs(scan_for = "high-rates", wait_ = FALSE, timeout_ = 10000)

  ### Wait before clicking apply ----
  app$wait_for_idle(timeout = 5000)

  ### Click apply button ----
  app$click("run_scan", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_idle(timeout = 10000)

  ### Get values ----
  app$expect_download(output = "downloadResults")
})
