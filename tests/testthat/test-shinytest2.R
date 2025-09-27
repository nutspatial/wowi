# test_that("{shinytest2} recording: app-data-upload", {

#     ## Initialise wowi Shiny application ----
#   wow <- ww_run_app()

#     app <- AppDriver$new()

#     ## Upload data into data uploading tab ----
#     ### Read file ----
#     data <- read.csv(file = "inst/wowi/app/tests/testthat/anthro2.csv")
#     tmpfile <- tempfile(fileext = ".csv")
#     write.csv(data, tmpfile, row.names = FALSE)

#     ### Upload to Data Uploading Tab, InputId = "upload" ----
#     app$upload_file(upload = tmpfile, wait_ = TRUE)

#     ### Get a screenshot of the data uploading tab ----
#     app$get_screenshot()



#     ## Data wrangling tab ----
#     ### Wrangle weight-for-height data ----
#     app$set_inputs(wrangle = "wfhz")
#     app$set_inputs(sex = "sex")
#     app$set_inputs(weight = "weight")
#     app$set_inputs(height = "height")
#     app$set_inputs(oedema = "")
#     app$click("apply_wrangle")
#     app$wait_for_idle()

#     ### Set parameters for run scan ----
#     # app$set_inputs(analysis_scope = "multiple-area")
#     # app$set_inputs(area = "district")
#     # app$set_inputs(directory = "/Users/tomaszaba/Desktop/wowi")
#     # app$set_inputs(latitude = "longitude")
#     # app$set_inputs(longitude = "latitude")
#     # app$set_inputs(sslocation = "/Applications/SaTScan.app/Contents/app")
#     # app$set_inputs(ssbatchfilename = "satscan")
#     # app$set_inputs(satscan_version = "10.3.2")
#     # app$click("run_scan")
#     # app$wait_for_idle()

#     app$stop()
# })

# Test 1: Basic App Loading and Navigation
testthat::test_that("App uploads input data as expected", {
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
  data <- read.csv(file = here::here("inst", "app", "anthro2.csv"))
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

  ### Run tests ----
  testthat::expect_true(vals$output$fileUploaded)
  testthat::expect_false(vals$output$showProgress)
  testthat::expect_equal(vals$input$upload$size, 219727)
  testthat::expect_equal(vals$input$upload$type, "text/csv")

## ---- Navigate to Data Wrangling Tab -----------------------------------------
  
  ### Click on the "Data Wrangling" tab
  app$click(selector = "a[data-value='<strong>Data Wrangling</strong>']")
  app$wait_for_idle(timeout = 5000)
  
  ### Set wrangle for wfhz ----
  app$set_inputs(wrangle = "wfhz", wait_ = FALSE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5000)
  
  # Wait a bit more for the variable selectors to render
  Sys.sleep(2)
  
  # Now set the variable selectors ----
    app$set_inputs(sex = "sex", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs(weight = "weight", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs(height = "height", wait_ = FALSE, timeout_ = 10000)
    app$set_inputs(oedema = "", wait_ = FALSE, timeout_ = 10000)
    
    # Wait before clicking apply
    app$wait_for_idle(timeout = 5000)
    
    # Click apply button
    app$click("apply_wrangle", wait_ = TRUE, timeout_ = 15000)
    app$wait_for_idle(timeout = 10000)
    
    wfhz <- app$get_values()
  
    # Add your assertions here for the wrangled data
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
  print(str(muac))

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

    testthat::expect_equal(combined$input$wrangle, expected = "combined")
  testthat::expect_equal(length(combined$input$wrangled_data_rows_all), 30)
  testthat::expect_equal(length(combined$input$wrangled_data_search_columns), 20)
})
