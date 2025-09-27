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

  ### Get a screenshot of the data uploading tab ----
  #app$get_screenshot()

    ### Get output values of data uploading tab ----
    app$expect_values(output = TRUE)
    vals <- app$get_values()

    ### Run tests ----
  testthat::expect_true(vals$output$fileUploaded)
  testthat::expect_false(vals$output$showProgress)
  testthat::expect_equal(vals$input$upload$size, 219727)
  testthat::expect_equal(vals$input$upload$type, "text/csv")
    #testthat::expect_equal(vals$output$uploadedDataTable, "data.frame")



  # # Test that app loads - check for wowi in the page content
  # page_content <- app$get_html("body")
  # expect_true(grepl("wowi", page_content, ignore.case = TRUE))

  # # Test that app loads
  # expect_true(app$get_text("title") |> grepl("wowi", x = _))
  
  # # Test navigation to different tabs
  # app$click("nav-tab-Data Uploading")
  # app$expect_values(output = "fileUploaded")
  
  # app$click("nav-tab-Data Wrangling") 
  # app$expect_values(input = "wrangle")
  
  # app$click("nav-tab-Run Spatial Scan")
  # app$expect_values(input = "analysis_scope")
  
  # Take screenshot of final state
  #app$expect_screenshot()
})

# # Test 2: Data Upload Functionality
# test_that("Data upload works correctly", {
#   app <- AppDriver$new(name = "data-upload")
  
#   # Navigate to upload tab
#   app$click("nav-tab-Data Uploading")
  
#   # Create a sample CSV file for testing
#   temp_csv <- tempfile(fileext = ".csv")
#   sample_data <- data.frame(
#     age = c(12, 24, 36, 48, 60),
#     sex = c(1, 2, 1, 2, 1),
#     weight = c(8.5, 10.2, 12.1, 14.5, 16.2),
#     height = c(75.2, 85.1, 92.5, 98.2, 105.1),
#     muac = c(12.5, 13.2, 14.1, 14.8, 15.2),
#     oedema = c("n", "n", "n", "n", "n"),
#     latitude = c(-1.2921, -1.2922, -1.2923, -1.2924, -1.2925),
#     longitude = c(36.8219, 36.8220, 36.8221, 36.8222, 36.8223)
#   )
#   write.csv(sample_data, temp_csv, row.names = FALSE)
  
#   # Upload the file
#   app$upload_file(upload = temp_csv)
  
#   # Wait for processing to complete
#   app$wait_for_value(output = "fileUploaded", ignore = list(NULL, FALSE))
  
#   # Check that file info is displayed
#   file_info <- app$get_value(output = "fileInfo")
#   expect_true(grepl("Rows: 5", file_info))
#   expect_true(grepl("Columns: 8", file_info))
  
#   # Check that data table is populated
#   expect_true(app$get_value(output = "fileUploaded"))
  
#   app$expect_screenshot()
  
#   # Cleanup
#   unlink(temp_csv)
# })

# # Test 3: Data Wrangling Functionality  
# test_that("Data wrangling works for different methods", {
#   app <- AppDriver$new(name = "data-wrangling")
  
#   # First upload data
#   app$click("nav-tab-Data Uploading")
  
#   temp_csv <- tempfile(fileext = ".csv")
#   sample_data <- data.frame(
#     age = c(12, 24, 36, 48, 60),
#     sex = c(1, 2, 1, 2, 1),
#     weight = c(8.5, 10.2, 12.1, 14.5, 16.2),
#     height = c(75.2, 85.1, 92.5, 98.2, 105.1),
#     muac = c(12.5, 13.2, 14.1, 14.8, 15.2),
#     oedema = c("n", "n", "n", "n", "n"),
#     latitude = c(-1.2921, -1.2922, -1.2923, -1.2924, -1.2925),
#     longitude = c(36.8219, 36.8220, 36.8221, 36.8222, 36.8223)
#   )
#   write.csv(sample_data, temp_csv, row.names = FALSE)
#   app$upload_file(upload = temp_csv)
#   app$wait_for_value(output = "fileUploaded", ignore = list(NULL, FALSE))
  
#   # Navigate to wrangling tab
#   app$click("nav-tab-Data Wrangling")
  
#   # Test WFHZ method
#   app$set_inputs(wrangle = "wfhz")
#   app$wait_for_idle()
  
#   # Select variables for WFHZ
#   app$set_inputs(sex = "sex")
#   app$set_inputs(weight = "weight") 
#   app$set_inputs(height = "height")
#   app$set_inputs(oedema = "oedema")
  
#   # Apply wrangling
#   app$click("apply_wrangle")
#   app$wait_for_idle(timeout = 10000) # Wait up to 10 seconds
  
#   # Test MUAC method
#   app$set_inputs(wrangle = "muac")
#   app$wait_for_idle()
  
#   app$set_inputs(age = "age")
#   app$set_inputs(sex = "sex")
#   app$set_inputs(muac = "muac")
#   app$set_inputs(oedema = "oedema")
  
#   app$click("apply_wrangle")
#   app$wait_for_idle(timeout = 10000)
  
#   app$expect_screenshot()
  
#   unlink(temp_csv)
# })

# # Test 4: Spatial Scan Configuration
# test_that("Spatial scan configuration works", {
#   app <- AppDriver$new(name = "spatial-scan-config")
  
#   # Upload and wrangle data first
#   app$click("nav-tab-Data Uploading")
  
#   temp_csv <- tempfile(fileext = ".csv")
#   sample_data <- data.frame(
#     area = c("District A", "District A", "District B", "District B", "District C"),
#     age = c(12, 24, 36, 48, 60),
#     sex = c(1, 2, 1, 2, 1),
#     weight = c(8.5, 10.2, 12.1, 14.5, 16.2),
#     height = c(75.2, 85.1, 92.5, 98.2, 105.1),
#     muac = c(12.5, 13.2, 14.1, 14.8, 15.2),
#     oedema = c("n", "n", "n", "n", "n"),
#     latitude = c(-1.2921, -1.2922, -1.2923, -1.2924, -1.2925),
#     longitude = c(36.8219, 36.8220, 36.8221, 36.8222, 36.8223)
#   )
#   write.csv(sample_data, temp_csv, row.names = FALSE)
#   app$upload_file(upload = temp_csv)
#   app$wait_for_value(output = "fileUploaded", ignore = list(NULL, FALSE))
  
#   # Quick wrangling
#   app$click("nav-tab-Data Wrangling")
#   app$set_inputs(wrangle = "wfhz")
#   app$wait_for_idle()
#   app$set_inputs(sex = "sex", weight = "weight", height = "height", oedema = "oedema")
#   app$click("apply_wrangle")
#   app$wait_for_idle(timeout = 10000)
  
#   # Navigate to spatial scan tab
#   app$click("nav-tab-Run Spatial Scan")
  
#   # Test single-area analysis configuration
#   app$set_inputs(analysis_scope = "single-area")
#   app$wait_for_idle()
  
#   # Check that single-area inputs are available
#   expect_true("filename" %in% names(app$get_values()$input))
  
#   # Test multiple-area analysis configuration  
#   app$set_inputs(analysis_scope = "multiple-area")
#   app$wait_for_idle()
  
#   # Check that multiple-area inputs are available
#   expect_true("area" %in% names(app$get_values()$input))
  
#   app$expect_screenshot()
  
#   unlink(temp_csv)
# })

# # Test 5: Input Validation
# test_that("Input validation works correctly", {
#   app <- AppDriver$new(name = "input-validation")
  
#   # Upload data
#   app$click("nav-tab-Data Uploading")
  
#   temp_csv <- tempfile(fileext = ".csv")
#   sample_data <- data.frame(
#     age = c(12, 24, 36),
#     sex = c(1, 2, 1),
#     weight = c(8.5, 10.2, 12.1),
#     height = c(75.2, 85.1, 92.5)
#   )
#   write.csv(sample_data, temp_csv, row.names = FALSE)
#   app$upload_file(upload = temp_csv)
#   app$wait_for_value(output = "fileUploaded", ignore = list(NULL, FALSE))
  
#   # Navigate to wrangling and try to wrangle without selecting all required variables
#   app$click("nav-tab-Data Wrangling")
#   app$set_inputs(wrangle = "wfhz")
#   app$wait_for_idle()
  
#   # Only select some variables (missing height)
#   app$set_inputs(sex = "sex")
#   app$set_inputs(weight = "weight")
#   # Intentionally leave height empty
  
#   # Try to apply wrangling - should show error
#   app$click("apply_wrangle")
#   app$wait_for_idle()
  
#   # The app should show a notification error
#   # We can't easily test notifications in shinytest2, but we can verify
#   # that wrangling didn't complete by checking that wrangled data table is empty
  
#   app$expect_screenshot()
  
#   unlink(temp_csv)
# })

# # Test 6: UI Responsiveness
# test_that("UI elements respond correctly to user actions", {
#   app <- AppDriver$new(name = "ui-responsiveness")
  
#   # Test that wrangling method selection updates UI
#   app$click("nav-tab-Data Wrangling")
  
#   # Start with WFHZ
#   app$set_inputs(wrangle = "wfhz")
#   app$wait_for_idle()
  
#   # Switch to MUAC and verify UI changes
#   app$set_inputs(wrangle = "muac") 
#   app$wait_for_idle()
  
#   # Switch to combined
#   app$set_inputs(wrangle = "combined")
#   app$wait_for_idle()
  
#   # Test spatial scan scope changes
#   app$click("nav-tab-Run Spatial Scan")
  
#   app$set_inputs(analysis_scope = "single-area")
#   app$wait_for_idle()
  
#   app$set_inputs(analysis_scope = "multiple-area")
#   app$wait_for_idle()
  
#   app$expect_screenshot()
# })