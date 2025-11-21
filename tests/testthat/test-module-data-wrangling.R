# ==============================================================================
# 📦 Shiny Module: Data Wrangling
# ==============================================================================

# ------------------------------------------------------------------------------
# 🔒 Internal:
# ------------------------------------------------------------------------------

## ---- Data Wrangling: WFHZ ---------------------------------------------------

testthat::test_that(
  desc = "Data wrangling server works as expected for WFHZ",
  code = {

    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 10000,
      wait = TRUE
    )

    ### Click on Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    ### Read in data and upload ----
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

    ### Wait until wrangling is completed ----
    app$wait_for_value(output = "wrangle-wrangled_data", timeout = 40000)

    ### Get all values ----
    wfhz <- app$get_values()

    ### Get wrangled df ----
    vals <- app$get_js("
    $('#wrangle-wrangled_data thead th').map(function() {
      return $(this).text();
    }).get();
  ")[-1] |> as.character()

    testthat::expect_true(all(c("wfhz", "flag_wfhz") %in% vals))
    testthat::expect_equal(
      object = length(vals),
      expected = 17
    )
    ### Assert expectectations ----
    testthat::expect_true("wrangle-wrangled_data" %in% names(wfhz$output))
    testthat::expect_equal(wfhz$input$"wrangle-wrangle", expected = "wfhz")
    testthat::expect_equal(length(wfhz$input$"wrangle-wrangled_data_rows_all"), 20)

    #### Stop the app ----
    app$stop()
  }
)

## ---- Data Wrangling: MUAC ---------------------------------------------------

testthat::test_that(
  desc = "Data wrangling server works as expected for MUAC",
  code = {

    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 120000,
      wait = TRUE
    )

    ### Wait until app has fully init ----
    app$wait_for_idle(timeout = 40000)

    ### Click on Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    ### Read data in then upload ----
    data <- read.csv(
      file = system.file("app", "anthro_.csv", package = "wowi"),
      sep = ","
    )
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(data, tmpfile, row.names = FALSE)

    #### Upload onto the appp ----
    app$upload_file("upload_data-upload" = tmpfile, wait_ = TRUE)

    ### Wait until upload is processed ----
    app$wait_for_idle(timeout = 15000)

    ### Click on the "Data Wrangling" tab ----
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 40000)

    ### Set Wrangling method ----
    app$set_inputs("wrangle-wrangle" = "muac", wait_ = TRUE, timeout_ = 12000)

    ### Now set the variable selectors ----
    app$set_inputs(`wrangle-age` = "age", wait_ = FALSE)
    app$set_inputs("wrangle-sex" = "sex", wait_ = FALSE)
    app$set_inputs("wrangle-muac" = "muac", wait_ = FALSE)
    app$set_inputs("wrangle-oedema" = "oedema", wait_ = FALSE)

    ### Wait before clicking apply
    app$wait_for_idle(timeout = 40000)

    ### Click apply button
    app$click("wrangle-apply_wrangle")
    Sys.sleep(5)
    app$wait_for_value(output = "wrangle-wrangled_data", timeout = 40000)

    ### Get all values ----
    muac <- app$get_values()

    ### Get wrangled df ----
    vals <- app$get_js("
    $('#wrangle-wrangled_data thead th').map(function() {
      return $(this).text();
    }).get();
  ")[-2] |> as.character()


    ### Assert expectectations ----
    testthat::expect_true(all(c("mfaz", "flag_mfaz", "gam", "sam", "mam") %in% vals))
    testthat::expect_equal(object = length(vals), expected = 18)
    testthat::expect_equal(length(muac$input$"wrangle-wrangled_data_rows_all"), 20)

    #### Stop the app ----
    app$stop()
  }
)

## ---- Data Wrangling: Combined -----------------------------------------------

testthat::test_that(
  desc = "Data wrangling server works as expected for combined",
  code = {

    ### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      load_timeout = 10000,
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

    ### Set Wrangling method ----
    app$set_inputs("wrangle-wrangle" = "combined", wait_ = TRUE, timeout_ = 12000)

    ### Now set the variable selectors ----
    app$set_inputs("wrangle-sex" = "sex", wait_ = FALSE)
    app$set_inputs("wrangle-age" = "age", wait_ = FALSE)
    app$set_inputs("wrangle-weight" = "weight", wait_ = FALSE)
    app$set_inputs("wrangle-height" = "height", wait_ = FALSE)
    app$set_inputs("wrangle-muac" = "muac", wait_ = FALSE)
    app$set_inputs("wrangle-oedema" = "oedema", wait_ = FALSE)

    ### Wait before clicking apply
    app$wait_for_idle(timeout = 5000)

    ### Click apply button
    app$click("wrangle-apply_wrangle", wait_ = TRUE, timeout_ = 15000)
    Sys.sleep(5)

    ### Wait for wrangled df ----
    app$wait_for_value(output = "wrangle-wrangled_data", timeout = 40000)

    ### Get wrangled df ----
    vals <- app$get_js("
    $('#wrangle-wrangled_data thead th').map(function() {
      return $(this).text();
    }).get();
  ")[-2] |> as.character()
    
    combined <- app$get_values()

    ### Assert expectectations ----
    testthat::expect_true(all(c("mfaz", "wfhz", "flag_wfhz", "flag_mfaz",
      "cgam", "csam", "cmam") %in% vals))
    testthat::expect_equal(object = length(vals), expected = 20)
    testthat::expect_equal(length(combined$input$"wrangle-wrangled_data_rows_all"), 20)

    #### Stop the app ----
    app$stop()
  }
)
