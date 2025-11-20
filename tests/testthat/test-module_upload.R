# ==============================================================================
#  Test Suite: Module Data Upload
# ==============================================================================

## ---- Module: Data Upload ----------------------------------------------------

testthat::test_that(
  desc = "Data upload server works as expected",
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

    ### Get values ----
    vals <- app$get_values(
      input = "upload_data-upload",
      output = c("upload_data-fileUploaded", "upload_data-showProgress")
    )

    ### Assert expectactions ----
    testthat::expect_true(vals$output$"upload_data-fileUploaded")
    testthat::expect_false(vals$output$"upload_data-showProgress")
    testthat::expect_gt(vals$input$upload$size, 219000)
    testthat::expect_equal(vals$input$upload$type, "text/csv")
    expect_equal(
    object = app$get_js("
    $('#upload_data-uploadedDataTable thead th').map(function() {
      return $(this).text();
    }).get();
  ")[-1][-1][1:11] |> as.character(),
    expected = c(
      "district", "cluster", "sex", "age","weight", "height", "oedema", "muac",
      "latitude", "longitude", "precision"
    )
  )
  }
)
