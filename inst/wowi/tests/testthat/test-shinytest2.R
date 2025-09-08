library(shinytest2)


test_that("{shinytest2} recording: data-uploading", {
  wowi_app <- ww_run_app() |> record_test()
  app <- AppDriver$new(variant = wowi_app, name = "wowi")
  app$expect_screenshot()
})

testthat::test_that("sample app works", {
  appdir <- system.file(package = "wowi", "wowi")
  test_app(app_dir = appdir)
}

)
