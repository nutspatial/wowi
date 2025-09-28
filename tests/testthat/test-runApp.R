# ==============================================================================
# 📦 Functions: get_app_dir()
# ==============================================================================

# ------------------------------------------------------------------------------
# 🔒 Internal:
# ------------------------------------------------------------------------------
test_that("get_app_dir returns a valid path", {
  path <- get_app_dir()
  expect_true(dir.exists(path))
})

testthat::test_that("app throws an error when directory is not found", {
  testthat::expect_error(
    object = get_app_dir("x"),
    regexp = "Could not find Shiny directory. Try re-installing `wowi`."
  )
})
