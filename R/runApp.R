run_wowi <- function() {
  app_dir <- system.file("wowi", "app", package = "wowi")

  if (app_dir == "") {
    stop("Could not find Shiny directory. Try re-installing `wowi`.", call = FALSE)
  }

  shiny::runApp(appDir = app_dir, display.mode = "normal")
}
