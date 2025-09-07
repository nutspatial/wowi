#'
#' 
#' Initialise built-in Shiny application
#' 
#' @return NULL
#' 
#' @examples
#' if(interactive()) ww_run_app()
#' 
#' @export
#' 
ww_run_app <- function() {
  app_dir <- system.file("wowi", "app", package = "wowi")

  if (app_dir == "") {
    stop(
      "Could not find Shiny directory. Try re-installing `wowi`.", call = FALSE
    )
  }

  shiny::runApp(appDir = app_dir, display.mode = "normal")
}
