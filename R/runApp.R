#'
#' 
#' Helper function to locate app directory
#' 
#' @keywords internal
#' 
get_app_dir <- function() {
  app_dir <- system.file("wowi", "app", package = "wowi")
  if (app_dir == "") {
    stop("Could not find Shiny directory. Try re-installing `wowi`.", call. = FALSE)
  }
  app_dir
}



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

# nocov start
ww_run_app <- function() {
  shiny::runApp(appDir = get_app_dir(), display.mode = "normal")
}
# nocov end