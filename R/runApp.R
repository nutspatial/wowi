#'
#' 
#' Helper function to locate app directory
#' 
#' @keywords internal
#' 
get_app_dir <- function(package = "wowi") {
  app_dir <- system.file("app", package = package)
  if (app_dir == "") {
    stop("Could not find Shiny directory. Try re-installing `wowi`.", call. = FALSE)
  }
  app_dir
}



#'
#' 
#' Initialise built-in Shiny application
#' 
#' @param package package name ("wowi").
#' 
#' @return NULL
#' 
#' @examples
#' if(interactive()) ww_run_app()
#' 
#' @export

# nocov start
ww_run_app <- function(package = "wowi") {
  shiny::runApp(appDir = get_app_dir(package), display.mode = "normal")
}
# nocov end