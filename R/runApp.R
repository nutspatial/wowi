#'
#' 
#' Helper function to locate app directory
#' 
#' @keywords internal
#' 
get_app_dir <- function(pkg = "wowi") {
  app_dir <- system.file(pkg, "app", package = "wowi")
  if (app_dir == "") {
    stop("Could not find Shiny directory. Try re-installing `wowi`.", call. = FALSE)
  }
  app_dir
}



#'
#' 
#' Initialise built-in Shiny application
#' 
#' @param pkg package name ("wowi").
#' 
#' @return NULL
#' 
#' @examples
#' if(interactive()) ww_run_app()
#' 
#' @export

# nocov start
ww_run_app <- function(pkg = "wowi") {
  shiny::runApp(appDir = get_app_dir(pkg), display.mode = "normal")
}
# nocov end