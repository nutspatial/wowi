#'
#'
#' @keywords internal
#'
skip_if_no_satscan <- function(ss_path = "/Applications/SaTScan.app/Contents/app/satscan") {
  testthat::skip_if_not(file.exists(ss_path), message = "SaTScan is not installed or not found")
}
