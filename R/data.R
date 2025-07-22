#'
#' Sample data set of district-level SMART surveys with geographical coordinates
#'
#' @description
#' `anthro` is a SMART survey-generated data conducted in nine districts in Uganda.
#'
#' @format A tibble of 2,934 rows and 17 columns. 
#' 
#' |**Variable** | **Description** |
#' | :--- | :---|
#' | *district* | Location in which the survey was undertaken |
#' | *cluster* | Primary sampling unit |
#' | *sex* | Sex; "1" = boys, "2" = girls |
#' | *age* | Calculated age in months with two decimal places |
#' | *weight* | Weight in kilograms |
#' | *height* | Height in centimetres |
#' | *oedema* | Oedema; "n" = no oedema, "y" = with oedema |
#' | *muac* | Mid upper-arm circumference in millimetres |
#' | *y* | Geographical coordinates: Latitude |
#' | *x* | Geographical coordinates: Longitude |
#' | *precision* | Estimated spatial accuracy of the recorded GPS coordinates, in meters. |
#'
#' @source anonymous
#'
#' @examples
#' anthro
#'
"anthro"
