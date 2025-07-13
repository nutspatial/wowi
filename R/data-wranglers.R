#'
#'
#' @keywords internal
#'
wrangle_data <- function(.data, .gam_based = c("wfhz", "muac", "combined")) {
  ## Enforce options in `.gam_based` ----
  .gam_based <- match.arg(.gam_based)

  ## Define unquoted expressions to pass downstream ----
  flag_expr <- rlang::parse_expr(
    dplyr::case_when(
      .gam_based == "wfhz" ~ "flag_wfhz != 1",
      .gam_based == "muac" ~ "flag_mfaz != 1",
      .gam_based == "combined" ~ "flag_wfhz != 1 | flag_mfaz != 1 "
    )
  )

  ## Define a variable name for gam by wfhz, muac or combined ----
  gam_sym <- rlang::sym(if (.gam_based != "combined") "gam" else "cgam")

  ## Case file ----
  cases <- .data |>
    dplyr::filter(!!flag_expr, !!gam_sym != 0) |>
    dplyr::select(.data$cluster, !!gam_sym) |>
    dplyr::rename(cases = !!gam_sym, locationid = .data$cluster)

  ## Control file ----
  ctrls <- .data |>
    dplyr::filter(!!flag_expr, !!gam_sym != 1) |>
    dplyr::select(.data$cluster, !!gam_sym) |>
    dplyr::rename(ctrls = !!gam_sym, locationid = .data$cluster) |>
    dplyr::mutate(ctrls = replace(ctrls, values = 1))

  ## Geographical coordinates file ----
  geo <- .data |>
    dplyr::select(locationid = .data$cluster, .data$latitude, .data$longitude) |>
    dplyr::relocate(.data$longitude, .after = .data$locationid) |>
    dplyr::slice_head(by = .data$locationid, n = 1) |>
    dplyr::arrange(.data$locationid)

  list(cases, ctrls, geo)
}

#'
#' 
#' 
#' Wrangle data into cases, controls and geographic coordinates files, as 
#' required in SaTScan GUI, and save them in the working directory
#' 
#' 
ww_wrangle_data <- function(.data, 
filename = character(), destfile = character(),
.gam_based = c("wfhz", "muac", "combined")
) {

## Enforce options in `.gam_based` ----
.gam_based <- match.arg(.gam_based)

## Create a directory if it does not exist ----
if (!dir.exists(destfile)) {
  dir.create(
    path = destfile,
    showWarnings = TRUE,
    recursive = TRUE
  )
} else {
  message(
    paste0("`", destfile, "` already exists in project repo.")
  )
}

## Wrangle data into cases, controls and geographical files ----
input_files <- wrangle_data(.data = .data, .gam_based = .gam_based)

## Write file into `destfile` in which SaTScan will access them ----

### Case file ---
do.call(
  what = rsatscan::write.cas,
  args = list(
    x = input_files[[1]],
    location = destfile,
    filename = filename
  )
)

### Control file ---
do.call(
  what = rsatscan::write.ctl,
  args = list(
    x = input_files[[2]],
    location = destfile,
    filename = filename
  )
)

### Geo file ---
do.call(
  what = rsatscan::write.geo,
  args = list(
    x = input_files[[3]],
    location = destfile,
    filename = filename
  )
)

## Return full path ----
file.path(destfile, filename)
}