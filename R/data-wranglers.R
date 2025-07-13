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
#' Prepare SaTScan-required input data files for a Bernoulli spatial scan
#' analysis and save them in a user-defined working directory
#'
#' @description
#' SaTScan's Bernoulli-based spatial scan requires the input data to be split
#' into cases, controls, and geographical coordinates files, saved in a format
#'  readable by the software, and placed in a directory it can access.
#'
#' `ww_wrangle_data()` is a convenient function designed for this task. It
#' assumes that the input anthropometric data has been pre-processed using the
#' `{mwana}` data wrangling functions.
#'
#' @param .data A data frame object that has been wrangled using
#'  `mwana::mw_wrangle_*()` functions.
#'
#' @param filename A quoted string identifying the analysis area.
#'
#' @param dir A quoted string of the folder or directory in which the files
#' should be saved.
#'
#' @param .gam_based A quoted string indicating the criterion used to define acute
#' malnutrition. This is used to identify the right vector where flagged values
#' are identified, and for which should be excluded from the analysis. Default
#' is `wfhz`.
#'
#' @returns
#' Three files are created and saved in the user-defined directory as defined
#' by `dir`: a `.cas` file for cases, a `.ctl` for controls, and
#' a `.geo` file for geographical coordinates. The full filenames will incorporate
#' the use-defined `filename` string.
#'
#' The `.cas` and `.ctl` files will each have two columns: the first containing
#' survey cluster or enumeration area IDs, and the second containing only `1`s,
#' representing either cases or controls, respectively. The length of the `.cas`
#' file depends on the number of positive acute malnutrition cases
#' (`gam == 1`), and the `.ctl` file on the number of negative cases
#' (`gam == 0`).
#'
#' The `.geo` file will have three columns: cluster or enumeration area IDs,
#' latitude, and longitude.
#'
#' @examples
#'
#' ## Given a temporary directory ----
#' tmp <- withr::local_tempdir()
#' directory <- file.path(tmp, "input-files")
#'
#' ## Wrangle data with `{mwana}` ----
#' x <- df |>
#'   mwana::mw_wrangle_wfhz(
#'     sex = sex,
#'     .recode_sex = TRUE,
#'     weight = weight,
#'     height = height
#'   ) |>
#'   mwana::define_wasting(
#'     zscores = wfhz,
#'     .by = "zscores",
#'     edema = edema
#'   )
#'
#' ## Apply the function ----
#' ww_wrangle_data(
#'   .data = x,
#'   filename = "Locality",
#'   dir = directory,
#'   .gam_based = "wfhz"
#' )
#'
#' ## Show created files ----
#' list.files(file.path(tmp, "input-files"))
#'
#' ## Display each files' content ----
#' file.show(file.path(tmp, "input-files/Locality.cas"))
#' file.show(file.path(tmp, "input-files/Locality.ctl"))
#' file.show(file.path(tmp, "input-files/Locality.geo"))
#'
#' @export
#'
ww_wrangle_data <- function(
    .data,
    filename = character(), dir = character(),
    .gam_based = c("wfhz", "muac", "combined")) {
  ## Enforce options in `.gam_based` ----
  .gam_based <- match.arg(.gam_based)

  ## Create a directory if it does not exist ----
  if (!dir.exists(dir)) {
    dir.create(
      path = dir,
      showWarnings = TRUE,
      recursive = TRUE
    )
  } else {
    message(
      paste0("`", basename(dir), "` already exists in project repo.")
    )
  }

  ## Wrangle data into cases, controls and geographical files ----
  input_files <- wrangle_data(.data = .data, .gam_based = .gam_based)

  ## Write file into `dir` in which SaTScan will access them ----

  ### Case file ---
  do.call(
    what = rsatscan::write.cas,
    args = list(
      x = input_files[[1]],
      location = dir,
      filename = filename
    )
  )

  ### Control file ---
  do.call(
    what = rsatscan::write.ctl,
    args = list(
      x = input_files[[2]],
      location = dir,
      filename = filename
    )
  )

  ### Geo file ---
  do.call(
    what = rsatscan::write.geo,
    args = list(
      x = input_files[[3]],
      location = dir,
      filename = filename
    )
  )

  ## Return full path ----
  file.path(dir, filename)
}
