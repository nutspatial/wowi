#'
#'
#'
#'
#'
#' @keywords internal
#'
wrangle_data <- function(.data, .for = c("wfhz", "muac", "combined")) {
  ## Enforce options in `.for` ----
  .for <- match.arg(.for)

  ## Define unquoted expressions to pass downstream ----
  flag_expr <- rlang::parse_expr(
    dplyr::case_when(
      .for == "wfhz" ~ "flag_wfhz != 1",
      .for == "muac" ~ "flag_mfaz != 1",
      .for == "combined" ~ "flag_wfhz != 1 | flag_mfaz != 1 "
    )
  )

  ## Define a variable name for gam by wfhz, muac or combined ----
  gam_sym <- rlang::sym(if (.for != "combined") "gam" else "cgam")

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
