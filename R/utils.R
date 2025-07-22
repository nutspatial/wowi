#'
#'
#' @keywords internal
#'
skip_if_no_satscan <- function(ss_path = "/Applications/SaTScan.app/Contents/app/satscan") {
  testthat::skip_if_not(file.exists(ss_path), message = "SaTScan is not installed or not found")
}

#'
#'
#' @keywords internal
#'
#'
parse_clusters <- function(file) {

  ## Access the txt-based results ----
  txt <- file$main

  ## Find line indices with "Location IDs included" ----
  cluster_start <- stringr::str_which(txt, "^\\d+\\.Location IDs included\\.:")
  area_name <- stringr::str_which(txt, "^[Case File]+\\:")

  ## Define line offsets to grab per cluster ----
  cluster_blocks <- lapply(cluster_start, function(start_idx) {
    txt[start_idx:(start_idx + 10)]
  })

  ## Parse each cluster block ----
  parsed_clusters <- lapply(cluster_blocks, function(block) {
    tibble::tibble(
      survey_area = stringr::str_extract_all(basename(txt[[area_name]]), "^[^.]+") |>
        as.character(),
      nr_EAs = stringr::str_extract(txt[[18]], "\\d+") |> as.integer(),
      total_children = stringr::str_extract(txt[[19]], "\\d+") |> as.integer(),
      total_cases = stringr::str_extract(txt[[20]], "\\d+") |> as.integer(),
      `%_cases` = stringr::str_extract(txt[[21]], "\\d+") |> as.double(),
      location_ids = stringr::str_extract_all(block[1], "[0-9]+")[[1]][-1] |>
        paste(collapse = ","),
      geo = stringr::str_extract(block[2], "\\d+\\.\\d+\\s+\\w\\,\\s+\\d+\\.\\d+\\s+\\w"),
      radius = stringr::str_extract(block[2], "\\d+\\.\\d+\\s*\\w+$"),
      span = stringr::str_extract(block[3], "[0-9]+[.]+[0-9]+\\s+\\w+$"),
      children = stringr::str_extract(block[4], "[0-9]+") |> as.integer(),
      n_cases = stringr::str_extract(block[5], "[0-9]+") |> as.integer(),
      expected_cases = stringr::str_extract(block[6], "[0-9]+[.]+[0-9]+") |> as.double(),
      observedExpected = stringr::str_extract(block[7], "[0-9]+[.]+[0-9]+") |> as.double(),
      relative_risk = stringr::str_extract(block[8], "[0-9]+\\.\\d+") |> as.double(),
      `%_cases_in_area` = stringr::str_extract(block[9], "[0-9]+\\.\\d+") |> as.double(),
      log_lik_ratio = stringr::str_extract(block[10], "[0-9]+\\.\\d+") |> as.double(),
      pvalue = stringr::str_extract(block[11], "[0-9]+[.]+[0-9]+") |> as.double(),
      ipc_amn = ifelse(
        test = length(strsplit(location_ids, ",\\s*")[[1]]) >= 5 & as.numeric(children) > 100,
        yes = "yes",
        no = "no"
      )
    )
  })

  ## Combine all into one data frame ----
  dplyr::bind_rows(parsed_clusters)
}
