#'
#'
#' Display input variables dynamically, according to UI for screening
#'
#' @keywords internal
#'
#'
mod_display_input_variables <- function(.data, analysis_scope = c("single", "multiple"), ns) {
  shiny::tagList(
    if (analysis_scope == "single") {
      shiny::textInput(
        inputId = ns("filename"),
        label = shiny::tagList(
          htmltools::tags$span("Area of Analysis",
            style = "font-size: 14px; font-weight: bold"
          ),
          htmltools::tags$span("*", style = "color: red;"),
          htmltools::tags$div(
            style = "font-size: 10px; color: #6c757d;",
            "Name of a district, county, etc, as in your dataset"
          )
        )
      )
    } else {
      shiny::tagList(
        shiny::selectInput(
          inputId = ns("area"),
          label = shiny::tagList(
            htmltools::tags$span("Area of Analysis",
              style = "font-size: 14px; font-weight: bold"
            ),
            htmltools::tags$span("*", style = "color: red;"),
            htmltools::tags$div(
              style = "font-size: 10px; color: #6c757d;",
              "Name of a district, county, etc, as in your dataset"
            )
          ),
          choices = c("", names(.data))
        )
      )
    },
    shiny::textInput(
      inputId = ns("directory"),
      label = shiny::tagList(
        htmltools::tags$span(
          "Directory wherein files should be saved",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$span("*", style = "color: red;")
      ),
      value = ""
    ),
    shiny::selectInput(
      inputId = ns("latitude"),
      label = shiny::tagList(
        htmltools::tags$span("Latitude",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$span("*", style = "color: red;"),
      ),
      choices = c("", names(.data))
    ),
    shiny::selectInput(
      inputId = ns("longitude"),
      label = shiny::tagList(
        htmltools::tags$span("Longitude",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$span("*", style = "color: red;")
      ),
      choices = c("", names(.data))
    ),
    shiny::textInput(
      inputId = ns("sslocation"),
      label = shiny::tagList(
        htmltools::tags$span("Path to where SaTScan GUI is installed on your computer",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$span("*", style = "color: red;"),
        htmltools::tags$div(
          style = "font-size: 10px; color: #6c757d;",
          'e.g., macOS: "/Applications/SaTScan.app/Contents/app";
            Windows: "C:/Program Files/SaTScan"'
        )
      ),
      value = "",
      width = NULL
    ),
    shiny::textInput(
      inputId = ns("ssbatchfilename"),
      label = shiny::tagList(
        htmltools::tags$span("SaTScan batch file name",
          style = "font-size: 14px; font-weight: bold"
        ),
        htmltools::tags$span("*", style = "color: red;"),
        htmltools::tags$div(
          style = "font-size: 10px; color: #6c757d;",
          'e.g., macOS: "satscan"; Windows: "SaTScanBatch64"'
        )
      )
    ),
    shiny::textInput(
      inputId = ns("satscan_version"),
      label = shiny::tagList(
        htmltools::tags$span("Version of SaTScan",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$span("*", style = "color: red;"),
        htmltools::tags$div(
          style = "font-size: 10px; color: #6c757d;",
          'e.g., "10.3.2"'
        )
      )
    ),
    shiny::radioButtons(
      inputId = ns("scan_for"),
      label = htmltools::tags$span(
        "Type of clusters to be scanned for",
        style = "font-size: 14px; font-weight: bold;"
      ),
      choices = list(
        "Clusters of High Rates" = "high-rates",
        "Cluster of High and Low Rates" = "high-low-rates"
      ),
      selected = "high-low-rates"
    )
  )
}


#'
#'
#' Run Scan
#'
#' @keywords internal
#'

mod_call_satscan <- function(
    .data,
    filename,
    directory,
    sslocation,
    ssbatchfilename,
    satscan_version,
    scan_for,
    latitude,
    longitude,
    gam_based,
    area,
    vals,
    output) {
  #### Make user-defined parameters wowi-usable inputs ----
  filename <- if (!is.null(filename)) as.character(filename) else NULL
  dir <- as.character(directory)
  satscan_location <- as.character(sslocation)
  batchfilename <- as.character(ssbatchfilename)
  version <- as.character(satscan_version)
  scan_for <- as.character(scan_for)
  gam_based <- as.character(gam_based)

  df <- .data
  df <- dplyr::rename(df,
    longitude = !!rlang::sym(longitude),
    latitude  = !!rlang::sym(latitude)
  )
  if (!is.null(area)) {
    df <- dplyr::rename(df, area = !!rlang::sym(area))
  }

  result <- df |>
    ww_run_satscan(
      filename = filename,
      dir = dir,
      sslocation = satscan_location,
      ssbatchfilename = batchfilename,
      satscan_version = version,
      .by_area = is.null(filename),
      .scan_for = scan_for,
      .gam_based = gam_based,
      latitude = .data$latitude,
      longitude = .data$longitude,
      area = if (!is.null(area)) df$area else NULL
    )

  result
}
