## ---- Module: UI -------------------------------------------------------------

#'
#'
#' Module UI for data upload
#'
#'
#' @param id Module ID
#'
#' @keywords internal
#'
#'
module_ui_run_spatial_scan <- function(id) {
  ## Namespace module ID ----
  ns <- shiny::NS(id)

  ## Add UI elements ----
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 400,
      bslib::card(
        style = "background-color: #fbfdfd;",
        bslib::card_header(htmltools::tags$span(
          "Set wowi Hyperparameters for Analysis",
          style = "font-size: 15px; font-weight: bold;"
        )),
        shiny::radioButtons(
          inputId = ns("analysis_scope"),
          label = htmltools::tags$span(
            "What is the analysis scope that wowi should consider?",
            style = "font-size: 14px; font-weight: bold;"
          ),
          choices = c(
            "Single-area analysis" = "single-area",
            "Multiple-area analysis" = "multiple-area"
          ),
          selected = "single-area"
        ),

        #### Container in UI to store outputs derived from the server ----
        shiny::uiOutput(outputId = ns("hyperparameters")),

        #### Action button ----
        shiny::actionButton(
          inputId = ns("run_scan"),
          label = "Run Scan",
          class = "btn-primary"
        )
      )
    ),
    bslib::layout_column_wrap(
      width = "100%",
      bslib::layout_columns(
        col_widths = c(3, 9), # First column narrower
        row_heights = NULL, # Let height flow naturally

        ### First column: full-height card
        htmltools::tags$div(
          style = "height: 100vh;", # Full viewport height
          bslib::card(
            style = "background-color: #fbfdfd;",
            bslib::card_header(htmltools::tags$span(
              "Created Files",
              style = "font-size: 15px; font-weight: bold;"
            )),
            shiny::verbatimTextOutput(outputId = ns("files_created"))
          )
        ),

        ### Second column: table card ----
        bslib::card(
          style = "background-color: #fbfdfd;",
          bslib::card_header(htmltools::tags$span(
            "Results of Detected Clusters",
            style = "font-size: 15px; font-weight: bold;"
          )),
          shinycssloaders::withSpinner(
            ui_element = DT::DTOutput(outputId = ns("clusters")),
            type = 8,
            color.background = "#9dac7c",
            image = "logo.png",
            image.height = "70px",
            color = "#9dac7c",
            caption = htmltools::tags$div(
              htmltools::tags$h6("Scanning"), htmltools::tags$h6("Please wait...")
            )
          ),
          shiny::uiOutput(outputId = ns("download"))
        )
      )
    )
  )
}
