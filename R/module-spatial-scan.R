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
          label = shiny::tagList(
            htmltools::tags$span("Analysis Scope",
              style = "font-size: 14px; font-weight: bold;"
            ),
            htmltools::tags$span("*", style = "color: red;")
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
              htmltools::tags$h6(htmltools::tags$span("Scanning", style = "font-size: 12px;")),
              htmltools::tags$h6(htmltools::tags$span("Please wait...", style = "font-size: 12px;"))
            )
          ),
          shiny::uiOutput(outputId = ns("download"))
        )
      )
    )
  )
}


## ---- Module: Server ---------------------------------------------------------

#'
#'
#'
#' Module server for data upload
#'
#'
#' @param id Module ID
#'
#' @keywords internal
#'
#'
module_server_run_spatial_scan <- function(id, .data) {
  ### Shiny module server ----
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      ## Container for reactive vals of scan results ----
      vals <- shiny::reactiveValues(scanned = NULL, gam_based = NULL)

      output$hyperparameters <- shiny::renderUI({
        ### Ensure data exists before rendering ----
        shiny::req(.data())

        ### Collect user-defined parameters based on scope of analysis ----
        switch(input$analysis_scope,
          "single-area" = shiny::tagList(
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
            ),
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
              choices = c("", names(.data()))
            ),
            shiny::selectInput(
              inputId = ns("longitude"),
              label = shiny::tagList(
                htmltools::tags$span("Longitude",
                  style = "font-size: 14px; font-weight: bold;"
                ),
                htmltools::tags$span("*", style = "color: red;")
              ),
              choices = c("", names(.data()))
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
          ),

          ### Multiple-area analysis ----
          "multiple-area" = shiny::tagList(
            shiny::selectInput(
              inputId = ns("area"),
              label = shiny::tagList(
                htmltools::tags$strong("Area of Analysis"),
                htmltools::tags$div(
                  style = "font-size: 0.85em; color: #6c757d;",
                  "Name of a district, county, etc, as in your dataset"
                )
              ),
              choices = c("", names(.data()))
            ),
            shiny::textInput(
              inputId = ns("directory"),
              label = htmltools::tags$strong(
                "Directory where the parameters files should be saved in"
              ),
              value = ""
            ),
            shiny::selectInput(
              inputId = ns("latitude"),
              label = htmltools::tags$strong("Latitude"),
              choices = c("", names(.data()))
            ),
            shiny::selectInput(
              inputId = ns("longitude"),
              label = htmltools::tags$strong("Longitude"),
              choices = c("", names(.data()))
            ),
            shiny::textInput(
              inputId = ns("sslocation"),
              label = shiny::tagList(
                htmltools::tags$strong("Path to where SaTScan GUI is installed on your computer"),
                htmltools::tags$div(
                  style = "font-size: 0.85em; color: #6c757d;",
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
                htmltools::tags$strong("SaTScan batch file name"),
                htmltools::tags$div(
                  style = "font-size: 0.85em; color: #6c757d;",
                  'e.g., macOS: "satscan"; Windows: "SaTScanBatch64"'
                )
              )
            ),
            shiny::textInput(
              inputId = ns("satscan_version"),
              label = shiny::tagList(
                htmltools::tags$strong("Version of SaTScan"),
                htmltools::tags$div(
                  style = "font-size: 0.85em; color: #6c757d;",
                  'e.g., "10.3.2"'
                )
              )
            ),
            shiny::radioButtons(
              inputId = ns("scan_for"),
              label = htmltools::tags$strong("Which type of cluster should be scanned for?"),
              choices = list(
                "Clusters of High Rates" = "high-rates",
                "Cluster of High and Low Rates" = "high-low-rates"
              ),
              selected = "high-low-rates"
            )
          )
        )
      })

      # Initialize the scanning reactive value (add this near your other reactive values)
      scanning <- shiny::reactiveVal(FALSE)

      ### Logic for calculations ----
      shiny::observeEvent(
        eventExpr = input$run_scan,
        {
          ## Catch wrangling method used to inform method of GAM definition ----
          x <- .data()
          vals$gam_based <- if ("flag_mfaz" %in% names(x) && !"flag_wfhz" %in% names(x)) {
            gam_based <- "muac"
          } else if ("flag_wfhz" %in% names(x) && !"flag_mfaz" %in% names(x)) {
            gam_based <- "wfhz"
          } else {
            gam_based <- "combined"
          }

          #### Clear previous results and start scanning ----
          vals$scanned <- NULL
          scanning(TRUE)

          #### Ensure data exists before rendering ----
          shiny::req(.data())

          #### Logic for single-area spatial scan ----
          if (input$analysis_scope == "single-area") {
            #### Ensure that all parameters for single-area analysis are given ----
            shiny::req(
              input$filename, input$directory, input$sslocation,
              input$ssbatchfilename, input$satscan_version, input$scan_for
            )

            #### Make user-defined parameters wowi-usable inputs ----
            area <- as.character(input$filename)
            dir <- as.character(input$directory)
            satscan_location <- as.character(input$sslocation)
            batchfilename <- as.character(input$ssbatchfilename)
            version <- as.character(input$satscan_version)
            scan_for <- as.character(input$scan_for)

            tryCatch(
              {
                result <- .data() |>
                  dplyr::rename(
                    longitude = !!rlang::sym(input$longitude),
                    latitude = !!rlang::sym(input$latitude)
                  ) |>
                  ww_run_satscan(
                    filename = area,
                    dir = dir,
                    sslocation = satscan_location,
                    ssbatchfilename = batchfilename,
                    satscan_version = version,
                    .by_area = FALSE,
                    .scan_for = scan_for,
                    .gam_based = vals$gam_based,
                    latitude = .data$latitude,
                    longitude = .data$longitude,
                    area = NULL
                  )

                vals$scanned <- result

                #### Display the list of files in the given directory ----
                output$files_created <- shiny::renderText({
                  files <- base::list.files(path = dir, all.files = TRUE, full.names = FALSE)
                  base::paste(files, collapse = "\n")
                })
              },
              error = function(e) {
                shiny::showNotification(
                  base::paste("Error during scanning:", e$message, type = "error")
                )
              }
            )
          } else {
            #### Ensure that all parameters for single-area analysis are given ----
            shiny::req(
              input$directory, input$sslocation, input$ssbatchfilename,
              input$satscan_version, input$scan_for
            )

            #### Make user-defined parameters wowi-usable inputs ----
            dir <- as.character(input$directory)
            satscan_location <- as.character(input$sslocation)
            batchfilename <- as.character(input$ssbatchfilename)
            version <- as.character(input$satscan_version)
            scan_for <- as.character(input$scan_for)

            #### Run scan ----
            tryCatch(
              {
                result <- .data() |>
                  dplyr::rename(
                    latitude = !!rlang::sym(input$latitude),
                    longitude = !!rlang::sym(input$longitude),
                    area = !!rlang::sym(input$area)
                  ) |>
                  ww_run_satscan(
                    filename = NULL,
                    dir = dir,
                    sslocation = satscan_location,
                    ssbatchfilename = batchfilename,
                    satscan_version = version,
                    .by_area = TRUE,
                    latitude = .data$latitude,
                    longitude = .data$longitude,
                    .scan_for = scan_for,
                    .gam_based = vals$gam_based,
                    area = area
                  )

                vals$scanned <- result

                output$files_created <- shiny::renderText({
                  files <- list.files(path = dir, all.files = TRUE, full.names = FALSE)
                  base::paste(files, collapse = "\n")
                })
              },
              error = function(e) {
                shiny::showNotification(base::paste("Error during scanning:", e$message), type = "error")
              }
            )
          }

          # End scanning
          scanning(FALSE)
        }
      )

      #### Display a summary table of detected cluster and prettified ----
      output$clusters <- DT::renderDT({
        # Show scanning message while scanning is in progress

        if (scanning()) {
          # Return a placeholder table while scanning
          DT::datatable(
            data = data.frame(Status = "Scanning in progress..."),
            rownames = FALSE,
            options = list(
              dom = "t",
              ordering = FALSE,
              searching = FALSE,
              info = FALSE,
              paging = FALSE,
              columnDefs = list(
                list(className = "dt-center", targets = "_all")
              )
            ),
            selection = "none"
          ) |> DT::formatStyle(
            columns = "Status",
            fontSize = "16px",
            fontWeight = "bold",
            color = "#398DF3"
          )
        } else {
          # Only render when not scanning and results exist
          shiny::req(vals$scanned)

          ##### Display the first 8 rows only ----
          DT::datatable(
            data = utils::head(vals$scanned$.df, 5),
            rownames = FALSE,
            options = list(
              scrollX = FALSE,
              scrolly = "800px",
              columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            caption = if (base::nrow(vals$scanned$.df) > 5) {
              base::paste(
                "Showing first 5 rows of", base::format(base::nrow(vals$scanned$.df), big.mark = ","),
                "total rows"
              )
            } else {
              base::paste("showing all", base::nrow(vals$scanned$.df), "rows")
            }
          ) |> DT::formatStyle(columns = base::colnames(vals$scanned$.df), fontSize = "13px")
        }
      })

      #### Download button to download table of detected clusters in .xlsx ----
      ##### Output into the UI ----
      output$download <- shiny::renderUI({
        shiny::req(vals$scanned)
        shiny::req(!scanning())
        htmltools::tags$div(
          style = "margin-bottom: 15px; text-align: right;",
          shiny::downloadButton(
            outputId = ns("downloadResults"),
            label = "Download Clusters",
            class = "btn-primary",
            icon = shiny::icon(name = "download", class = "fa-lg")
          )
        )
      })

      ##### Downloadable results by clicking on the download button ----
      output$downloadResults <- shiny::downloadHandler(
        filename = function() {
          shiny::req(vals$gam_based)
          w <- vals$gam_based
          ### Filename according to wrangling method and analysis scope ----
          if (w == "wfhz") {
            base::paste0("wowi-detected-clusters-gambywfhz_", Sys.Date(), ".xlsx", sep = "")
          } else if (w == "muac") {
            base::paste0("wowi-detected-clusters-gambymuac_", Sys.Date(), ".xlsx", sep = "")
          } else {
            base::paste0("wowi-detected-clusters-cgam_", Sys.Date(), ".xlsx", sep = "")
          }
        },
        content = function(file) {
          shiny::req(vals$scanned) # Ensure results exist
          tryCatch(
            {
              openxlsx::write.xlsx(vals$scanned$.df, file)
              shiny::showNotification("File downloaded successfully!", type = "message")
            },
            error = function(e) {
              shiny::showNotification(base::paste("Error creating file:", e$message), type = "error")
            }
          )
        }
      )
    }
  )
}
