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
          "single-area" = mod_display_input_variables(.data(), "single", ns),

          ### Multiple-area analysis ----
          "multiple-area" = mod_display_input_variables(.data(), "multiple", ns)
        )
      })

      # Initialize the scanning reactive value (add this near your other reactive values)
      vals$scanning <- shiny::reactiveVal(FALSE)

      ### Logic for calculations ----
      shiny::observeEvent(
        eventExpr = input$run_scan,
        {
          #### Clear previous results and start scanning ----
          vals$scanned <- NULL
          vals$scanning(TRUE)

          #### Ensure data exists before rendering ----
          shiny::req(.data())


          ## Catch wrangling method used to inform method of GAM definition ----
          x <- .data()
          vals$gam_based <- if ("flag_mfaz" %in% names(x) && !"flag_wfhz" %in% names(x)) {
            gam_based <- "muac"
          } else if ("flag_wfhz" %in% names(x) && !"flag_mfaz" %in% names(x)) {
            gam_based <- "wfhz"
          } else {
            gam_based <- "combined"
          }

          ### Handle error gracefully ----
          #### Containers for graceful error handling ----
          msg <- NULL
          valid <- TRUE

          #### Condition for when analysis scope is single area ----
          if (input$analysis_scope == "single-area") {
            if (any(!nzchar(c(
              input$filename, input$directory, input$sslocation,
              input$ssbatchfilename, input$satscan_version, input$scan_for,
              input$latitude, input$longitude
            )))) {
              valid <- FALSE
              msg <- "Please supply all required inputs indicated by *."
            }

            #### Raise error when condition is not met ----
            if (!valid) {
              shiny::showNotification(msg, type = "error")
              vals$scanning(FALSE)
              return()
            }
          }

          #### Condition for when analysis scope is multiple area ----
          if (input$analysis_scope == "multiple-area") {
            if (any(!nzchar(c(
              input$directory, input$sslocation,
              input$ssbatchfilename, input$satscan_version, input$scan_for,
              input$latitude, input$longitude
            )))) {
              valid <- FALSE
              msg <- "Please supply all required inputs indicated by *."
            }

            #### Raise error when condition is not met ----
            if (!valid) {
              shiny::showNotification(msg, type = "error")
              vals$scanning(FALSE)
              return()
            }
          }

          #### Logic for single-area spatial scan ----
          if (input$analysis_scope == "single-area") {
            #### Ensure that all parameters for single-area analysis are given ----
            shiny::req(
              input$filename, input$directory, input$sslocation,
              input$ssbatchfilename, input$satscan_version, input$scan_for,
              input$latitude, input$longitude
            )

            ### Try execute operation and raise error in case not ----
            tryCatch(
              {
                r <- mod_call_satscan(
                  .data = .data(),
                  filename = input$filename,
                  directory = input$directory,
                  sslocation = input$sslocation,
                  ssbatchfilename = input$ssbatchfilename,
                  satscan_version = input$satscan_version,
                  scan_for = input$scan_for,
                  latitude = input$latitude,
                  longitude = input$longitude,
                  gam_based = vals$gam_based,
                  area = input$area,
                  vals = vals,
                  output = output
                )

                vals$scanned <- r
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
              input$satscan_version, input$scan_for, input$latitude, input$longitude
            )

            ### Try execute operation and raise error in case not ----
            tryCatch(
              {
                r <- mod_call_satscan(
                  .data = .data(),
                  filename = NULL,
                  directory = input$directory,
                  sslocation = input$sslocation,
                  ssbatchfilename = input$ssbatchfilename,
                  satscan_version = input$satscan_version,
                  scan_for = input$scan_for,
                  latitude = input$latitude,
                  longitude = input$longitude,
                  gam_based = vals$gam_based,
                  area = input$area,
                  vals = vals,
                  output = output
                )

                vals$scanned <- r
              },
              error = function(e) {
                shiny::showNotification(
                  base::paste("Error during scanning:", e$message, type = "error")
                )
              }
            )
          }

          #### Display the list of files in the given directory ----
          output$files_created <- shiny::renderText({
            files <- base::list.files(path = input$directory, all.files = TRUE, full.names = FALSE)
            base::paste(files, collapse = "\n")
          })

          # End scanning
          vals$scanning(FALSE)
        }
      )

      #### Display a summary table of detected cluster and prettified ----
      output$clusters <- DT::renderDT({
        # Show scanning message while scanning is in progress
        shiny::req(!vals$scanning()) # Let waiting spinner run until wrangling is done
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
      })

      #### Download button to download table of detected clusters in .xlsx ----
      ##### Output into the UI ----
      output$download <- shiny::renderUI({
        shiny::req(vals$scanned)
        shiny::req(!vals$scanning())
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
