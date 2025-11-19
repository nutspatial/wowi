# ==============================================================================
#                               SERVER LOGIC
# ==============================================================================

## ---- Server's definitions ---------------------------------------------------

server <- function(input, output, session) {

  ### Data upload; returns a reactive data ----
  data <- wowi:::module_server_upload(id = "upload_data")

  ### Data Wrangling ----
  wrangled <- wowi:::module_server_wrangle_data(id = "wrangle", data = data)

#   ## ---- Logic for Tab 3: Run Spatial Scan ------------------------------------

#   output$hyperparameters <- renderUI({
#     ### Ensure data exists before rendering ----
#     req(values$wrangled)

#     ### Collect user-defined parameters based on scope of analysis ----
#     switch(input$analysis_scope,
#       "single-area" = tagList(
#         textInput(
#           inputId = "filename",
#           label = tagList(
#             strong("Area of Analysis"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               "Name of a district, county, etc, as in your dataset"
#             )
#           )
#         ),
#         textInput(
#           inputId = "directory",
#           label = strong(
#             "Directory where files should be saved in"
#           ),
#           value = ""
#         ),
#         selectInput(
#           inputId = "latitude",
#           label = strong("Latitude"),
#           choices = c("", names(values$wrangled))
#         ),
#         selectInput(
#           inputId = "longitude",
#           label = strong("Longitude"),
#           choices = c("", names(values$wrangled))
#         ),
#         textInput(
#           inputId = "sslocation",
#           label = tagList(
#             strong("Path to where SaTScan GUI is installed on your computer"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               'e.g., macOS: "/Applications/SaTScan.app/Contents/app";
#             Windows: "C:/Program Files/SaTScan"'
#             )
#           ),
#           value = "",
#           width = NULL
#         ),
#         textInput(
#           inputId = "ssbatchfilename",
#           label = tagList(
#             strong("SaTScan batch file name"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               'e.g., macOS: "satscan"; Windows: "SaTScanBatch64"'
#             )
#           )
#         ),
#         textInput(
#           inputId = "satscan_version",
#           label = tagList(
#             strong("Version of SaTScan"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               'e.g., "10.3.2"'
#             )
#           )
#         ),
#         radioButtons(
#           inputId = "scan_for",
#           label = strong("Which type of cluster should be scanned for?"),
#           choices = list(
#             "Clusters of High Rates" = "high-rates",
#             "Cluster of High and Low Rates" = "high-low-rates"
#           ),
#           selected = "high-low-rates"
#         )
#       ),

#       ### Multiple-area analysis ----
#       "multiple-area" = tagList(
#         selectInput(
#           inputId = "area",
#           label = tagList(
#             strong("Area of Analysis"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               "Name of a district, county, etc, as in your dataset"
#             )
#           ),
#           choices = c("", names(values$wrangled))
#         ),
#         textInput(
#           inputId = "directory",
#           label = strong(
#             "Directory where the parameters files should be saved in"
#           ),
#           value = ""
#         ),
#         selectInput(
#           inputId = "latitude",
#           label = strong("Latitude"),
#           choices = c("", names(values$wrangled))
#         ),
#         selectInput(
#           inputId = "longitude",
#           label = strong("Longitude"),
#           choices = c("", names(values$wrangled))
#         ),
#         textInput(
#           inputId = "sslocation",
#           label = tagList(
#             strong("Path to where SaTScan GUI is installed on your computer"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               'e.g., macOS: "/Applications/SaTScan.app/Contents/app";
#             Windows: "C:/Program Files/SaTScan"'
#             )
#           ),
#           value = "",
#           width = NULL
#         ),
#         textInput(
#           inputId = "ssbatchfilename",
#           label = tagList(
#             strong("SaTScan batch file name"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               'e.g., macOS: "satscan"; Windows: "SaTScanBatch64"'
#             )
#           )
#         ),
#         textInput(
#           inputId = "satscan_version",
#           label = tagList(
#             strong("Version of SaTScan"),
#             tags$div(
#               style = "font-size: 0.85em; color: #6c757d;",
#               'e.g., "10.3.2"'
#             )
#           )
#         ),
#         radioButtons(
#           inputId = "scan_for",
#           label = strong("Which type of cluster should be scanned for?"),
#           choices = list(
#             "Clusters of High Rates" = "high-rates",
#             "Cluster of High and Low Rates" = "high-low-rates"
#           ),
#           selected = "high-low-rates"
#         )
#       )
#     )
#   })

#   # Initialize the scanning reactive value (add this near your other reactive values)
#   values$scanning <- reactiveVal(FALSE)

#   ### Logic for calculations ----
#   observeEvent(
#     eventExpr = input$run_scan,
#     {
#       #### Clear previous results and start scanning ----
#       values$scan_result <- NULL
#       values$scanning(TRUE)

#       #### Ensure data exists before rendering ----
#       req(values$wrangled)

#       #### Logic for single-area spatial scan ----
#       if (input$analysis_scope == "single-area") {
#         #### Ensure that all parameters for single-area analysis are given ----
#         req(
#           input$filename, input$directory, input$sslocation,
#           input$ssbatchfilename, input$satscan_version, input$scan_for
#         )

#         #### Make user-defined parameters wowi-usable inputs ----
#         area <- as.character(input$filename)
#         dir <- as.character(input$directory)
#         satscan_location <- as.character(input$sslocation)
#         batchfilename <- as.character(input$ssbatchfilename)
#         version <- as.character(input$satscan_version)
#         scan_for <- as.character(input$scan_for)
#         gam_based <- as.character(input$wrangle)

#         tryCatch(
#           {
#             result <- values$wrangled |>
#               rename(
#                 longitude = !!sym(input$longitude),
#                 latitude = !!sym(input$latitude)
#               ) |>
#               ww_run_satscan(
#                 filename = area,
#                 dir = dir,
#                 sslocation = satscan_location,
#                 ssbatchfilename = batchfilename,
#                 satscan_version = version,
#                 .by_area = FALSE,
#                 .scan_for = scan_for,
#                 .gam_based = gam_based,
#                 latitude = latitude,
#                 longitude = longitude,
#                 area = NULL
#               )

#             values$scan_result <- result

#             #### Display the list of files in the given directory ----
#             output$files_created <- renderText({
#               files <- list.files(path = dir, all.files = TRUE, full.names = FALSE)
#               paste(files, collapse = "\n")
#             })
#           },
#           error = function(e) {
#             showNotification(
#               paste("Error during scanning:", e$message, type = "error")
#             )
#           }
#         )
#       } else {
#         #### Ensure that all parameters for single-area analysis are given ----
#         req(
#           input$directory, input$sslocation, input$ssbatchfilename,
#           input$satscan_version, input$scan_for
#         )

#         #### Make user-defined parameters wowi-usable inputs ----
#         dir <- as.character(input$directory)
#         satscan_location <- as.character(input$sslocation)
#         batchfilename <- as.character(input$ssbatchfilename)
#         version <- as.character(input$satscan_version)
#         scan_for <- as.character(input$scan_for)
#         gam_based <- as.character(input$wrangle)

#         #### Run scan ----
#         tryCatch(
#           {
#             result <- values$wrangled |>
#               rename(
#                 latitude = !!sym(input$latitude),
#                 longitude = !!sym(input$longitude),
#                 area = !!sym(input$area)
#               ) |>
#               ww_run_satscan(
#                 filename = NULL,
#                 dir = dir,
#                 sslocation = satscan_location,
#                 ssbatchfilename = batchfilename,
#                 satscan_version = version,
#                 .by_area = TRUE,
#                 latitude = latitude,
#                 longitude = longitude,
#                 .scan_for = scan_for,
#                 .gam_based = gam_based,
#                 area = area
#               )

#             values$scan_result <- result

#             output$files_created <- renderText({
#               files <- list.files(path = dir, all.files = TRUE, full.names = FALSE)
#               paste(files, collapse = "\n")
#             })
#           },
#           error = function(e) {
#             showNotification(paste("Error during scanning:", e$message), type = "error")
#           }
#         )
#       }

#       # End scanning
#       values$scanning(FALSE)
#     }
#   )

#   #### Display a summary table of detected cluster and prettified ----
#   output$clusters <- renderDT({
#     # Show scanning message while scanning is in progress

#     if (values$scanning()) {
#       # Return a placeholder table while scanning
#       datatable(
#         data = data.frame(Status = "Scanning in progress..."),
#         rownames = FALSE,
#         options = list(
#           dom = "t",
#           ordering = FALSE,
#           searching = FALSE,
#           info = FALSE,
#           paging = FALSE,
#           columnDefs = list(
#             list(className = "dt-center", targets = "_all")
#           )
#         ),
#         selection = "none"
#       ) |> formatStyle(
#         columns = "Status",
#         fontSize = "16px",
#         fontWeight = "bold",
#         color = "#398DF3"
#       )
#     } else {
#       # Only render when not scanning and results exist
#       req(values$scan_result)

#       ##### Display the first 8 rows only ----
#       datatable(
#         data = head(values$scan_result$.df, 5),
#         rownames = FALSE,
#         options = list(
#           scrollX = FALSE,
#           scrolly = "800px",
#           columnDefs = list(list(className = "dt-center", targets = "_all"))
#         ),
#         caption = if (nrow(values$scan_result$.df) > 5) {
#           paste(
#             "Showing first 5 rows of", format(nrow(values$scan_result$.df), big.mark = ","),
#             "total rows"
#           )
#         } else {
#           paste("showing all", nrow(values$scan_result$.df), "rows")
#         },
#         style = "default",
#         filter = "top"
#       ) |> formatStyle(columns = colnames(values$scan_result$.df), fontSize = "12px")
#     }
#   })

#   #### Download button to download table of detected clusters in .xlsx ----
#   ##### Output into the UI ----
#   output$download <- renderUI({
#     req(values$scan_result)
#     req(!values$scanning())
#     div(
#       style = "margin-bottom: 15px; text-align: right;",
#       downloadButton(
#         outputId = "downloadResults",
#         label = "Download Clusters",
#         class = "btn-primary",
#         icon = icon(name = "download", class = "fa-lg")
#       )
#     )
#   })

#   ##### Downloadable results by clicking on the download button ----
#   output$downloadResults <- downloadHandler(
#     filename = function() {
#       paste0("detected-clusters_", Sys.Date(), ".xlsx", sep = "")
#     },
#     content = function(file) {
#       req(values$scan_result) # Ensure results exist
#       tryCatch(
#         {
#           openxlsx::write.xlsx(values$scan_result$.df, file)
#           showNotification("File downloaded successfully! 🎉 ", type = "message")
#         },
#         error = function(e) {
#           showNotification(paste("Error creating file:", e$message), type = "error")
#         }
#       )
#     }
#   )
}
