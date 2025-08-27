# ==============================================================================
#                               Server Interface
# ==============================================================================

## ---- Server's definitions ---------------------------------------------------

### Tab 1: ----
server <- function(input, output, session) {
  
  #### Reactive values to manage state ----
  values <- reactiveValues(
    data = NULL,
    processing = FALSE,
    file_uploaded = FALSE
  )

  #### Show progress indicator ----
  output$showProgress <- reactive({
    return(values$processing)
  })

  outputOptions(output, "showProgress", suspendWhenHidden = FALSE)

  #### Check if file is uploaded (for conditional panels)
  output$fileUploaded <- reactive({
    return(values$file_uploaded)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  #### Progress bar output ----
  output$uploadProgress <- renderUI({
    if (values$processing) {
      div(
        class = "progress",
        div(
          class = "progress-bar progress-bar-striped progress-bar-animated",
          role = "progressbar",
          style = "width: 100%"
        )
      )
    }
  })

  #### Reactive value to store uploaded data with progress
  observe({
    req(input$upload)

    ##### Start processing
    values$processing <- TRUE
    values$file_uploaded <- FALSE

    ##### Simulate progress with delay (you can remove this for real use)
    progress <- Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())

    progress$set(message = "Reading file...", value = 20)
    Sys.sleep(0.5) # Remove this in production

    progress$set(message = "Processing data...", value = 50)

    ##### Read the uploaded CSV file ----
    tryCatch(
      {
        df <- read.csv(input$upload$datapath, stringsAsFactors = FALSE)

        progress$set(message = "Finalizing...", value = 80)
        Sys.sleep(0.3) # Remove this in production

        ###### Store the data ----
        values$data <- df

        progress$set(message = "Complete!", value = 100)
        Sys.sleep(0.2) # Remove this in production

        ###### Update state ----
        values$processing <- FALSE
        values$file_uploaded <- TRUE
      },
      error = function(e) {
        values$processing <- FALSE
        values$file_uploaded <- FALSE
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  ### Display file information ----
  output$fileInfo <- renderText({
    req(input$upload, values$data)
    paste0(
      "Filename: ", input$upload$name, "\n",
      "Size: ", round(input$upload$size / 1024, 2), " KB\n",
      "Rows: ", format(nrow(values$data), big.mark = ","), "\n",
      "Columns: ", ncol(values$data)
    )
  })

  #### Display uploaded data table (max 20 rows) ----
  output$uploadedDataTable <- renderDT({
    req(values$data)

    ##### Show at most 20 rows ----
    df_preview <- head(values$data, 30)

    datatable(
      df_preview,
      rownames = FALSE,
      options = list(
        pageLength = 30,
        scrollX = TRUE,
        scrollY = "800px",
        #dom = "t", # Only show table (no search, pagination, etc.)
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      caption = if (nrow(values$data) > 30) {
        paste(
          "Showing first 30 rows of", format(
            nrow(values$data), big.mark = ","
          ), "total rows")
      } else {
        paste("Showing all", nrow(values$data), "rows")
      }
    ) |> 
      formatStyle(columns = colnames(df_preview), fontSize = "12px")
  })
}