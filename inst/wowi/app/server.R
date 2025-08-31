# ==============================================================================
#                               SERVER LOGIC
# ==============================================================================

## ---- Server's definitions ---------------------------------------------------

server <- function(input, output, session) {
  values <- reactiveValues(
    data = NULL,
    processing = FALSE,
    file_uploaded = FALSE,
    wrangled = NULL
  )

  output$showProgress <- reactive({
    values$processing
  })
  outputOptions(output, "showProgress", suspendWhenHidden = FALSE)

  output$fileUploaded <- reactive({
    values$file_uploaded
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

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

  observe({
    req(input$upload)
    values$processing <- TRUE
    values$file_uploaded <- FALSE

    progress <- Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())

    progress$set(message = "Reading file...", value = 20)
    Sys.sleep(0.5)

    progress$set(message = "Processing data...", value = 50)

    tryCatch(
      {
        df <- read.csv(input$upload$datapath, stringsAsFactors = FALSE)
        progress$set(message = "Finalizing...", value = 80)
        Sys.sleep(0.3)

        values$data <- df
        progress$set(message = "Complete!", value = 100)
        Sys.sleep(0.2)

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

  output$fileInfo <- renderText({
    req(input$upload, values$data)
    paste0(
      "Filename: ", input$upload$name, "\n",
      "Size: ", round(input$upload$size / 1024, 2), " KB\n",
      "Rows: ", format(nrow(values$data), big.mark = ","), "\n",
      "Columns: ", ncol(values$data)
    )
  })

  output$uploadedDataTable <- renderDataTable({
    req(values$data)
    df_preview <- head(values$data, 30)
    datatable(df_preview,
      rownames = FALSE,
      options = list(
        pageLength = 30, scrollX = TRUE, scrollY = "800px",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      caption = if (nrow(values$data) > 30) {
        paste(
          "Showing first 30 rows of",
          format(nrow(values$data), big.mark = ","),
          "total rows"
        )
      } else {
        paste("Showing all", nrow(values$data), "rows")
      }
    ) |> formatStyle(columns = colnames(df_preview), fontSize = "12px")
  })

  output$variableSelectors <- renderUI({
    req(values$data)
    cols <- names(values$data)

    switch(input$wrangle,
      "WHZ" = tagList(
        selectInput(inputId = "sex_var", label = "Sex", choices = cols),
        selectInput(inputId = "weight_var", label = "Weight (kg)", choices = cols),
        selectInput(inputId = "height_var", label = "Height (cm)", choices = cols)
      ),
      "MFAZ" = tagList(
        selectInput(inputId = "age_var", label = "Age (months)", choices = cols),
        selectInput(inputId = "sex_var", label = "Sex", choices = cols),
        selectInput(inputId = "muac_var_mfaz", label = "MUAC (cm)", choices = cols)
      ),
      "MUAC" = tagList(
        selectInput(inputId = "sex_var", label = "Sex", choices = cols),
        selectInput(inputId = "muac_var", label = "MUAC (cm)", choices = cols)
      )
    )
  })

  observeEvent(input$apply_wrangle, {
    req(values$data)
    data <- values$data
    valid <- TRUE
    msg <- ""

    if (input$wrangle == "WHZ") {
      if (input$sex_var == "" || input$weight_var == "" || input$height_var == "") {
        valid <- FALSE
        msg <- "Please select all required variables (Sex, Weight, Height) for WHZ method."
      }
    } else if (input$wrangle == "MFAZ") {
      if (input$sex_var == "" || input$muac_var_mfaz == "" || input$age_var == "") {
        valid <- FALSE
        msg <- "Please select all required variables (Sex, MUAC, Age) for MFAZ method."
      }
    } else if (input$wrangle == "MUAC") {
      if (input$sex_var == "" || input$muac_var == "") {
        valid <- FALSE
        msg <- "Please select all required variables (Sex, MUAC) for MUAC method."
      }
    }

    if (!valid) {
      showNotification(msg, type = "error")
      return()
    }

    tryCatch(
      {
        result <- switch(input$wrangle,
          "WHZ" = mw_wrangle_wfhz(
            df = data,
            sex = data[[input$sex_var]],
            weight = data[[input$weight_var]],
            height = data[[input$height_var]]
          ),
          "MFAZ" = {
            req(input$age_var, input$muac_var_mfaz, input$sex_var)

            df <- data

            # Defensive assignment
            if (input$muac_var_mfaz %in% names(df)) {
              df$muac <- df[[input$muac_var_mfaz]]
            } else {
              showNotification("MUAC variable not found in dataset.", type = "error")
              return(NULL)
            }

            df$age <- df[[input$age_var]]
            df$sex <- df[[input$sex_var]]

            # Apply age wrangling
            df <- mw_wrangle_age(df = df, age = df$age)

            # Apply MUAC wrangling
            mw_wrangle_muac(
              df = df,
              sex = df$sex,
              muac = df$muac,
              age = df$age,
              .recode_sex = FALSE,
              .recode_muac = FALSE,
              .to = "none"
            )
          },
          "MUAC" = mw_wrangle_muac(
            df = data,
            sex = data[[input$sex_var]],
            muac = data[[input$muac_var]],
            .recode_muac = TRUE,
            .to = "cm",
            .recode_sex = FALSE
          )
        )

        values$wrangled <- result
      },
      error = function(e) {
        showNotification(paste("Wrangling error:", e$message), type = "error")
      }
    )
  })

  output$wrangled_data <- renderDT({
    req(values$wrangled)
    datatable(values$wrangled, options = list(
      pageLength = 30, scrollX = TRUE
    ))
  })

  ## ---- Logic for Tab 3: Run Spatial Scan ------------------------------------

  output$hyperparameters <- renderUI({
    req(values$data)

    ### Collect user-defined parameters based on scope of analysis ----
    switch(input$analysis_scope,
      "single-area" = tagList(
        textInput(
          inputId = "filename",
          label = tagList(
            strong("Area of Analysis"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              "Name of a district, county, etc, as in your dataset"
            )
          )
        ),
        textInput(
          inputId = "directory",
          label = strong(
            "Directory where the parameters files should be saved in"
          ),
          value = ""
        ),
        textInput(
          inputId = "sslocation",
          label = tagList(
            strong("Path to where SaTScan GUI is installed on your computer"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              'e.g., macOS: "/Applications/SaTScan.app/Contents/app";
            Windows: "C:/Program Files/SaTScan"'
            )
          ),
          value = "",
          width = NULL
        ),
        textInput(
          inputId = "ssbatchfilename",
          label = tagList(
            strong("SaTScan batch file name"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              'e.g., macOS: "satscan"; Windows: "SaTScanBatch64"'
            )
          )
        ),
        textInput(
          inputId = "satscan_version",
          label = tagList(
            strong("Version of SaTScan"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              'e.g., "10.3.2"'
            )
          )
        ),
        radioButtons(
          inputId = "scan_for",
          label = strong("Which type of cluster should be scanned for?"),
          choices = list(
            "Clusters of High Rates" = "high-rates",
            "Cluster of High and Low Rates" = "high-low-rates"
          ),
          selected = "high-low-rates"
        ),
        radioButtons(
          inputId = "gam_based",
          label = strong("Which acute malnutrition's case-definition should be used?"),
          choices = list(
            "Weight-for-Height z-scores" = "wfhz",
            "Mid-upper Arm Circumference" = "MUAC",
            "Combined Case Definition" = "combined"
          ),
          selected = "wfhz"
        )
      ),
      "multiple-area" = tagList(
        textInput(
          inputId = "directory",
          label = strong(
            "Directory where the parameters files should be saved in"
          ),
          value = ""
        ),
        textInput(
          inputId = "sslocation",
          label = tagList(
            strong("Path to where SaTScan GUI is installed on your computer"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              'e.g., macOS: "/Applications/SaTScan.app/Contents/app";
            Windows: "C:/Program Files/SaTScan"'
            )
          ),
          value = "",
          width = NULL
        ),
        textInput(
          inputId = "ssbatchfilename",
          label = tagList(
            strong("SaTScan batch file name"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              'e.g., macOS: "satscan"; Windows: "SaTScanBatch64"'
            )
          )
        ),
        textInput(
          inputId = "satscan_version",
          label = tagList(
            strong("Version of SaTScan"),
            tags$div(
              style = "font-size: 0.85em; color: #6c757d;",
              'e.g., "10.3.2"'
            )
          )
        ),
        radioButtons(
          inputId = "scan_for",
          label = strong("Which type of cluster should be scanned for?"),
          choices = list(
            "Clusters of High Rates" = "high-rates",
            "Cluster of High and Low Rates" = "high-low-rates"
          ),
          selected = "high-low-rates"
        ),
        radioButtons(
          inputId = "gam_based",
          label = strong("Which acute malnutrition's case-definition should be used?"),
          choices = list(
            "Weight-for-Height z-scores" = "wfhz",
            "Mid-upper Arm Circumference" = "MUAC",
            "Combined Case Definition" = "combined"
          ),
          selected = "wfhz"
        )
      )
    )
  })
}