# ==============================================================================
#                               SERVER LOGIC
# ==============================================================================

## ---- Server's definitions ---------------------------------------------------

server <- function(input, output, session) {
  values <- reactiveValues(
    data = NULL,
    processing = FALSE,
    file_uploaded = FALSE,
    wrangled = NULL,
    scan_result = NULL
  )

  ### Show input data upload progress bar ----
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

  ### Dynamic content for use/display over input data uploading process ----
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

  ### Display details about input data ----
  output$fileInfo <- renderText({
    req(input$upload, values$data)
    paste0(
      "Filename: ", input$upload$name, "\n",
      "Size: ", round(input$upload$size / 1024, 2), " KB\n",
      "Rows: ", format(nrow(values$data), big.mark = ","), "\n",
      "Columns: ", ncol(values$data)
    )
  })

  ### Display and prettify input data ----
  output$uploadedDataTable <- renderDataTable({
    req(values$data)
    df_preview <- head(values$data, 30)
    datatable(df_preview,
      rownames = FALSE,
      options = list(
        pageLength = 30, scrollX = FALSE, scrollY = "800px",
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

  ## ---- Logic for Data Wrangling Tab -----------------------------------------

  ### List of variables to be displayed to user based on wrangling method ----
  output$variableSelectors <- renderUI({
    req(values$data)
    cols <- names(values$data)

    switch(input$wrangle,

      #### Weight-for-heigh ----
      "wfhz" = tagList(
        selectInput(
          inputId = "sex",
          label = "Sex",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "weight",
          label = "Weight (kg)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "height",
          label = "Height (cm)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "oedema",
          label = "Oedema",
          choices = c("", cols)
        )
      ),

      #### Mid-Upper Arm Circumference (wrangling based on MUAC-for-age ) ----
      "muac" = tagList(
        selectInput(
          inputId = "age",
          label = "Age (months)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "sex",
          label = "Sex",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "muac",
          label = "MUAC (cm)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "oedema",
          label = "Oedema",
          choices = c("", cols)
        )
      ),

      #### Both weight-for-height and MUAC ----
      "combined" = tagList(
        selectInput(
          inputId = "sex",
          label = "Sex",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "age",
          label = "Age (months)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "weight",
          label = "Weight (kg)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "height",
          label = "Height (cm)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "muac",
          label = "MUAC (cm)",
          choices = c("", cols)
        ),
        selectInput(
          inputId = "oedema",
          label = "Oedema",
          choices = c("", cols)
        )
      )
    )
  })

  observeEvent(input$apply_wrangle, {
    #### Ensure input data from Tab 1 exists before rendering ----
    req(values$data)
    data <- values$data
    valid <- TRUE

    #### Manage errors gracefully ----
    msg <- ""

    if (input$wrangle == "wfhz") {
      if (input$sex == "" || input$weight == "" || input$height == "") {
        valid <- FALSE
        msg <- "😬 Please select all required variables (Sex, Weight, Height) and try again."
      }
    } else if (input$wrangle == "muac") {
      if (input$sex == "" || input$muac == "" || input$age == "") {
        valid <- FALSE
        msg <- "😬 Please select all required variables (Sex, MUAC, Age) and try again."
      }
    } else if (input$wrangle == "combined") {
      if (
        any(c(
          input$age, input$sex, input$muac, input$weight,
          input$height
        ) == "")) {
        valid <- FALSE
        msg <-
          "😬 Please select all required variables (Age, Sex, Weight, Height, MUAC)."
      }
    }

    if (!valid) {
      showNotification(msg, type = "error")
      return()
    }

    #### Apply data wrangling and manage errors gracefully ----
    tryCatch(
      {
        result <- switch(input$wrangle,
          "wfhz" = {
            req(input$sex, input$weight, input$height)

            data |>
              dplyr::select(
                sex = !!sym(input$sex),
                weight = !!sym(input$weight),
                height = !!sym(input$height),
                oedema = if (input$oedema != "") !!sym(input$oedema) else NULL,
                everything()
              ) |>
              mw_wrangle_wfhz(
                sex = sex,
                .recode_sex = FALSE,
                weight = weight,
                height = height
              ) |>
              mwana::define_wasting(
                zscores = wfhz,
                .by = "zscores",
                edema = oedema
              )
          },
          "muac" = {
            req(input$age, input$muac, input$sex)

            # Create a working dataset with standardized column names
            data |>
              dplyr::select(
                age = !!sym(input$age),
                muac = !!sym(input$muac),
                sex = !!sym(input$sex),
                oedema = if (input$oedema == "") NULL else !!sym(input$oedema),
                everything()
              ) |>
              dplyr::mutate(muac = as.numeric(muac)) |>
              mw_wrangle_age(age = age) |>
              mw_wrangle_muac(
                sex = sex,
                .recode_sex = FALSE,
                muac = muac,
                .recode_muac = FALSE,
                .to = "none",
                age = age
              ) |>
              dplyr::mutate(muac = mwana::recode_muac(muac, .to = "mm")) |>
              define_wasting(muac = muac, .by = "muac", edema = oedema)
          },
          "combined" = {
            req(
              input$sex, input$weight, input$height,
              input$age, input$muac
            )

            data |>
              dplyr::select(
                sex = !!sym(input$sex),
                weight = !!sym(input$weight),
                height = !!sym(input$height),
                age = !!sym(input$age),
                muac = !!sym(input$muac),
                oedema = if (input$oedema != "") !!sym(input$oedema) else NULL,
                everything()
              ) |>
              mwana::mw_wrangle_wfhz(
                sex = sex,
                .recode_sex = FALSE,
                weight = weight,
                height = height
              ) |>
              mwana::mw_wrangle_age(age = age) |>
              mwana::mw_wrangle_muac(
                sex = sex,
                .recode_sex = FALSE,
                muac = muac,
                .recode_muac = FALSE,
                .to = "none",
                age = age
              ) |>
              dplyr::mutate(muac = mwana::recode_muac(muac, .to = "mm")) |>
              mwana::define_wasting(
                zscores = wfhz,
                muac = muac,
                .by = "combined",
                edema = oedema
              )
          }
        )

        values$wrangled <- result
      },
      error = function(e) {
        showNotification(paste("Wrangling error:", e$message), type = "error")
      }
    )
  })

  #### Display wrangled data ----
  output$wrangled_data <- renderDT({
    req(values$wrangled)
    datatable(
      data = head(values$wrangled, 30),
      rownames = FALSE,
      options = list(
        pageLength = 30,
        scrollX = FALSE,
        scrolly = "800px",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      caption = if (nrow(values$wrangled) > 30) {
        paste(
          "Showing first 30 rows of", format(nrow(values$wrangled), big.mark = ","),
          "total rows"
        )
      } else {
        paste("showing all", nrow(values$wrangled), "rows")
      },
      style = "default",
      filter = "top"
    ) |> formatStyle(columns = colnames(values$wrangled), fontSize = "12px")
  })

  ## ---- Logic for Tab 3: Run Spatial Scan ------------------------------------

  output$hyperparameters <- renderUI({
    ### Ensure data exists before rendering ----
    req(values$wrangled)

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
        )
      ),

      ### Multiple-area analysis ----
      "multiple-area" = tagList(
        selectInput(
          inputId = "area",
          label = strong("Area"),
          choices = c("", names(values$wrangled))
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
        )
      )
    )
  })

  ### Logic for calculations ----
  observeEvent(
    eventExpr = input$run_scan,
    {
      #### Ensure data exists before rendering ----
      req(values$wrangled)

      #### Logic for single-area spatial scan ----
      if (input$analysis_scope == "single-area") {
        #### Ensure that all parameters for single-area analysis are given ----
        req(
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
        gam_based <- as.character(input$wrangle)

        data <- values$wrangled |>
          dplyr::rename(longitude = x, latitude = y)

        #### Run scan ----
        result <- ww_run_satscan(
          .data = data,
          filename = area,
          dir = dir,
          sslocation = satscan_location,
          ssbatchfilename = batchfilename,
          satscan_version = version,
          .by_area = FALSE,
          .scan_for = scan_for,
          .gam_based = gam_based,
          area = NULL
        )

        values$scan_result <- result

        #### Display the list of files in the given directory ----
        output$files_created <- renderText({
          files <- list.files(path = dir, all.files = TRUE, full.names = FALSE)
          paste(files, collapse = "\n")
        })
      } else {
        #### Ensure that all parameters for single-area analysis are given ----
        req(
          input$directory, input$sslocation, input$ssbatchfilename,
          input$satscan_version, input$scan_for
        )

        #### Make user-defined parameters wowi-usable inputs ----
        dir <- as.character(input$directory)
        satscan_location <- as.character(input$sslocation)
        batchfilename <- as.character(input$ssbatchfilename)
        version <- as.character(input$satscan_version)
        scan_for <- as.character(input$scan_for)
        gam_based <- as.character(input$wrangle)

        #### Run scan ----
        data <- values$wrangled |>
          dplyr::rename(longitude = x, latitude = y)
        result <- ww_run_satscan(
          .data = data,
          filename = NULL,
          dir = dir,
          sslocation = satscan_location,
          ssbatchfilename = batchfilename,
          satscan_version = version,
          .by_area = TRUE,
          .scan_for = scan_for,
          .gam_based = gam_based,
          area = data[[input$area]]
        )

        values$scan_result <- result

        output$files_created <- renderText({
          files <- list.files(path = dir, all.files = TRUE, full.names = FALSE)
          paste(files, collapse = "\n")
        })
      }
    }
  )

  #### Display a summary table of detected cluster and prettified ----
  output$clusters <- renderDT({
    # Ensure data exists before rendering ----
    req(values$scan_result)

    ##### Display the first 8 rows only ----
    datatable(
      data = head(values$scan_result$.df, 5),
      rownames = FALSE,
      options = list(
        scrollX = FALSE,
        scrolly = "800px",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      caption = if (nrow(values$scan_result$.df) > 5) {
        paste(
          "Showing first 5 rows of", format(nrow(values$scan_result$.df), big.mark = ","),
          "total rows"
        )
      } else {
        paste("showing all", nrow(values$scan_result$.df), "rows")
      },
      style = "default",
      filter = "top"
    ) |> formatStyle(columns = colnames(values$scan_result$.df), fontSize = "12px")
  })

  #### Download button to download table of detected clusters in .xlsx ----
  ##### Output into the UI ----
  output$download <- renderUI({
    req(values$scan_result)
    div(
      style = "margin-bottom: 15px; text-align: right;",
      downloadButton(
        outputId = "downloadResults",
        label = "Download Clusters",
        class = "btn-primary",
        icon = icon(name = "download", class = "fa-lg")
      )
    )
  })

  ##### Downloadable results by clicking on the download button ----
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste0("detected-clusters_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$scan_result) # Ensure results exist
      tryCatch(
        {
          openxlsx::write.xlsx(values$scan_result$.df, file)
          showNotification("File downloaded successfully! 🎉 ", type = "message")
        },
        error = function(e) {
          showNotification(paste("Error creating file:", e$message), type = "error")
        }
      )
    }
  )
}