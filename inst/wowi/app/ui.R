# ==============================================================================
#                            USER INTERFACE (UI)
# ==============================================================================

## ---- Load required libraries ------------------------------------------------

library(shiny)
library(bslib)
library(mwana)
library(rsatscan)
library(wowi)
library(shinyFeedback)
library(DT)
library(openxlsx)

## ---- User's navigation bars -------------------------------------------------

ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center;",
    span("wowi", style = "margin-right: 10px;"),
    a(
      href = "https://nutspatial.github.io/wowi/",
      img(src = "logo.jpg", height = "25px")
    )
  ),

  ### Tab 1: wowi Home ----
  nav_panel(
    title = strong("Home"), 
    icon = icon(name = "home")
  ),

  ### Tab 2: Data Uploading ----
  nav_panel(
    title = strong("Data Uploading"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        card(
          card_header("Upload Data Here"),
          style = "width: 350px;",
          fileInput(
            inputId = "upload",
            label = "Upload a CSV file",
            buttonLabel = "Browse...",
            accept = ".csv"
          ),
          conditionalPanel(
            condition = "output.showProgress",
            hr(),
            h4("Processing File..."),
            uiOutput("uploadProgress")
          ),
          conditionalPanel(
            condition = "output.fileUploaded",
            hr(),
            h5("File Information"),
            verbatimTextOutput("fileInfo")
          )
        )
      ),
      card(
        card_header("Uploaded Data Preview"),
        conditionalPanel("output.fileUploaded", DTOutput("uploadedDataTable")),
        conditionalPanel("output.showProgress", div(
          style = "text-align: center; padding: 50px;",
          div(class = "spinner-border text-primary", role = "status"),
          h4("Loading data...", style = "color: #007bff; margin-top: 20px;"),
          p("Please wait while we process your file.")
        )),
        conditionalPanel("!output.fileUploaded", div(
          style = "text-align: center; padding: 50px;",
          h4("No file uploaded yet", style = "color: #6c757d;"),
          p("Please upload a CSV file to see the data preview.")
        ))
      )
    )
  ),

  ### Tab 3: Data Wrangling ----
  nav_panel(
    title = strong("Data Wrangling"),
    layout_sidebar(
      sidebar = sidebar(
        width = 500,
        card(
          card_header("Data Wrangling"),
          radioButtons(
            inputId = "wrangle",
            label = strong("How should acute malnutrition be defined?"),
            choices = list(
              "Weight-for-Height z-score" = "wfhz",
              "Mid-Upper Arm Circumference" = "muac",
              "Combined Case Definition" = "combined"
            ), selected = "wfhz"
          ),
          helpText(strong("Select the variables")),
          uiOutput("variableSelectors"),
          br(),
          actionButton(
            inputId = "apply_wrangle",
            label = "Apply Data Wrangling",
            class = "btn-primary"
          )
        )
      ),
      card(
        card_header("Wrangled Data"),
        DTOutput("wrangled_data")
      )
    )
  ),

  ### Tab 4: Run Spatial Scan ----
  nav_panel(
    title = strong("Run Spatial Scan"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        card(
          card_header("Set wowi Hyperparameters for Analysis"),
          radioButtons(
            inputId = "analysis_scope",
            label = strong("What is the analysis scope that wowi should consider?"),
            choices = c(
              "Single-area analysis" = "single-area",
              "Multiple-area analysis" = "multiple-area"
            ),
            selected = "single-area"
          ),
          #### Container in UI to store outputs derived from the server ----
          uiOutput("hyperparameters"),
          actionButton(
            inputId = "run_scan",
            label = "Run Scan",
            class = "btn-primary"
          )
        )
      ),
      layout_column_wrap(
        width = "100%",
        layout_columns(
          col_widths = c(3, 9), # First column narrower
          row_heights = NULL, # Let height flow naturally

          ### First column: full-height card
          div(
            style = "height: 100vh;", # Full viewport height
            card(
              card_header("Files Created"),
              verbatimTextOutput("files_created")
            )
          ),

          ### Second column: table card
          card(
            card_header("Results: Table"),
            DTOutput("clusters"), 
            uiOutput(outputId = "download")
          )
        )
      )
    )
  )
)
