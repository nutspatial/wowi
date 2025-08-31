# ==============================================================================
#                             User Interface (UI)
# ==============================================================================

## ---- Load required libraries ------------------------------------------------

library(shiny)
library(shinythemes)
library(bslib)
library(mwana)
library(rsatscan)
library(wowi)
library(shinyFeedback)
library(DT)

## ---- User's navigation bars -------------------------------------------------

ui <- page_navbar(
  title = "wowi",

  ### Tab 1: Data Uploading ----
  nav_panel(
    title = "Data Uploading",
    icon = icon(name = "home"),
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

  ### Tab 2: Data Wrangling ----
  nav_panel(
    title = "Data Wrangling",
    layout_sidebar(
      sidebar = sidebar(
        width = 500,
        width = 500,
        card(
          card_header("Data Wrangling"),
          radioButtons(
            inputId = "wrangle",
            label = "Which method should be used for data wrangling?",
            choices = list(
              "Weight-for-Height z-score" = "WHZ", 
              "MUAC-for-Age z-score" = "MFAZ", 
            choices = list(
              "Weight-for-Height z-score" = "WHZ",
              "MUAC-for-Age z-score" = "MFAZ",
              "Raw MUAC" = "MUAC"
            ), selected = "WHZ"
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

  ### Tab 3: Run Spatial Scan ----
  nav_panel(
    title = "Run Spatial Scan", 
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
          hr()
        ), 
        card("Created Files")
    ), 
    card(card_header("Results: Map")),
    plotOutput("cluster_map"), 
    card(card_header("Results: Table")),
    DTOutput("cluster_df")
    )
    title = "Run Spatial Scan",
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
          hr()
        ),
        card("Created Files")
      ),
      card(card_header("Results: Map")),
      plotOutput("cluster_map"),
      card(card_header("Results: Table")),
      DTOutput("cluster_df")
    )
  )
)
