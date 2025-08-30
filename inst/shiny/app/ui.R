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
    title = "Data uploading",
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
    title = "Data wrangling",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        card(
          card_header("Data wrangling"),
          radioButtons(
            inputId = "wrangle",
            label = "Which method should be used for data wrangling?",
            choices = c("WHZ", "MFAZ", "MUAC"), selected = "WHZ"
          ),
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
    title = "Run Spatial Scan"
  )
)
