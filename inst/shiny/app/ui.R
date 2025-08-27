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
            label = p("Upload a CSV file"),
            buttonLabel = "Browse...",
            accept = ".csv"
          ),
          #### Progress bar for file processing ----
          conditionalPanel(
            condition = "output.showProgress",
            hr(),
            h4("Processing File..."),
            uiOutput("uploadProgress")
          ),
          #### Show file info when uploaded ----
          conditionalPanel(
            condition = "output.fileUploaded",
            hr(),
            h5("File Information"),
            verbatimTextOutput(outputId = "fileInfo")
          )
        )
      ),

      #### Main content area ----
      card(
        card_header("Uploaded Data Preview"),
        conditionalPanel(
          condition = "output.fileUploaded",
          DTOutput("uploadedDataTable")
        ),
        conditionalPanel(
          condition = "output.showProgress",
          div(
            style = "text-align: center; padding: 50px;",
            div(class = "spinner-border text-primary", role = "status"),
            h4("Loading data...", style = "color: #007bff; margin-top: 20px;"),
            p("Please wait while we process your file.")
          )
        ),
        conditionalPanel(
          condition = "!output.fileUploaded",
          div(
            style = "text-align: center; padding: 50px;",
            h4("No file uploaded yet", style = "color: #6c757d;"),
            p("Please upload a CSV file to see the data preview.")
          )
        )
      )
    )
  ),

  ### Tab 2: Data wrangling ----
  nav_panel(
    title = "Data wrangling"
  ),


  ### Tab 3: Run Spatial Scan ----
  nav_panel(
    title = "Run Spatial Scan"
  )
)