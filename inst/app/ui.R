# ==============================================================================
#                            USER INTERFACE (UI)
# ==============================================================================

## ---- Load required libraries ------------------------------------------------

library(shiny)
library(bslib)
library(mwana)
library(rsatscan)
library(wowi)
library(dplyr)
library(DT)
library(openxlsx)
library(rlang)
library(shinycssloaders)

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

  ## ---- Tab 1: wowi Home -----------------------------------------------------

  nav_panel(
    title = strong("Home"),
    icon = icon("house"),
    # Bootstrap 5 icon name

    layout_sidebar(
      sidebar = div(
        style = "padding: 1rem;",
        tags$h4("Contents"),
        tags$h6(tags$a(href = "#sec1", "Introduction")),
        tags$h6(tags$a(href = "#sec2", "Usage")),
        tags$h6(tags$a(href = "#sec3", "Useful Resources"))
      ),
      card(
        style = "padding: 1rem;",
        tags$html(
          tags$div(
            style = "padding: 0.5rem 1rem;",

            ### Title + Logo row ----
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center;",

              #### Title ----
              tags$h3(
                style = "margin: 0; font-weight: bold;",
                list(
                  tags$code("wowi"),
                  " App: Detecting Statistically Significant Spatial Clusters of Acute Malnutrition"
                )
              ),

              #### Logo ----
              tags$a(
                href = "https://nutspatial.github.io/wowi/",
                tags$img(
                  src = "logo.jpg",
                  height = "80px",
                  alt = "wowi website",
                  style = "margin-left: 1rem;"
                )
              )
            ),

            ### Subtitle directly below, no spacing ----
            tags$h4(
              style = "margin: 0; font-weight: normal; line-height: 1.2;",
              list(
                "A simplified workflow of the ", tags$code("wowi"), " package for non-R users"
              )
            )
          ),
          tags$div(
            id = "sec1",
            style = "font-family:",
            tags$p(
              "This app is part of the", tags$code("wowi"), "package, designed for
          non-R users.", "The", tags$code("wowi"), "package is a set of utility
          functions for detecting spatial clusters - whether high-only or high
          and low rates - of acute malnutrition that are unlikely to be ocurring by
          chance. These clusters are identified using SaTScan's Bernoulli
          spatial-scan model."
            ),
            tags$p(
              "The app only does anything useful if you have SaTScan installed on
          your computer, and if the", tags$code("wowi"), "package is installed
          in R."
            ),

            ### How to use the app ----
            tags$div(
              id = "sec2",
              hr(),
              tags$h5(tags$strong("Usage")),
              tags$p(
                "The app is divided into three easy-to-manage tabs, as described below:"
              ),
              tags$ol(
                tags$li("Data Uploading"),
                tags$li("Data Wrangling"),
                tags$li("Run Spatial Scan")
              )
            ),

            #### Data uploading ----
            tags$p(tags$strong("1. Data Uploading")),
            tags$p(
              "In this tab, you are expected to upload the input dataset in 
              comma-separated format (CSV). Only this format is accepted."
            ),
            tags$p(
              tags$b(
                tags$ul(
                  tags$li("Required variables"),
                  tags$br(),
                  tags$ul(
                    tags$li("Aspatial Variables"),
                    tags$span(
                      style = "font-weight: normal; space-between;",
                      "Acute malnutrition can be defined based on weight-for-heigth
                    z-scores (WFHZ), based on Mid-Upper Arm Circumference (MUAC)
                     or based on the combination of the former two, including or
                      not bilateral oedema. The required variables to be uploaded
                       will depend on the method to be considered first and
                       foremost. Nonetheless, all in all:"
                    ),
                    tags$ul(
                      tags$div(
                        style = "font-weight: normal;",
                        tags$br(),
                        tags$li(
                          tags$b("Age:"), "must be in months. Any values outside the
                       range of 6 to 59 months old will be set as 'not applicable'.
                       The variable name must be written in lowercase (age)."
                        ),
                        tags$li(
                          tags$b("Sex:"), "must be code as 1 for boys/male and 2 for
                       girls/female. The variable name must be written in lowercase
                       (sex)."
                        ),
                        tags$li(
                          tags$b("MUAC:"), "must be in centimetres."
                        ),
                        tags$li(
                          tags$b("Weight:"), "must be in Kilograms."
                        ),
                        tags$li(
                          tags$b("Height:"), "must be in centimetres."
                        ),
                        tags$li(
                          tags$b("Oedema:"), "must be coded as 'y' for yes and 'n'
                       for no."
                        )
                      )
                    )
                  ),
                  tags$br(),
                  tags$ul(
                    tags$li("Spatial Variables"),
                    tags$div(
                      style = "font-weight: normal;",
                      tags$ul(
                        tags$li(
                          tags$b("Latitude: x-axis")
                        ),
                        tags$li(
                          tags$b("Longitude: y-axis")
                        )
                      )
                    ),
                    br(),
                    tags$p(
                      style = "font-weight: normal",
                      "Once uploaded, the file becomes available for use in the
                       following tab."
                    ),
                  )
                )
              ),

              #### Data wrangling ----
              tags$p(tags$strong("2. Data Wrangling")),
              tags$p(
                "The wrangling workflow consists in calculating z-scores,
              identifying outliers, flagging them, and defining acute malnutrition.
              You must select the method on which the wrangling should be based.
              This step of the analysis uses data wranglers from the",
                tags$code("mwana"), "package under the hood. Read more about them
              in the resources provided below."
              ),
              tags$p("In this process, the variable oedema is optional."),
              tags$p("Once wrangled, the data becomes available for use in the
            following tab.")
            ),

            #### Run Spatial Scan ----
            tags$p(tags$strong("3. Run Spatial Scan")),
            tags$p(
              "In this tab, begin by specifying the scope of your analysis: either
            single-area or multiple-area. Single-area analysis applies when your
             dataset contains only one area (e.g., district, county), and the
             scan should be run within that area. Multiple-area analysis applies
              when your dataset includes several areas, and the scan should be
              run across them. If conducting a single-area analysis, enter the
              name of the area under review in the corresponding input field.
              Otherwise, for multiple-area analysis, specify the variable in your
               dataset that contains the area names. Once complete, fill in the
               remaining fields as appropriate, then click Run Scan to initiate
               the process."
            ),
            tags$p(
              "Once the scan is complete, several files will be saved in the
            directory you specified earlier. A list of these files will appear
            under the 'Created Files' section. Additionally, the 'Summary Results
            of Detected Clusters' section will display a table showing the
            clusters identified in each analysis area. You can download the
            output table by clicking the download button, which becomes available
            once the scan has finished."
            )
          ),

          #### Useful Resources ----
          tags$div(
            id = "sec3",
            hr(),
            tags$h5(tags$strong("Useful Resources")),
            tags$p("Read more about:"),
            tags$ul(
              tags$li(
                tags$code("mwana"),
                tags$a(href = "https://nutriverse.io/mwana/dev/", "here")
              ),
              tags$li(
                tags$code("wowi"),
                tags$a(href = "https://nutspatial.github.io/wowi/", "here")
              ),
              tags$li(
                tags$code("SaTScan"),
                tags$a(href = "https://www.satscan.org", "here")
              )
            )
          )
        )
      ),

      ### Footnote ----
      tags$footer(
        "Made with ❤️ by Tomás Zaba",
        style = "color: grey; font-size: 12px; text-align: right;"
      )
    )
  ),

  ## ---- Tab 2: Data Uploading ------------------------------------------------
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

  ## ---- Tab 3: Data Wrangling ------------------------------------------------

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
        withSpinner(
          ui_element = DTOutput("wrangled_data"),
          type = 1,
          color = "#398DF3",
          caption = div("Wrangling data", br(), h5("Please wait..."))
        )
      )
    )
  ),

  ## ---- Tab 4: Run Spatial Scan ----------------------------------------------
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
              card_header("Created Files"),
              verbatimTextOutput("files_created")
            )
          ),

          ### Second column: table card ----
          card(
            card_header("Summary Results of Detected Clusters"),
            withSpinner(
              ui_element = DTOutput("clusters"),
              type = 3,
              color.background = "#398DF3",
              caption = div("Scanning through", br(), h5("Please wait..."))
            ),
            uiOutput(outputId = "download")
          )
        )
      )
    )
  )
)
