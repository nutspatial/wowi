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

ui <- tagList(

  ### Connect to CSS stylesheet ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  page_navbar(
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",

      ### Left side of the page navigation bar: app name and logo ----
      tags$div(
        style = "display: flex; align-items: center;",
        tags$span("wowi",
          style = "margin-right: 10px; font-family: Arial, sans-serif; font-size: 50px;"
        ),
        tags$a(
          href = "https://nutspatial.github.io/wowi/",
          tags$span(
            tags$img(src = "logo.png", height = "40px"),
            style = "margin-right: 20px;"
          )
        )
      ),

      ### Right side of the page navigation bar ----
      tags$span("v.1.0.0",
        id = "app-version",
        style = "font-size: 12.5px; color:  rgba(31, 42, 68, 0.58);
        position: fixed; top: 40px; right: 20px;"
      )
    ),

    ## ---- Tab 1: wowi Home ---------------------------------------------------

    nav_panel(
      title = "Home",
      icon = icon("house"),

      ### Left sidebar for contents ----
      layout_sidebar(
        sidebar = tags$div(
          style = "padding: 1rem;",
          tags$h4("Contents"),
          tags$h6(tags$a(href = "#sec1", "Welcome")),
          tags$h6(tags$a(href = "#sec2", "Data Upload")),
          tags$h6(tags$a(href = "#sec3", "Data Wrangling")),
          tags$h6(tags$a(href = "#sec4", "Spatial Scan")),
          tags$h6(tags$a(href = "#sec5", "Authorship")),
          tags$h6(tags$a(href = "#sec6", "License")),
          tags$h6(tags$a(href = "#sec7", "Useful Resources"))
        ),
        card(
          style = "padding: 1rem; background-color: #fbfdfd;",
          tags$html(
            tags$div(
              style = "padding: 0.5rem 1rem;",

              ### Title + Logo row ----
              tags$div(
                style = "display: flex; justify-content: space-between; align-items: center;",

                #### Title ----
                tags$h3(
                  style = "marging-top: 30px; font-weight: bold;",
                  "
                    Detecting Statistically Significant Spatial Clusters of
                    Acute Malnutrition
                    "
                )
              ),

              ### Logo ----
              tags$a(
                href = "https://nutspatial.github.io/wowi/",
                tags$img(
                  src = "logo.png",
                  height = "160px",
                  alt = "wowi website",
                  align = "right",
                  style = " margin-top: -40px; margin-right: 20px;"
                )
              ),

              ### Subtitle directly below, no spacing ----
              tags$h4(
                style = "fmargin: 0; font-weight: normal; line-height: 1.2;",
                list(
                  "A simplified workflow of the ", tags$code("wowi"), " package for non-R users"
                )
              )
            ),
            tags$div(
              id = "sec1",
              style = "text-align: justify;",
              tags$hr(),
              tags$p(
                "
                This app is a lightweight, field-ready and handy tool thoughtful
                conceived to help users detect spatial clusters - whether
                high-only or high and low rates - of acute malnutrition that are
                unlikely to be ocurring by chance. It automates the key analysis
                workflow of the package", tags$code("wowi"), "for non-R users.
                "
              ),
              tags$p(
                "
                The app only does anything useful if you have SaTScan installed on
                your computer, and if the", tags$code("mwana"), "package is installed in R.
                "
              ),
              tags$p(
                "
                  The app is divided into three easy-to-navigate tabs, apart from
                  the Home - where you are now:
                  "
              ),
              tags$ol(
                tags$li(tags$b("Data Upload")),
                tags$li(tags$b("Data Wrangling")),
                tags$li(tags$b("Spatial Scan"))
              )
            ),

            #### Data uploading ----
            tags$div(
              id = "sec2",
              style = "text-align: justify;",
              tags$hr(),
              tags$p(tags$b("1. Data Upload")),
              tags$p(
                "
                This is where the workflow begins. Upload the dataset saved in a
                comma-separated-value format (.csv); this is the only accepted
                format. Click on the 'Browse' button to locate the file to be
                uploaded from your computer; it is as simple as that.
                Once uploaded, the first 20 rows will be priviewed on the right side.
                "
              ),
              tags$p(
                tags$ul(
                  tags$li(tags$b("Data requirements")),
                  tags$p(
                    "
                        The data to be uploaded must have been tidy up in accordance
                        to the below-described app's", tags$b("input file"), "and",
                    tags$b("input variable"), "requirements:
                        "
                  ),
                  tags$br(),
                  tags$ul(
                    tags$li(
                      tags$b("Input file requirements"),
                      tags$p(
                        tags$b("File naming:"), "the file name must use
                            underscore ( _ ) to separate words. Hyphen ( - ) or
                            simple spaces will lead to errors along the uploading
                            process. Consider the following naming example:",
                        tags$em("my_file_to_upload.csv")
                      )
                    ),
                    tags$br(),
                    tags$li(tags$b("Aspatial Variables")),
                    tags$span(
                      style = "font-weight: normal; space-between;",
                      "
                        Acute malnutrition can be defined based on either
                        Weight-for-Heigth z-scores (WFHZ) or Mid-Upper Arm
                        Circumference (MUAC), or even based on the combination
                        of the former two - including or not bilateral oedema.
                        On this note, the required variables to be uploaded
                        will depend on the method to be considered in the first
                        place. Nonetheless, all in all:
                        "
                    ),
                    tags$ul(
                      tags$div(
                        style = "font-weight: normal;",
                        tags$br(),
                        tags$li(
                          tags$b("Age:"),
                          "
                            values must be in months. Any values outside the
                            range of 6 to 59 months old will be set as 'not
                            applicable'. The variable name must be written in
                            lowercase ('age').
                            "
                        ),
                        tags$li(
                          tags$b("Sex:"),
                          "
                            values must be given in 'm' for boys/male and 'f' for
                            girls/female. The variable name must be written in lowercase
                            ('sex').
                            "
                        ),
                        tags$li(
                          tags$b("MUAC:"),
                          "
                            values must be given in millimetres. Ensure that there
                            no strange numbers, such as '114.1'. The presence of
                            decimal places (even if one case) will raise error
                            in the data wrangling tab and hault the app.
                            "
                        ),
                        tags$li(
                          tags$b("Weight:"), "values must be given in Kilograms."
                        ),
                        tags$li(
                          tags$b("Height:"), "values must be given in centimetres."
                        ),
                        tags$li(
                          tags$b("Oedema:"),
                          "
                            values must be given in 'y' for yes and 'n' for no.
                            "
                        )
                      )
                    )
                  ),
                  tags$br(),
                  tags$ul(
                    tags$li(tags$b("Spatial Variables")),
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
                    )
                  )
                )
              ),

              #### Data wrangling ----
              tags$div(
                id = "sec3",
                style = "text-align: justify;",
                tags$hr(),
                tags$p(tags$b("Data Wrangling")),
                tags$p(
                  "
                  The wrangling workflow consists in calculating z-scores,
                  identifying outliers, flagging them, and defining acute malnutrition.
                  You must select the method on which the wrangling should be based.
                  This step of the analysis uses data wranglers from the
                  ",
                  tags$code("mwana"),
                  "
                  package under the hood. Read more about them in the resources
                  provided below.
                  "
                ),
                tags$p("In this process, the variable oedema is optional.")
              )
            ),

            #### Run Spatial Scan ----
            tags$div(
              id = "sec4",
              style = "text-align: justify;",
              tags$hr(),
              tags$p(tags$b("Spatial Scan")),
              tags$p(
                "
                In this tab, begin by specifying the scope of your analysis: either
                single-area or multiple-area. Single-area analysis applies when your
                dataset contains only one area (e.g., district, county), and the
                scan should be run within that area. Multiple-area analysis applies
                when your dataset includes several areas, and the scan should be
                run across them. If the former scope is chosen, enter the
                name of the area under review in the corresponding input field.
                Otherwise, specify the variable in your
                dataset that contains the area names. Once complete, fill in the
                remaining fields as appropriate, then click 'Run Scan' to initiate
                the process.
               "
              ),
              tags$p(
                "
                Once the scan is complete, several files will be saved in the
                directory you specified earlier. A list of these files will appear
                under the 'Created Files' section. Additionally, the 'Results
                of Detected Clusters' section will display a table showing the
                clusters detected in each analysis area. You can download the
                output table by clicking the download button, which becomes available
                once the scan has finished.
                "
              )
            )
          ),

          #### Authorship ----
          tags$div(
            id = "sec5",
            style = "text-align: justify;",
            tags$hr(),
            tags$p(tags$b("Authorship")),
            tags$p("This app was developed and is maintained by Tomás Zaba.")
          ),
          tags$div(
            id = "sec6",
            style = "text-align: justify;",
            tags$hr(),
            tags$p(tags$b("License")),
            tags$p("This app is licensed under the GPL (>=3) license.")
          ),

          #### Useful Resources ----
          tags$div(
            id = "sec6",
            style = "text-align: justify;",
            tags$hr(),
            tags$p(tags$b("Useful Resources")),
            tags$p("Read more about:"),
            tags$ul(
              tags$li(
                tags$code("mwana"),
                tags$a(href = "https://nutriverse.io/mwana/dev/", "click here")
              ),
              tags$li(
                tags$code("wowi"),
                tags$a(href = "https://nutspatial.github.io/wowi/", "click here")
              ),
              tags$li(
                tags$code("SaTScan"),
                tags$a(href = "https://www.satscan.org", "click here")
              )
            )
          )
        )
      )
    ),

    ## ---- Tab 2: Data Uploading ----------------------------------------------
    nav_panel(
      title = "Data Upload",
      wowi:::module_ui_upload(id = "upload_data")
    ),

    ## ---- Tab 3: Data Wrangling ------------------------------------------------

    nav_panel(
      title = "Data Wrangling",
      wowi:::module_ui_wrangle_data(id = "wrangle")
    ),

    ## ---- Tab 4: Spatial Scan ----------------------------------------------
    nav_panel(
      title = "Spatial Scan",
      wowi:::module_ui_run_spatial_scan(id = "scan")
    )
  )
)
