## ---- Module: UI -------------------------------------------------------------

#'
#'
#' Module UI for data upload
#'
#'
#' @param id Module ID
#'
#' @keywords internal
#'
#'
module_ui_wrangle_data <- function(id) {

  ## Namespace ID to generate unique ID every the module is called ----
  ns <- shiny::NS(id)

  ## US elements ----
  shiny::tagList(
        ### Left side of the nav panel ----
        bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 400,
          style = "width: 350px",
          bslib::card(
            style = "background-color: #fbfdfd;",
            bslib::card_header(htmltools::tags$span("Define Parameters for Data Wrangling",
            style = "font-size: 15px; font-weight: bold;")),
            shiny::radioButtons(
              inputId = ns("wrangle"),
              label = htmltools::tags$span("Case Definition based on",
            style = "font-size: 14px; font-weight: bold;"
            ),
              choices = list(
                "Weight-for-Height z-score" = "wfhz",
                "Mid-Upper Arm Circumference" = "muac",
                "Combined Case Definition" = "combined"
              ), selected = "wfhz"
            ),
            shiny::uiOutput(outputId = ns("variableSelectors")),
            htmltools::tags$br(),
            shiny::actionButton(
              inputId = ns("apply_wrangle"),
              label = "Wrangle",
              class = "btn-primary"
            )
          )
        ),

        ### Right side of the nav bar ----
        bslib::card(
          style = "background-color: #fbfdfd;",
          bslib::card_header(htmltools::tags$span("Data Preview",
        style = "font-size: 15px; font-weight: bold;")
      ),
          shinycssloaders::withSpinner(
            ui_element = DT::DTOutput(outputId = ns("wrangled_data")),
            type = 8,
            color.background = "#9dac7c",
            image = "logo.png",
            image.height = "70px",
            color = "#9dac7c",
            caption = htmltools::tags$div(
              htmltools::tags$h6(htmltools::tags$span("Wrangling", style = "font-size: 12px;")), 
              htmltools::tags$h6(htmltools::tags$span("Please wait...", style = "font-size: 12px;"))
            )
          )
        )
      )
  )
}



## ---- Module: Server ---------------------------------------------------------

#'
#'
#'
#' Module server for data upload
#'
#'
#' @param id Module ID
#'
#' @keywords internal
#'
#'
module_server_wrangle_data <- function(id, data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      ### Container for reactivy ----
      df <- shiny::reactiveValues(wrangled = NULL)
      
    ### List of variables to be displayed to user based on wrangling method ----
  output$variableSelectors <- shiny::renderUI({
    shiny::req(data())
    cols <- base::names(data())

    switch(input$wrangle,

      #### Weight-for-heigh ----
      "wfhz" = shiny::tagList(
        shiny::selectInput(
          inputId = ns("sex"),
          label = shiny::tagList(htmltools::tags$span("Sex", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("weight"),
          label = shiny::tagList(
            htmltools::tags$span("Weight (kg)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;"),
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("height"),
          label = shiny::tagList(htmltools::tags$span("Height (cm)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("oedema"),
          label = htmltools::tags$span("Oedema", 
          style = "font-size: 14px; font-weight: bold;"),
          choices = c("", cols)
        )
      ),

      #### Mid-Upper Arm Circumference (wrangling based on MUAC-for-age ) ----
      "muac" = shiny::tagList(
        shiny::selectInput(
          inputId = ns("age"),
         label = shiny::tagList(
            htmltools::tags$span("Age (months)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;"),
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("sex"),
          label = shiny::tagList(htmltools::tags$span("Sex", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("muac"),
          label = shiny::tagList(htmltools::tags$span("MUAC (mm)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("oedema"),
          label = htmltools::tags$span("Oedema", 
          style = "font-size: 14px; font-weight: bold;"),
          choices = c("", cols)
        )
      ),

      #### Both weight-for-height and MUAC ----
      "combined" = shiny::tagList(
        shiny::selectInput(
          inputId = ns("sex"),
          label = shiny::tagList(htmltools::tags$span("Sex", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("age"),
         label = shiny::tagList(
            htmltools::tags$span("Age (months)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;"),
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("weight"),
          label = shiny::tagList(
            htmltools::tags$span("Weight (kg)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;"),
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("height"),
          label = shiny::tagList(htmltools::tags$span("Height (cm)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("muac"),
          label = shiny::tagList(htmltools::tags$span("MUAC (mm)", 
          style = "font-size: 14px; font-weight: bold;"), htmltools::tags$span("*",
        style = "color: red;")
          ),
          choices = c("", cols)
        ),
        shiny::selectInput(
          inputId = ns("oedema"),
          label = htmltools::tags$span("Oedema", 
          style = "font-size: 14px; font-weight: bold;"),
          choices = c("", cols)
        )
      )
    )
  })

  df$wrangling <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$apply_wrangle, {
    #### Ensure input data from Tab 1 exists before rendering ----
    shiny::req(data())
    df$wrangling(TRUE)

    #### Handle errors gracefully ----
    valid <- TRUE
    msg <- ""

    if (input$wrangle == "wfhz") {
      if (input$sex == "" || input$weight == "" || input$height == "") {
        valid <- FALSE
        msg <- "Please select all required variables (Sex, Weight, Height) and try again."
      }
    } else if (input$wrangle == "muac") {
      if (input$sex == "" || input$muac == "" || input$age == "") {
        valid <- FALSE
        msg <- "Please select all required variables (Sex, MUAC, Age) and try again."
      }
    } else if (input$wrangle == "combined") {
      if (
        any(c(
          input$age, input$sex, input$muac, input$weight,
          input$height
        ) == "")) {
        valid <- FALSE
        msg <-
          "Please select all required variables (Age, Sex, Weight, Height, MUAC)."
      }
    }

    if (!valid) {
      shiny::showNotification(msg, type = "error")
      return()
    }

    #### Apply data wrangling and manage errors gracefully ----
    tryCatch(
      {
        result <- switch(input$wrangle,
          "wfhz" = {
            shiny::req(input$sex, input$weight, input$height)

            data() |>
              dplyr::rename(
                sex = !!rlang::sym(input$sex),
                weight = !!rlang::sym(input$weight),
                height = !!rlang::sym(input$height)
              ) |>
              mwana::mw_wrangle_wfhz(
                sex = .data$sex,
                .recode_sex = FALSE,
                weight = .data$weight,
                height = .data$height
              ) |>
              mwana::define_wasting(
                zscores = .data$wfhz,
                .by = "zscores",
                edema = if (input$oedema != "") !!rlang::sym(input$oedema) else NULL
              )
          },
          "muac" = {
            shiny::req(input$age, input$muac, input$sex)

            # Create a working dataset with standardized column names
            data() |>
                  dplyr::mutate(
                muac = as.numeric(.data$muac),
                    muac = mwana::recode_muac(x = !!rlang::sym(input$muac), .to = "cm")
                  ) |> 
              dplyr::rename(
                age = !!rlang::sym(input$age),
                sex = !!rlang::sym(input$sex)
              ) |>
              mwana::mw_wrangle_age(age = .data$age) |>
              mwana::mw_wrangle_muac(
                sex = .data$sex,
                .recode_sex = FALSE,
                muac = .data$muac,
                .recode_muac = FALSE,
                .to = "none",
                age = .data$age
              ) |>
              dplyr::mutate(muac = mwana::recode_muac(.data$muac, .to = "mm")) |>
              mwana::define_wasting(
                muac = .data$muac,
                .by = "muac",
                edema = if (input$oedema != "") !!rlang::sym(input$oedema) else NULL
              )
          },
          "combined" = {
            shiny::req(
              input$sex, input$weight, input$height,
              input$age, input$muac
            )

            data() |>
                  dplyr::mutate(
                muac = base::as.numeric(.data$muac),
                    muac = recode_muac(!!rlang::sym(input$muac), "cm")
                  ) |> 
              dplyr::rename(
                sex = !!rlang::sym(input$sex),
                weight = !!rlang::sym(input$weight),
                height = !!rlang::sym(input$height),
                age = !!rlang::sym(input$age)
              ) |>
              mwana::mw_wrangle_wfhz(
                sex = .data$sex,
                .recode_sex = FALSE,
                weight = .data$weight,
                height = .data$height
              ) |>
              mwana::mw_wrangle_age(age = .data$age) |>
              mwana::mw_wrangle_muac(
                sex = .data$sex,
                .recode_sex = FALSE,
                muac = .data$muac,
                .recode_muac = FALSE,
                .to = "none",
                age = .data$age
              ) |>
              dplyr::mutate(muac = mwana::recode_muac(.data$muac, .to = "mm")) |>
              mwana::define_wasting(
                zscores = .data$wfhz,
                muac = .data$muac,
                .by = "combined",
                edema = if (input$oedema != "") !!rlang::sym(input$oedema) else NULL
              )
          }
        )

        df$wrangled <- result
      },
      error = function(e) {
        shiny::showNotification(
          base::paste("Wrangling error:", e$message), type = "error")
      }
    )
    df$wrangling(FALSE)
  })

  #### Display wrangled data ----
  output$wrangled_data <- DT::renderDT({
    shiny::req(!df$wrangling()) # Let waiting spinner run until wrangling is done
    shiny::req(df$wrangled)
    DT::datatable(
      data = utils::head(df$wrangled, 20),
      rownames = FALSE,
      options = base::list(
        pageLength = 20,
        scrollX = FALSE,
        scrolly = "800px",
        columnDefs = base::list(base::list(className = "dt-center", targets = "_all"))
      ),
      caption = if (base::nrow(df$wrangled) > 20) {
        base::paste(
          "Showing first 20 rows of", base::format(base::nrow(df$wrangled), big.mark = ","),
          "total rows"
        )
      } else {
        base::paste("showing all", base::nrow(df$wrangled), "rows")
      }
    ) |> DT::formatStyle(columns = base::colnames(df$wrangled), fontSize = "13px")
  })
      
      #### Return data ----
      return(shiny::reactive(df$wrangled))

    }
    
  )
}