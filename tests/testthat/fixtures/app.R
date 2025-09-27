# Source wowi app's UI and Server files ----
# Source UI and Server files properly

ui_path <- here::here("inst", "app", "ui.R")
server_path <- here::here("inst", "app", "server.R")

source(file = ui_path, local = TRUE)
source(file = server_path, local = TRUE)

# Now create the shinyApp with the sourced objects
shiny::shinyApp(ui = ui, server = server)
