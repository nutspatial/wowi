# Source wowi app's UI and Server files ----
# Source UI and Server files properly

ui_path <- system.file("app", "ui.R", package = "wowi")
server_path <- system.file("app", "server.R", package = "wowi")

source(file = ui_path, local = TRUE)
source(file = server_path, local = TRUE)

# Now create the shinyApp with the sourced objects
shiny::shinyApp(ui = ui, server = server)
