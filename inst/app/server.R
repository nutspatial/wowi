# ==============================================================================
#                               SERVER LOGIC
# ==============================================================================

## ---- Server's definitions ---------------------------------------------------

server <- function(input, output, session) {

  ### Data upload; returns a reactive data ----
  data <- wowi:::module_server_upload(id = "upload_data")

  ### Data Wrangling ----
  wrangled <- wowi:::module_server_wrangle_data(id = "wrangle", data = data)

  ### Spatial Scan ----
  wowi:::module_server_run_spatial_scan(id = "scan", .data = wrangled)

}
