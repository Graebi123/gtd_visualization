library(shiny)

# Source UI and SERVER after defining `years`
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)

