# app.R

# Source global settings and reusable components
source("global.R")

# Load the user interface and server logic
ui <- source("ui.R")$value
server <- source("server.R")$value

# Launch the Shiny app
shinyApp(ui = ui, server = server)