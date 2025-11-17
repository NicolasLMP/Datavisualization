library(shiny)

source("modules/emissions_by_sector_abs.R")

ui <- fluidPage(
  titlePanel("CO₂ Emissions Dashboard"),
  mod_emissions_ui("emissions_by_sector_rel_stacked")
)

server <- function(input, output, session) {
  mod_emissions_server("emissions_by_sector_rel_stacked")
}

shinyApp(ui, server)

