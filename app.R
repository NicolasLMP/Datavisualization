library(shiny)
source("modules/emissions_by_sector_abs.R")
source("modules/emissions_by_sector_rel.R")
source("modules/emissions_by_sector_rel_stacked.R")


ui <- navbarPage(
  title = "Greenhouse Gas Emissions",
  
  # Tab for Emissions
  tabPanel("Emissions by sectors",
           fluidPage(
             mod_emissions_by_sectors_ui("emissions_by_sector_abs")
           )
  ),
  tabPanel("Emissions by sectors relative",
           fluidPage(
             mod_emissions_by_sectors_ui("emissions_by_sector_rel")
           )
  ),
  
  tabPanel("Emissions by sectors relative stacked",
           fluidPage(
             mod_emissions_by_sectors_ui("emissions_by_sector_rel_stacked")
           )
  ),
  
  # Placeholder for other diagrams
  tabPanel("Other Charts",
           fluidPage(
             h3("Coming soon: Additional visualizations")
           )
  ),
  
  # About tab
  tabPanel("About",
           fluidPage(
             h3("About this dashboard"),
             p("This dashboard visualizes global CO₂ emissions by sector over time using EDGAR data.")
           )
  )
)

server <- function(input, output, session) {
  mod_emissions_by_sectors_server("emissions_by_sector_abs")
  mod_emissions_by_sectors_rel_server("emissions_by_sector_rel")
  mod_emissions_by_sectors_rel_stacked_server("emissions_by_sector_rel_stacked")
}

shinyApp(ui, server)