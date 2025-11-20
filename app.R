library(shiny)

# Load modules
source("Diagrams/emissions_by_sector_abs.R")
source("Diagrams/emissions_by_region.R")
source("Diagrams/global_heatmap.R")
source("Diagrams/top_companies.R")

ui <- navbarPage(
  title = "Greenhouse Gas Emissions",
  
  tabPanel("Emissions by sectors",
           fluidPage(
             mod_emissions_by_sectors_ui("emissions_by_sector_abs")
           )
  ),
  
  tabPanel("Emissions by Region",
           fluidPage(
             mod_emissions_by_region_ui("emissions_by_region")
           )
  ),
  
  tabPanel("Global Heatmap",
           fluidPage(
             mod_global_heatmap_ui("global_heatmap")
           )
  ),
  
  tabPanel("Top Companies",
           fluidPage(
             mod_top_companies_ui("top_companies")
           )
  ),
  
  # Info tab
  tabPanel("About",
           fluidPage(
             h3("About this dashboard"),
             p("This dashboard visualizes global CO₂ emissions by sector and region using EDGAR data.")
           )
  )
)

server <- function(input, output, session) {
  # Module calls
  mod_emissions_by_sectors_server("emissions_by_sector_abs")
  mod_emissions_by_region_server("emissions_by_region")
  mod_global_heatmap_server("global_heatmap")
  mod_top_companies_server("top_companies")
}

shinyApp(ui, server)
