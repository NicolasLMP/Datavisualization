library(shiny)

# Load diagram modules
source("diagrams/emissions_by_region.R")
source("diagrams/global_heatmap.R")
source("diagrams/top_companies.R")

# Load page modules
source("pages/page_regions.R")
source("pages/page_heatmap.R")
source("pages/page_companies.R")

ui <- navbarPage(
  title = "Greenhouse Gas Emissions",
  tabPanel(
    "Emissions by Region",
    fluidPage(
      mod_page_regions_ui("page_regions")
    )
  ),
  tabPanel(
    "Global Heatmap",
    fluidPage(
      mod_page_heatmap_ui("page_heatmap")
    )
  ),
  tabPanel(
    "Top Companies",
    fluidPage(
      mod_page_companies_ui("page_companies")
    )
  ),

  # Info tab
  tabPanel(
    "About",
    fluidPage(
      h3("About this dashboard"),
      p("This dashboard visualizes global CO₂ emissions by sector and region using EDGAR data.")
    )
  )
)

server <- function(input, output, session) {
  # Call page modules
  mod_page_regions_server("page_regions")
  mod_page_heatmap_server("page_heatmap")
  mod_page_companies_server("page_companies")
}

shinyApp(ui, server)
