library(shiny)

# Load diagram modules
source("diagrams/emissions_by_region.R")
source("diagrams/global_heatmap.R")
source("diagrams/top_companies.R")

# Load page modules
source("pages/page_regions.R")
source("pages/page_heatmap.R")
source("pages/page_companies.R")
source("pages/page_about.R")

ui <- navbarPage(
  title = "Greenhouse gas emissions",
  tabPanel(
    "Global",
    fluidPage(
      mod_page_regions_ui("page_regions")
    )
  ),
  tabPanel(
    "Map",
    fluidPage(
      mod_page_heatmap_ui("page_heatmap")
    )
  ),
  tabPanel(
    "Companies",
    fluidPage(
      mod_page_companies_ui("page_companies")
    )
  ),

  # Info tab
  tabPanel(
    "About",
    fluidPage(
      mod_page_about_ui("page_about")
    )
  )
)

server <- function(input, output, session) {
  # Call page modules
  mod_page_regions_server("page_regions")
  mod_page_heatmap_server("page_heatmap")
  mod_page_companies_server("page_companies")
  mod_page_about_server("page_about")
}

shinyApp(ui, server)
