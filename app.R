library(shiny)
library(bslib)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(rnaturalearth)
library(sf)
library(plotly)
library(dygraphs)
library(xts)
library(purrr)
library(readxl)

# Load diagram modules
source("diagrams/emissions_by_region.R")
source("diagrams/global_heatmap.R")
source("diagrams/top_companies.R")
source("diagrams/emissions_by_sector_abs.R")
source("diagrams/emissions_by_sector_rel.R")
source("diagrams/emissions_by_sector_rel_stacked.R")
source("diagrams/emissions_top_companies_vs_world.R")


# Load page modules
source("pages/page_home.R")
source("pages/page_regions.R")
source("pages/page_heatmap.R")
source("pages/page_sectors.R")
source("pages/page_companies.R")
source("pages/page_about.R")
source("pages/page_download.R")

# Define UI
ui <- page_fluid(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#1D3557",
    primary = "#457B9D",
    secondary = "#2A9D8F",
    success = "#2A9D8F",
    danger = "#E63946",
    base_font = bslib::font_google("Inter")
  ),
  tags$head(
    tags$style(HTML("
      /* Custom styles to ensure full height and better spacing */
      html, body { height: 100%; }
      .navbar { margin-bottom: 20px; }
      .card { box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
    "))
  ),
  navbarPage(
    title = div(
      icon("leaf", class = "fa-lg", style = "color: #2c3e50; margin-right: 10px;"),
      span("GHG Emissions Dashboard", style = "font-weight: bold; color: #2c3e50;")
    ),
    id = "navbar",
    collapsible = TRUE,

    # Home Page
    tabPanel("Home",
      icon = icon("home"),
      mod_page_home_ui("home")
    ),

    # Global Emissions
    tabPanel("Global",
      icon = icon("globe"),
      mod_page_regions_ui("regions")
    ),

    # Map
    tabPanel("Map",
      icon = icon("map-marked-alt"),
      mod_page_heatmap_ui("heatmap")
    ),

    # Sectors
    tabPanel("Sectors",
      icon = icon("industry"),
      mod_page_sectors_ui("sectors")
    ),

    # Companies
    tabPanel("Companies",
      icon = icon("building"),
      mod_page_companies_ui("companies")
    ),

    # Report / Download
    tabPanel("Report",
      icon = icon("file-download"),
      mod_page_download_ui("download")
    ),

    # About
    tabPanel("About",
      icon = icon("info-circle"),
      mod_page_about_ui("about")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Call modules
  mod_page_home_server("home")
  mod_page_regions_server("regions")
  mod_page_heatmap_server("heatmap")
  mod_page_sectors_server("sectors")
  mod_page_companies_server("companies")
  mod_page_download_server("download")
  mod_page_about_server("about")
}
shinyApp(ui, server)
