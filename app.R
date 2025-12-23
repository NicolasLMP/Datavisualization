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

# --- UNIFIED COLOR PALETTE ---
# Define these globally so all sourced modules can access them if needed
options(ghg_pal = list(
  teal   = "#4DC3B3", # Agriculture / Americas
  orange = "#F28E5C", # Buildings / Asia
  slate  = "#6574B9", # Fuel / Europe / Companies
  lime   = "#A7CE47", # Power / Oceania
  tan    = "#E9BE86", # Transport / Africa
  grey   = "#B5B5B5", # Waste / World Total
  navy   = "#1D3557"  # Titles / Text
))

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

ui <- navbarPage(
  title = tags$span(
    icon("leaf"),
    "GHG Emissions Dashboard"
  ),
  # Harmonized bslib theme
  theme = bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#1D3557",
    primary = "#6574B9",   # Changed to Slate Blue to match "Fuel/Europe/Companies"
    secondary = "#4DC3B3", # Changed to Teal to match "Agriculture/Americas"
    success = "#A7CE47",   # Changed to Lime to match "Power/Oceania"
    danger = "#F28E5C",    # Changed to Soft Orange (replaces harsh red)
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  
  # Global CSS to clean up UI
  header = tags$head(
    tags$style(HTML("
      /* 1. Navbar bottom border */
      .navbar { border-bottom: 1px solid #EBEBEB !important; }
      
      /* 2. Style for ALL nav links */
      .nav-link { 
        font-weight: 500 !important; 
        color: #1D3557 !important; /* Default dark navy text */
      }
      
      /* 3. Style for the SELECTED (Active) tab */
      .navbar-nav .nav-link.active {
        color: #0072B2 !important; /* Your Theme Blue */
        border-bottom: 3px solid #0072B2 !important; /* Bottom underline */
        background-color: transparent !important;
      }

      /* 4. Hover effect */
      .nav-link:hover {
        color: #0072B2 !important;
        opacity: 0.8;
      }

      .container-fluid { padding-top: 20px; }
    "))
  ),
  
  tabPanel(
    "Home",
    icon = icon("home"),
    fluidPage(mod_page_home_ui("page_home"))
  ),
  tabPanel(
    "Global",
    icon = icon("globe-americas"),
    fluidPage(mod_page_regions_ui("page_regions"))
  ),
  tabPanel(
    "Map",
    icon = icon("map-marked-alt"),
    fluidPage(mod_page_heatmap_ui("page_heatmap"))
  ),
  tabPanel(
    "Sectors",
    icon = icon("industry"),
    fluidPage(mod_page_sectors_ui("page_sectors"))
  ),
  tabPanel(
    "Companies",
    icon = icon("building"),
    fluidPage(mod_page_companies_ui("page_companies"))
  ),
  tabPanel(
    "About",
    icon = icon("info-circle"),
    fluidPage(mod_page_about_ui("page_about"))
  ),
  # Footer
  tags$div(
    tags$p(
      style = "margin-top: 60px; padding: 30px; background-color: #f8f9fa;
                     text-align: center;",
      "Data sources: ",
      tags$a("EDGAR", href = "https://edgar.jrc.ec.europa.eu/report_2024", target = "_blank", style = "color: #457B9D; text-decoration: underline;"),
      "and ",
      tags$a("Carbon Majors Database", href = "https://carbonmajors.org/Downloads", target = "_blank", style = "color: #457B9D; text-decoration: underline;")
    )
  )
)

server <- function(input, output, session) {
  # Call page modules
  mod_page_home_server("page_home")
  mod_page_regions_server("page_regions")
  mod_page_heatmap_server("page_heatmap")
  mod_page_sectors_server("page_sectors")
  mod_page_companies_server("page_companies")
  mod_page_about_server("page_about")
}

shinyApp(ui, server)