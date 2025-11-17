# modules/emissions.R
library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)

# UI function
mod_emissions_by_sectors_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("sectors"), "Select sectors:",
                       choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                                   "Industrial Combustion", "Power Industry",
                                   "Processes", "Transport", "Waste"),
                       selected = c("Agriculture", "Buildings")),
    dygraphOutput(ns("plot"))
  )
}

# Server function
mod_emissions_by_sectors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load and prepare data
    data <- read_excel("../data/EDGAR_2024_GHG_booklet_2024.xlsx",
                       sheet = "GHG_by_sector_and_country",
                       guess_max = 10000)
    
    global_data <- subset(data, Country == "GLOBAL TOTAL" & Substance == "CO2")
    
    long_data <- global_data %>%
      pivot_longer(cols = matches("^[0-9]{4}$"),
                   names_to = "year", values_to = "value") %>%
      mutate(year = as.integer(year)) %>%
      select(sector = Sector, year, value)
    
    wide_data <- long_data %>%
      pivot_wider(names_from = sector, values_from = value)
    
    wide_data$year <- as.Date(paste0(wide_data$year, "-01-01"))
    ts_data <- xts(wide_data[-1], order.by = wide_data$year)
    
    # Render dygraph
    output$plot <- renderDygraph({
      req(input$sectors)
      dygraph(ts_data[, input$sectors]) %>%
        dyRangeSelector() %>%
        dyOptions(stackedGraph = FALSE)
    })
  })
}