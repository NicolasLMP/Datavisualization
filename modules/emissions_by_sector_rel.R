library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(plotly)


# UI function
mod_emissions_by_sectors_rel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("sectors"), "Select sectors:",
                       choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                                   "Industrial Combustion", "Power Industry",
                                   "Processes", "Transport", "Waste"),
                       selected = c("Industrial Combustion", "Power Industry")),
    plotlyOutput(ns("plot"))
  )
}

mod_emissions_by_sectors_rel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load and prepare data
    data <- read_excel("../data/EDGAR_2024_GHG_booklet_2024.xlsx",
                       sheet = "GHG_by_sector_and_country",
                       guess_max = 10000)
    
    global_data <- subset(data, Country == "GLOBAL TOTAL")
    
    long_data <- global_data %>%
      pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value") %>%
      mutate(year = as.integer(year))
    
    sector_gas_data <- long_data %>%
      group_by(year, Sector, Substance) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    wide_sector_gas <- sector_gas_data %>%
      pivot_wider(names_from = Substance, values_from = total) %>%
      mutate(CO2e = rowSums(select(., CO2, `GWP_100_AR5_CH4`, `GWP_100_AR5_N2O`, `GWP_100_AR5_F-gases`), na.rm = TRUE)) %>%
      rename(CH4 = `GWP_100_AR5_CH4`,
             N2O = `GWP_100_AR5_N2O`,
             F_gases = `GWP_100_AR5_F-gases`)
    
    # Compute relative values (% of total per year)
    wide_sector_gas <- wide_sector_gas %>%
      group_by(year) %>%
      mutate(year_total = sum(CO2e, na.rm = TRUE),
             relative = (CO2e / year_total) * 100) %>%
      ungroup()
    
    # Render Plotly chart
    output$plot <- renderPlotly({
      req(input$sectors)
      
      selected_data <- wide_sector_gas %>%
        filter(Sector %in% input$sectors)
      
      hover_text <- apply(selected_data, 1, function(row) {
        paste0(
          "<b>", row["Sector"], "</b><br>",
          "<b>", row["year"], "</b><br>",
          "Share: ", round(as.numeric(row["relative"]), 2), "%<br>",
          "CO₂e: ", round(as.numeric(row["CO2e"]), 2), " Mt"
        )
      })
      
      plot_ly(selected_data, x = ~year, y = ~relative, color = ~Sector,
              type = 'scatter', mode = 'lines+markers',
              text = hover_text, hoverinfo = 'text') %>%
        layout(title = "Global GHG Emissions by Sector (Relative %)",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Share of Total Emissions (%)"))
    })
    
  })
}