library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(plotly)


# UI function
mod_emissions_by_sectors_abs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

# Server function
mod_emissions_by_sectors_abs_server <- function(id, sectors) {
  moduleServer(id, function(input, output, session) {
    
    # Load and prepare data
    data <- read_excel("../data/EDGAR_2024_GHG_booklet_2024.xlsx",
                       sheet = "GHG_by_sector_and_country",
                       guess_max = 10000)
    
    # Filter global data
    global_data <- subset(data, Country == "GLOBAL TOTAL")
    
    # Pivot longer
    long_data <- global_data %>%
      pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value") %>%
      mutate(year = as.integer(year))
    
    # Aggregate by year, sector, and substance
    sector_gas_data <- long_data %>%
      group_by(year, Sector, Substance) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    # Pivot wider: each gas becomes a column
    wide_sector_gas <- sector_gas_data %>%
      pivot_wider(names_from = Substance, values_from = total)
    
    wide_sector_gas <- wide_sector_gas %>%
      mutate(CO2e = rowSums(select(., CO2, `GWP_100_AR5_CH4`, `GWP_100_AR5_N2O`, `GWP_100_AR5_F-gases`), na.rm = TRUE)) %>%
      rename(CH4 = `GWP_100_AR5_CH4`,
             N2O = `GWP_100_AR5_N2O`,
             F_gases = `GWP_100_AR5_F-gases`)
    
    # Render Plotly chart
    output$plot <- renderPlotly({
      req(sectors())
      
      selected_data <- wide_sector_gas %>%
        filter(Sector %in% sectors())
      
      hover_text <- apply(selected_data, 1, function(row) {
        total <- as.numeric(row["CO2e"])
        co2 <- as.numeric(row["CO2"])
        ch4 <- as.numeric(row["CH4"])
        n2o <- as.numeric(row["N2O"])
        fg <- as.numeric(row["F_gases"])
        
        paste0(
          "<b>", row["Sector"], "</b><br>",
          "<b>", row["year"], "</b><br>",
          "CO₂e: ", round(total, 2), " Mt<br>",
          "CO₂: ", round(co2/total*100, 1), "%<br>",
          "CH₄: ", round(ch4/total*100, 1), "%<br>",
          "N₂O: ", round(n2o/total*100, 1), "%<br>",
          if (!is.na(fg)) paste0("CO₂e from F-gases: ", round(fg, 2), " Mt (", round(fg/total*100, 1), "%)") else ""
        )
      })
      
      plot_ly(selected_data, x = ~year, y = ~CO2e, color = ~Sector,
              type = 'scatter', mode = 'lines+markers',
              text = hover_text, hoverinfo = 'text') %>%
        layout(title = "Global GHG Emissions by Sector (CO₂e)",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Emissions (Mt CO₂e)"))
      
    })
    
  })
}