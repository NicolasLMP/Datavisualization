library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(plotly)


# UI function
mod_emissions_by_sectors_rel_stacked_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

mod_emissions_by_sectors_rel_stacked_server <- function(id, sectors, countries) {
  moduleServer(id, function(input, output, session) {
    
    # Load and prepare data
    data <- read_excel("../data/EDGAR_2024_GHG_booklet_2024.xlsx",
                       sheet = "GHG_by_sector_and_country",
                       guess_max = 10000)
    
    long_data <- data %>%
      pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value") %>%
      mutate(year = as.integer(year))
    
    sector_gas_data <- long_data %>%
      group_by(year, Sector, Substance, Country) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    wide_sector_gas <- sector_gas_data %>%
      pivot_wider(names_from = Substance, values_from = total) %>%
      mutate(CO2e = rowSums(select(., CO2, `GWP_100_AR5_CH4`, `GWP_100_AR5_N2O`, `GWP_100_AR5_F-gases`), na.rm = TRUE)) %>%
      rename(CH4 = `GWP_100_AR5_CH4`,
             N2O = `GWP_100_AR5_N2O`,
             F_gases = `GWP_100_AR5_F-gases`)
    
    # Compute relative values (% of total per year)
    wide_sector_gas <- wide_sector_gas %>%
      group_by(year, Country) %>%
      mutate(year_total = sum(CO2e, na.rm = TRUE),
             relative = (CO2e / year_total) * 100) %>%
      ungroup()
    
    output$plot <- renderPlotly({
      req(sectors())
      req(countries())
      
      selected_data <- wide_sector_gas %>%
        filter(Sector %in% sectors() & Country %in% countries())
      
      hover_text <- apply(selected_data, 1, function(row) {
        paste0(
          "<b>", row["Sector"], "</b><br>",
          "<b>", row["year"], "</b><br>",
          "Share: ", round(as.numeric(row["relative"]), 2), "%<br>",
          "CO₂e: ", round(as.numeric(row["CO2e"]), 2), " Mt"
        )
      })
      
      sector_colors <- c(
        "Agriculture"           = "#4DC3B3",  # teal-green
        "Buildings"             = "#F28E5C",  # soft orange
        "Fuel Exploitation"     = "#6574B9",  # blue-purple
        "Industrial Combustion" = "#D970C4",  # pink-magenta
        "Power Industry"        = "#A7CE47",  # yellow-green
        "Processes"             = "#F1D54A",  # yellow
        "Transport"             = "#E9BE86",  # beige/light orange
        "Waste"                 = "#B5B5B5"   # grey
      )
      
      sector_colors <- sector_colors[names(sector_colors) %in% unique(selected_data$Sector)]
      
      
      plot_ly(selected_data, x = ~year, y = ~relative, color = ~Sector, colors = sector_colors,
              type = 'scatter', mode = 'lines',  # lines only
              stackgroup = 'one',  # still stacks values
              text = hover_text, hoverinfo = 'text') %>%
        layout(title = "Global GHG Emissions by Sector (relative (%), stacked)",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Share of Total Emissions (%)", range = c(0, 100)))
    })
    
  })
}



