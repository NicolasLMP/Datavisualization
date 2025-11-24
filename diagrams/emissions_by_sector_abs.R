library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(plotly)
library(purrr)

# Load data
data <- read.csv("data/data_cleaned/GHG_by_sector_and_country.csv", stringsAsFactors = FALSE)
# data$Continent <- countrycode(data$Country, origin = "country.name", destination = "continent")

# UI function
mod_emissions_by_sectors_abs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), height = "400px"),
    br(),
    h4("Gas Composition (Hover on a point)"),
    plotlyOutput(ns("donut"), height = "350px")
  )
}

# Server function
mod_emissions_by_sectors_abs_server <- function(id, sectors, countries) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive filtered data
    selected_data <- reactive({
      req(sectors())
      req(countries())
      data %>%
        filter(Sector %in% sectors(), Country %in% countries())
    })
    
    # Render main Plotly chart
    output$plot <- renderPlotly({
      df <- selected_data()
      req(nrow(df) > 0)
      
      # Prepare hover text safely
      hover_text <- pmap_chr(df, function(Sector, Country, year, CO2e, CO2, CH4, N2O, F_gases, ...) {
        total <- as.numeric(CO2e)
        co2 <- as.numeric(CO2)
        ch4 <- as.numeric(CH4)
        n2o <- as.numeric(N2O)
        fg <- as.numeric(F_gases)
        
        paste0(
          "<b>", Sector, "</b><br>",
          "<b>", year, "</b><br>",
          "CO₂e: ", round(total, 2), " Mt<br>",
          "CO₂: ", round(co2/total*100, 1), "%<br>",
          "CH₄: ", round(ch4/total*100, 1), "%<br>",
          "N₂O: ", round(n2o/total*100, 1), "%<br>",
          if (!is.na(fg)) paste0("CO₂e from F-gases: ", round(fg, 2), " Mt (", round(fg/total*100, 1), "%)") else ""
        )
      })
      
      # Define colors for sectors
      sector_colors <- c(
        "Agriculture"           = "#4DC3B3",
        "Buildings"             = "#F28E5C",
        "Fuel Exploitation"     = "#6574B9",
        "Industrial Combustion" = "#D970C4",
        "Power Industry"        = "#A7CE47",
        "Processes"             = "#F1D54A",
        "Transport"             = "#E9BE86",
        "Waste"                 = "#B5B5B5"
      )
      sector_colors <- sector_colors[names(sector_colors) %in% unique(df$Sector)]
      
      # Plotly scatter/line plot
      plot_ly(
        df,
        x = ~year,
        y = ~CO2e,
        color = ~Sector,
        colors = sector_colors,
        type = 'scatter',
        mode = 'lines+markers',
        text = hover_text,
        hoverinfo = 'text',
        source = session$ns("abs")
      ) %>%
        layout(
          title = paste("GHG Emissions by Sector for", paste(unique(df$Country), collapse = ", ")),
          xaxis = list(title = "Year"),
          yaxis = list(title = "Emissions (Mt CO₂e)")
        )
    })
    
    # Donut chart based on hover
    output$donut <- renderPlotly({
      hover <- event_data("plotly_hover", source = session$ns("abs"))
      req(hover)
      
      df <- selected_data()
      req(nrow(df) > 0)
      
      # Match sector from curveNumber
      sector_levels <- unique(df$Sector)
      hovered_sector <- sector_levels[ hover$curveNumber + 1 ]
      
      # Match row by year and sector
      row <- df %>% filter(year == hover$x, Sector == hovered_sector)
      req(nrow(row) == 1)
      
      # Gas breakdown for donut
      df_gas <- data.frame(
        gas = c("CO2", "CH4", "N2O", "F-gases"),
        value = c(row$CO2, row$CH4, row$N2O, row$F_gases)
      ) %>% filter(!is.na(value) & value > 0)
      
      # Define gas colors (color-blind friendly)
      gas_colors <- c(
        "CO2" = "#0072B2",
        "CH4" = "#009E73",
        "N2O" = "#D55E00",
        "F-gases" = "#CC79A7"
      )
      
      # Filter colors for only present gases
      gas_colors <- gas_colors[df_gas$gas]
      
      plot_ly(
        df_gas,
        labels = ~gas,
        values = ~value,
        type = "pie",
        hole = 0.6,
        textinfo = "label+percent",
        hoverinfo = "label+value",
        marker = list(colors = gas_colors)
      ) %>%
        layout(
          title = paste("Gas Breakdown –", hovered_sector, hover$x),
          showlegend = TRUE
        )
    })
    
    
  })
}
