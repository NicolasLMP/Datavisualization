library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(plotly)

data <- read.csv("data/data_cleaned/GHG_by_sector_and_country.csv", stringsAsFactors = FALSE)

# UI function
mod_emissions_by_sectors_rel_stacked_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

mod_emissions_by_sectors_rel_stacked_server <- function(id, sectors, countries) {
  moduleServer(id, function(input, output, session) {
    
    
    output$plot <- renderPlotly({
      req(sectors())
      req(countries())
      
      selected_data <- data %>%
        filter(Sector %in% sectors() & Country %in% countries())
      
      hover_text <- apply(selected_data, 1, function(row) {
        paste0(
          "<b>", row["Sector"], "</b><br>",
          "<b>", row["year"], "</b><br>",
          "Share: ", round(as.numeric(row["relative"]), 2), "%<br>",
          "CO₂e: ", round(as.numeric(row["CO2e"]), 2), " Mt"
        )
      })
      
      # sector_colors <- c(
      #   "Agriculture"           = "#4DC3B3",  # teal-green
      #   "Buildings"             = "#F28E5C",  # soft orange
      #   "Fuel Exploitation"     = "#6574B9",  # blue-purple
      #   "Industrial Combustion" = "#D970C4",  # pink-magenta
      #   "Power Industry"        = "#A7CE47",  # yellow-green
      #   "Processes"             = "#F1D54A",  # yellow
      #   "Transport"             = "#E9BE86",  # beige/light orange
      #   "Waste"                 = "#B5B5B5"   # grey
      # )
      # 
      # sector_colors <- sector_colors[names(sector_colors) %in% unique(selected_data$Sector)]
      palette_okabe <- color("okabe ito")(8)
      
      # Map the categories to the palette dynamically
      sector_names <- c(
        "Waste", "Agriculture", "Buildings", "Fuel Exploitation", 
        "Industrial Combustion", "Power Industry", "Processes", 
        "Transport"
      )
      sector_colors <- setNames(as.character(palette_okabe[1:length(sector_names)]), sector_names)
      
      
      plot_ly(selected_data, x = ~year, y = ~relative, color = ~Sector, colors = sector_colors,
              type = 'scatter', mode = 'lines',  # lines only
              stackgroup = 'one',  # still stacks values
              text = hover_text, hoverinfo = 'text') %>%
        layout(title = paste0("How does the share of ", paste(countries(), collapse = ", "), "'s sector emissions develop over time?"),
               xaxis = list(title = "Year", fixedrange = TRUE),
               yaxis = list(title = "Share of Total Emissions (%)", range = c(0, 100), fixedrange = TRUE))
    })
    
  })
}



