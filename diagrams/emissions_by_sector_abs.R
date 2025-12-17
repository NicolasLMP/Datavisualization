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
    fluidRow(
      # Column for the Main Plot (75% width: 9 out of 12)
      column(
        width = 8,
        plotlyOutput(ns("plot"), height = "400px")
      ),
      
      # Column for the Donut Chart (25% width: 3 out of 12)
      column(
        width = 4,
        # Note: The donut chart's title will be defined dynamically in the server
        plotlyOutput(ns("donut"), height = "400px")
      )
    )
  )
}

# Server function

mod_emissions_by_sectors_abs_server <- function(id, sectors, countries) {
  moduleServer(id, function(input, output, session) {
    
    last_hover_data <- reactiveVal(NULL)
    
    selected_data <- reactive({
      req(sectors())
      req(countries())
      data %>%
        filter(Sector %in% sectors(), Country %in% countries())
    })
    
    output$plot <- renderPlotly({
      df <- selected_data()
      req(nrow(df) > 0)
      
      hover_text <- pmap_chr(df, function(Sector, Country, year, CO2e, CO2, CH4, N2O, F_gases, ...) {
        total <- as.numeric(CO2e)
        co2 <- as.numeric(CO2)
        ch4 <- as.numeric(CH4)
        n2o <- as.numeric(N2O)
        fg <- as.numeric(F_gases)
        
        paste0(
          "<b>", Sector, "</b><br>",
          "<b>", year, "</b><br>",
          "CO₂e: ", round(total, 2), " Mt<br>"
        )
      })
      
      sector_colors <- c(
        "Agriculture" = "#4DC3B3", "Buildings" = "#F28E5C", "Fuel Exploitation" = "#6574B9",
        "Industrial Combustion" = "#D970C4", "Power Industry" = "#A7CE47", "Processes" = "#F1D54A",
        "Transport" = "#E9BE86", "Waste" = "#B5B5B5"
      )
      sector_colors <- sector_colors[names(sector_colors) %in% unique(df$Sector)]
      
      plot_ly(
        df, x = ~year, y = ~CO2e, color = ~Sector, colors = sector_colors,
        type = 'scatter', mode = 'lines+markers', text = hover_text,
        hoverinfo = 'text', source = session$ns("abs")
      ) %>%
        layout(
          title = paste0("What are the emission per sector in ", 
                         paste(unique(df$Country), 
                         collapse = ", "), "?"),
          xaxis = list(title = "Year"),
          yaxis = list(title = "Emissions (Mt CO₂e)", 
                       rangemode = "tozero",
                       zerolinecolor="#EBEBEB")
        )
    })
    
    output$donut <- renderPlotly({
      df <- selected_data()
      req(nrow(df) > 0)

      hover <- event_data("plotly_hover", source = session$ns("abs"))

      if (!is.null(hover)) {
        sector_levels <- unique(df$Sector)
        hovered_sector <- sector_levels[ hover$curveNumber + 1 ]

        row <- df %>% filter(year == hover$x, Sector == hovered_sector)

        if (nrow(row) == 1) {
          last_hover_data(list(
            row = row,
            sector = hovered_sector,
            year = hover$x
          ))
        }

      } else if (is.null(last_hover_data())) {
        latest_year <- max(df$year, na.rm = TRUE)
        row <- df %>%
          filter(year == latest_year) %>%
          arrange(Sector) %>%
          slice(1)

        if (nrow(row) == 0) return(NULL) # Safety check

        hovered_sector <- row$Sector
        hover_year <- latest_year

        last_hover_data(list(
          row = row,
          sector = hovered_sector,
          year = hover_year
        ))

      }
      current_data <- last_hover_data()
      req(current_data)

      row <- current_data$row
      hovered_sector <- current_data$sector
      hover_year <- current_data$year


      # Gas breakdown for donut
      df_gas <- data.frame(
        gas = c("CO2", "CH4", "N2O", "F-gases"),
        value = c(row$CO2, row$CH4, row$N2O, row$F_gases)
      ) %>% filter(!is.na(value) & value > 0)

      gas_colors <- c(
        "CO2" = "#2A9D8F",
        "CH4" = "#E9C46A",
        "N2O" = "#F4A261",
        "F-gases" = "#C34A36"
      )

      gas_colors <- gas_colors[df_gas$gas]

      plot_ly(
        df_gas,
        labels = ~gas,
        values = ~value,
        type = "pie",
        hole = 0.6,
        textinfo = "label+percent",
        hoverinfo = "none",
        marker = list(colors = gas_colors)
      ) %>%
        layout(
          title = paste0("Gas composition of ", 
                         hovered_sector, "<br>in ", hover_year),
          showlegend = TRUE
        )
    })
  })
}