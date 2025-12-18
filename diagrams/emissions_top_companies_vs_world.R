# modules/company_vs_world.R
library(plotly)
library(dplyr)
library(readr)
library(tidyr) 
library(purrr)

# Load and clean data
.company_emissions <- read_csv("data/data_cleaned/GHG_by_sector_and_companies.csv")
colnames(.company_emissions) <- gsub(" ", "_", colnames(.company_emissions))

mod_company_vs_world_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("companyWorldChart"), height = "500px")
  )
}

mod_company_vs_world_server <- function(id, race_year) {
  moduleServer(id, function(input, output, session) {
    
    output$companyWorldChart <- renderPlotly({
      req(race_year()) 
      selected_year <- race_year()
      
      # Robust column detection
      world_col_name <- if("World_Total" %in% colnames(.company_emissions)) {
        "World_Total"
      } else if("world" %in% colnames(.company_emissions)) {
        "world"
      } else {
        grep("world", colnames(.company_emissions), ignore.case = TRUE, value = TRUE)[1]
      }
      
      # Process data and calculate percentage share
      time_series_data <- .company_emissions %>%
        group_by(year) %>%
        summarise(
          comp_val = sum(total_emissions_MtCO2e, na.rm = TRUE),
          world_val = first(!!sym(world_col_name))
        ) %>%
        mutate(
          share_pct = (comp_val / world_val) * 100
        ) %>%
        pivot_longer(
          cols = c(comp_val, world_val),
          names_to = "Category", 
          values_to = "Emissions"
        ) %>%
        mutate(
          Category = ifelse(Category == "comp_val", "Top Emitting Companies", "World Total")
        )
      
      # Create custom hover text - ONLY add share info for Companies
      df <- time_series_data %>%
        mutate(hover_text = ifelse(
          Category == "Top Emitting Companies",
          paste0(
            "<b>", Category, "</b><br>",
            "Year: ", year, "<br>",
            "Emissions: ", round(Emissions, 2), " MtCO2e<br>",
            "<b>Share of World: ", round(share_pct, 1), "%</b>"
          ),
          paste0(
            "<b>", Category, "</b><br>",
            "Year: ", year, "<br>",
            "Emissions: ", round(Emissions, 2), " MtCO2e"
          )
        ))
      
      # Define colors
      cat_colors <- c("Top Emitting Companies" = "#6574B9", "World Total" = "#B5B5B5")
      
      plot_ly(
        df, x = ~year, y = ~Emissions, color = ~Category, colors = cat_colors,
        type = 'scatter', mode = 'lines+markers', text = ~hover_text,
        hoverinfo = 'text'
      ) %>%
        config(modeBarButtonsToRemove = c("select2d", "lasso2d")) %>%
        layout(
          title = "GHG Emission Trends: Companies vs. World",
          xaxis = list(title = "Year", fixedrange = TRUE),
          yaxis = list(title = "Emissions (Mt CO₂e)", rangemode = "tozero", fixedrange = TRUE),
          # shapes = list(
          #   list(
          #     type = "line",
          #     x0 = selected_year, x1 = selected_year,
          #     y0 = 0, y1 = 1, yref = "paper",
          #     line = list(color = "red", width = 1, dash = "dot")
          #   )
          # ),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
          dragmode = FALSE
        )
    })
  })
}