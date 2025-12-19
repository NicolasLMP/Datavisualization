mod_global_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "margin-bottom: 10px;",
      tags$h4("How are emissions distributed globally?", style = "font-weight: bold; color: #2c3e50;"),
      uiOutput(ns("map_explanation"))
    ),
    leafletOutput(ns("choroplethMap"), height = "500px")
  )
}


# Get world geometry


# Prepare data based on metric


# Add ISO matching


# Aggregation (in case of duplicates)


# Join with world data


# Color palette


# Create map with Cleaner basemap


# Add Legend
