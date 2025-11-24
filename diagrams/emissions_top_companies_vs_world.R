# modules/company_vs_world.R
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr) # Added tidyr for pivot_longer

# Load data (ensure this path is correct relative to your app.R)
.company_emissions <- read_csv("data/data_cleaned/GHG_by_sector_and_companies.csv")
colnames(.company_emissions) <- gsub(" ", "_", colnames(.company_emissions))

mod_company_vs_world_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("companyWorldChart"), height = "700px")
  )
}

# Update function signature to accept race_year
mod_company_vs_world_server <- function(id, race_year) {
  moduleServer(id, function(input, output, session) {
    output$companyWorldChart <- renderPlot({
      
      # 1. Access the passed reactive value using parentheses ()
      # We check if it exists to avoid startup errors
      req(race_year()) 
      selected_year <- race_year()
      
      # Aggregate for that year
      summary_data <- .company_emissions %>%
        filter(year == selected_year) %>%
        summarise(
          `Top Emitting Companies` = sum(total_emissions_MtCO2e, na.rm = TRUE),
          `World Total` = first(world)
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Category", values_to = "Emissions")
      
      # Static horizontal bar chart
      ggplot(summary_data, aes(x = Emissions, y = Category, fill = Category)) +
        geom_col(width = 0.6) +
        labs(
          title = "GHG Emission: Top Emitting Companies vs World Total",
          subtitle = paste("Year:", selected_year),
          x = "Emissions (MtCO2e)", y = NULL
        ) +
        theme_minimal(base_size = 14) + 
        theme(legend.position = "none")
    })
  })
}