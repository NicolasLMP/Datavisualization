# diagrams/ai_analysis.R
library(plotly)
library(dplyr)
library(tidyr)

# Load data
.ai_data <- read.csv("data/data_cleaned/GHG_by_sector_and_country.csv", stringsAsFactors = FALSE)

mod_ai_analysis_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$div(
            style = "margin-bottom: 20px;",
            tags$h3("Sector Fingerprint: Comparing Emission Profiles (2022)",
                style = "font-family: 'Inter', sans-serif; font-weight: bold; color: #2c3e50; text-align: center; margin-bottom: 20px;"
            )
        ),
        plotlyOutput(ns("radarChart"), height = "650px")
    )
}

mod_ai_analysis_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$radarChart <- renderPlotly({
            # 1. Filter Data for 2022 and clean sectors
            df_2022 <- .ai_data %>%
                filter(
                    year == 2022,
                    Sector != "Total including LUCF" & Sector != "Total excluding LUCF"
                )

            # 2. Calculate Sector Shares (%) for Specific Emitters
            target_entities <- c("China", "United States", "EU27", "GLOBAL TOTAL")

            df_radar <- df_2022 %>%
                filter(Country %in% target_entities) %>%
                group_by(Country) %>%
                mutate(
                    Total_Country = sum(CO2e, na.rm = TRUE),
                    Share = (CO2e / Total_Country) * 100
                ) %>%
                ungroup() %>%
                select(Country, Sector, Share)

            # 3. Reshape for Plotly
            # Ensure all sectors are present for all countries (fill 0 if missing)
            all_sectors <- unique(df_radar$Sector)
            df_radar_complete <- df_radar %>%
                complete(Country, Sector = all_sectors, fill = list(Share = 0))

            # 4. Plot - Radar Chart works best by adding traces one by one
            fig <- plot_ly(type = "scatterpolar", fill = "toself")

            # Define colors/styles for targets
            styles <- list(
                "China" = list(color = "#E9BE86", dash = "solid"), # Orange
                "United States" = list(color = "#6574B9", dash = "solid"), # Blue
                "EU27" = list(color = "#4DC3B3", dash = "solid"), # Teal
                "GLOBAL TOTAL" = list(color = "#2c3e50", dash = "dot") # Dark Grey Dotted
            )

            for (entity in target_entities) {
                entity_data <- df_radar_complete %>% filter(Country == entity)

                # Radar charts need to close the loop (repeat first point)
                r_vals <- c(entity_data$Share, entity_data$Share[1])
                theta_vals <- c(entity_data$Sector, entity_data$Sector[1])

                style <- styles[[entity]]

                fig <- fig %>% add_trace(
                    r = r_vals,
                    theta = theta_vals,
                    name = ifelse(entity == "GLOBAL TOTAL", "Global Average", entity),
                    line = list(color = style$color, dash = style$dash),
                    marker = list(color = style$color)
                )
            }

            fig %>%
                layout(
                    polar = list(
                        radialaxis = list(
                            visible = TRUE,
                            range = c(0, max(df_radar_complete$Share) + 5),
                            ticksuffix = "%"
                        )
                    ),
                    legend = list(title = list(text = "Economy Profile")),
                    margin = list(t = 20, l = 50, r = 50, b = 20)
                )
        })
    })
}
