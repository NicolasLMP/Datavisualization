# diagrams/ai_analysis.R
library(plotly)
library(dplyr)
library(tidyr)

# Load data
.ai_data <- read.csv("data/data_cleaned/GHG_by_sector_and_country.csv", stringsAsFactors = FALSE)

mod_ai_analysis_ui <- function(id) {
    ns <- NS(id)
    tagList(
        plotlyOutput(ns("radarChart"), height = "650px")
    )
}

mod_ai_analysis_server <- function(id, selected_year, selected_countries) {
    moduleServer(id, function(input, output, session) {
        output$radarChart <- renderPlotly({
            req(selected_year())
            req(selected_countries())

            # 1. Filter Data dynamically
            df_filtered <- .ai_data %>%
                filter(
                    year == selected_year(),
                    Sector != "Total including LUCF" & Sector != "Total excluding LUCF"
                )

            # 2. Calculate Sector Shares (%) for Specific Emitters
            df_radar <- df_filtered %>%
                filter(Country %in% selected_countries()) %>%
                group_by(Country) %>%
                mutate(
                    Total_Country = sum(CO2e, na.rm = TRUE),
                    Share = (CO2e / Total_Country) * 100
                ) %>%
                ungroup() %>%
                select(Country, Sector, Share)

            # Validate we have data
            validate(
                need(nrow(df_radar) > 0, "No data available for the selected combination.")
            )

            # 3. Reshape for Plotly
            all_sectors <- unique(df_filtered$Sector)
            df_radar_complete <- df_radar %>%
                complete(Country, Sector = all_sectors, fill = list(Share = 0))

            # 4. Plot - Radar Chart works best by adding traces one by one
            fig <- plot_ly(type = "scatterpolar", fill = "toself")

            # Color palette for dynamic selection (extended palette)
            palette <- c("#E9BE86", "#6574B9", "#4DC3B3", "#D970C4", "#A7CE47", "#F1D54A", "#F28E5C", "#B5B5B5", "#2c3e50", "#e74c3c")

            for (i in seq_along(selected_countries())) {
                entity <- selected_countries()[i]
                entity_data <- df_radar_complete %>% filter(Country == entity)

                # Radar charts need to close the loop (repeat first point)
                r_vals <- c(entity_data$Share, entity_data$Share[1])
                theta_vals <- c(entity_data$Sector, entity_data$Sector[1])

                # Cycle through colors
                col <- palette[(i - 1) %% length(palette) + 1]

                # Special style for GLOBAL TOTAL if present
                dash_style <- if (entity == "GLOBAL TOTAL") "dot" else "solid"

                fig <- fig %>% add_trace(
                    r = r_vals,
                    theta = theta_vals,
                    name = ifelse(entity == "GLOBAL TOTAL", "Global Average", entity),
                    line = list(color = col, dash = dash_style),
                    marker = list(color = col)
                )
            }

            fig %>%
                layout(
                    polar = list(
                        radialaxis = list(
                            visible = TRUE,
                            range = c(0, max(df_radar_complete$Share, na.rm = TRUE) + 5),
                            ticksuffix = "%"
                        )
                    ),
                    # Dynamic Title based on selection
                    title = list(
                        text = paste0("<b>Sector Fingerprint (", selected_year(), ")</b>"),
                        y = 1.2,
                        x = 0.5,
                        xanchor = "center",
                        yanchor = "top"
                    ),
                    legend = list(title = list(text = "Economy Profile")),
                    margin = list(t = 80, l = 50, r = 50, b = 20)
                )
        })
    })
}
