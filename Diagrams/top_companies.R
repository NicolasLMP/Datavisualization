# modules/top_companies.R
library(ggplot2)
library(gganimate)
library(dplyr)

.company_emissions <- read.csv("data/GHG_by_sector_and_company_no_nation_state.csv", stringsAsFactors = FALSE)
colnames(.company_emissions) <- gsub(" ", "_", colnames(.company_emissions))

mod_top_companies_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Top 10 Emitting Companies"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Time Controls"),
        sliderInput(ns("race_year"), "Year:",
                    min = min(.company_emissions$year, na.rm = TRUE),
                    max = max(.company_emissions$year, na.rm = TRUE),
                    value = min(.company_emissions$year, na.rm = TRUE),
                    step = 1, sep = "",
                    animate = animationOptions(interval = 500, loop = FALSE)),
        hr(),
        h4("Display Options"),
        checkboxInput(ns("show_percentage"), "Show percentage of total", value = TRUE),
        checkboxInput(ns("show_rank"), "Show rank numbers", value = TRUE),
        hr(),
        helpText("Click play to see rankings change over time")
      ),
      mainPanel(
        width = 9,
        plotOutput(ns("raceBarChart"), height = "700px")
      )
    )
  )
}

mod_top_companies_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$raceBarChart <- renderPlot({
      # Get data for selected year
      company_totals <- .company_emissions |>
        dplyr::filter(year == input$race_year) |>
        dplyr::group_by(parent_entity) |>
        dplyr::summarise(emissions = sum(total_emissions_MtCO2e, na.rm = TRUE), .groups = "drop")
      
      total_companies <- nrow(company_totals)
      
      # Get dominant commodity
      dominant_commodity <- .company_emissions |>
        dplyr::filter(year == input$race_year) |>
        dplyr::group_by(parent_entity, commodity) |>
        dplyr::summarise(commodity_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE), .groups = "drop") |>
        dplyr::group_by(parent_entity) |>
        dplyr::slice_max(order_by = commodity_emissions, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::select(parent_entity, primary_commodity = commodity)
      
      # Get top 10 and calculate stats
      year_data <- company_totals |>
        dplyr::left_join(dominant_commodity, by = "parent_entity") |>
        dplyr::arrange(desc(emissions)) |>
        dplyr::slice_head(n = 10)
      
      total_all <- sum(company_totals$emissions, na.rm = TRUE)
      
      # Add "All Companies" row
      all_companies_row <- data.frame(
        parent_entity = "All Companies",
        emissions = total_all,
        primary_commodity = "Total"
      )
      
      year_data <- dplyr::bind_rows(all_companies_row, year_data)
      
      top10_total <- sum(year_data$emissions[year_data$parent_entity != "All Companies"], na.rm = TRUE)
      top10_share <- ifelse(total_all > 0, (top10_total / total_all) * 100, NA_real_)
      
      # Create text shown at the top with the title
      caption_text <- sprintf(
        "Top 10 emit %s MtCO2e (%.1f%%) out of %s MtCO2e total (%d companies)",
        format(round(top10_total, 1), big.mark = ","),
        top10_share,
        format(round(total_all, 1), big.mark = ","),
        total_companies
      )
      
      # Add rank and formatting
      year_data <- year_data |>
        dplyr::mutate(
          percentage = ifelse(total_all > 0, (emissions / total_all) * 100, 0),
          rank = dplyr::row_number(),
          primary_commodity = ifelse(is.na(primary_commodity), "Other / Mixed", primary_commodity),
          # Label logic: No percentage for "All Companies"
          label = paste0(
            round(emissions, 1), " MtCO2e",
            ifelse(input$show_percentage & parent_entity != "All Companies", paste0(" (", round(percentage, 1), "%)"), "")
          )
        )
      
      # Color palette
      commodity_colors <- c(
        "Total" = "#2C3E50", # Dark Blue/Grey for All Companies
        "Oil & NGL" = "#8B0000",
        "Natural Gas" = "#DC143C",
        "Bituminous Coal" = "#2F4F4F",
        "Anthracite Coal" = "#000000",
        "Thermal Coal" = "#696969",
        "Lignite Coal" = "#808080",
        "SubBituminous Coal" = "#A9A9A9",
        "Metallurgical Coal" = "#555555",
        "Cement" = "#D3D3D3",
        "Other / Mixed" = "#8B4513"
      )
      
      # Create the plot
      p <- ggplot(year_data, aes(x = rank, fill = primary_commodity, group = parent_entity)) +
        geom_tile(aes(y = emissions / 2, height = emissions, width = 0.9),
                  alpha = 0.9, color = "white", size = 1) +
        geom_text(aes(y = 0, label = parent_entity),
                  hjust = "right", nudge_x = -0.1, size = 5, fontface = "bold") +
        geom_text(aes(y = emissions, label = label),
                  hjust = "left", nudge_x = 0.05, size = 4) +
        scale_fill_manual(values = commodity_colors, name = "Primary Commodity") +
        coord_flip(clip = "off", expand = FALSE) +
        scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
        scale_x_reverse() +
        labs(
          title = paste0("Top 10 Emitting Companies - ", input$race_year),
          subtitle = caption_text,
          x = NULL,
          y = "Emissions (MtCO2e)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0, size = 22, face = "bold", color = "#333333"),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#555555", margin = margin(b = 8)),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 12),
          plot.margin = margin(1, 4, 1, 8, "cm"),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "#E5E5E5"),
          plot.background = element_rect(fill = "#FFFFFF", color = NA),
          panel.background = element_rect(fill = "#F8F9FA", color = NA)
        )
      
      # Add rank numbers if requested
      if (input$show_rank) {
        p <- p + geom_text(aes(y = 0, label = paste0("#", rank)),
                           hjust = "right", nudge_x = -0.5, size = 6,
                           fontface = "bold", color = "#333333")
      }
      
      p
    })
  })
}
