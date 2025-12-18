# modules/emissions_by_region.R
library(plotly)
library(dplyr)
library(tidyr)
library(countrycode)

# Chargement et préparation des données (peut être mutualisé)
# Updated path to cleaned data
.edgar <- read.csv("data/data_cleaned/GHG_total_gdp_capita.csv", stringsAsFactors = FALSE)

# Re-mapping columns using direct assignment to avoid issues
colnames(.edgar) <- c("country_code", "country", "year", "total_emissions_MtCO2e", "emissions_per_capita", "emissions_per_GDP")
.edgar$country_code <- toupper(trimws(.edgar$country_code))

.edgar$country_clean <- .edgar$country
.edgar$country_clean[.edgar$country == "Switzerland and Liechtenstein"] <- "Switzerland"
.edgar$country_clean[.edgar$country == "Denmark (with Faroe Islands and Greenland)"] <- "Denmark"
.edgar$country_clean[.edgar$country == "Serbia and Montenegro"] <- "Serbia"

.edgar$continent <- countrycode(.edgar$country_clean, "country.name", "continent", warn = FALSE)
.edgar <- .edgar |>
  dplyr::filter(!is.na(continent)) |>
  dplyr::filter(!grepl("International|EU27|EARTH", country, ignore.case = TRUE)) |>
  dplyr::arrange(country, year) |>
  dplyr::group_by(country) |>
  dplyr::mutate(accumulated_emissions = cumsum(total_emissions_MtCO2e)) |>
  dplyr::ungroup()

.continent_data <- .edgar |>
  dplyr::group_by(continent, year) |>
  dplyr::summarise(
    total_emissions_MtCO2e = sum(total_emissions_MtCO2e, na.rm = TRUE),
    emissions_per_capita = mean(emissions_per_capita, na.rm = TRUE),
    emissions_per_GDP = mean(emissions_per_GDP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(continent, year) |>
  dplyr::group_by(continent) |>
  dplyr::mutate(accumulated_emissions = cumsum(total_emissions_MtCO2e)) |>
  dplyr::ungroup()

.country_data <- .edgar

mod_emissions_by_region_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("emissionsPlot"), height = "600px")
  )
}

mod_emissions_by_region_server <- function(id, continents, countries, metric, year_control) {
  moduleServer(id, function(input, output, session) {
    output$emissionsPlot <- renderPlotly({
      req(year_control())
      metric_col <- switch(metric(),
        "total" = "total_emissions_MtCO2e",
        "per_capita" = "emissions_per_capita",
        "per_gdp" = "emissions_per_GDP",
        "accumulated" = "accumulated_emissions"
      )
      metric_label <- switch(metric(),
        "total" = "Emissions (MtCO2e)",
        "per_capita" = "Emissions per Capita (tCO2e/person)",
        "per_gdp" = "Emissions per GDP (kgCO2e/$)",
        "accumulated" = "Accumulated Emissions (MtCO2e)"
      )

      continent_filtered <- if (!is.null(continents()) && length(continents()) > 0) {
        .continent_data |> dplyr::filter(continent %in% continents(), year <= year_control())
      } else {
        .continent_data[0, ]
      }

      country_filtered <- NULL
      if (!is.null(countries()) && length(countries()) > 0) {
        country_filtered <- .country_data |> dplyr::filter(country %in% countries(), year <= year_control())
      }

      continent_colors <- c(
        "Africa"   = "#E9BE86", # Tan
        "Asia"     = "#F28E5C", # Soft Orange
        "Europe"   = "#6574B9", # Slate Blue
        "Americas" = "#4DC3B3", # Teal
        "Oceania"  = "#A7CE47"  # Lime
      )
      plot <- plot_ly()

      if (!is.null(continent_filtered) && nrow(continent_filtered) > 0) {
        for (cont in unique(continent_filtered$continent)) {
          data_subset <- continent_filtered |> dplyr::filter(continent == cont)
          y_values <- data_subset[[metric_col]]
          x_values <- data_subset$year

          highlight_color <- continent_colors[cont]
          if (is.na(highlight_color)) highlight_color <- "#1f77b4"

          plot <- plot |>
            add_trace(
              x = x_values, y = y_values,
              type = "scatter", mode = "lines+markers",
              name = cont,
              line = list(color = highlight_color, width = 3),
              marker = list(size = 6, color = highlight_color),
              hovertemplate = paste(
                "<b>", cont, "</b><br>",
                "Year: %{x}<br>",
                metric_label, ": %{y:.2f}<br>",
                "<extra></extra>"
              )
            )
        }
      }

      if (!is.null(country_filtered) && nrow(country_filtered) > 0) {
        for (ctry in unique(country_filtered$country)) {
          data_subset <- country_filtered |> dplyr::filter(country == ctry)
          cont <- unique(data_subset$continent)[1]
          y_values <- data_subset[[metric_col]]
          x_values <- data_subset$year

          base_color <- continent_colors[cont]
          if (is.na(base_color)) base_color <- "#1f77b4"

          plot <- plot |>
            add_trace(
              x = x_values, y = y_values,
              type = "scatter", mode = "lines+markers",
              name = ctry,
              line = list(color = base_color, width = 2, dash = "dash"),
              marker = list(size = 4, color = base_color),
              hovertemplate = paste(
                "<b>", ctry, "</b><br>",
                "Year: %{x}<br>",
                metric_label, ": %{y:.2f}<br>",
                "<extra></extra>"
              )
            )
        }
      }

      plot |>
        layout(
          xaxis = list(title = "Year", gridcolor = "#E5E5E5", range = c(1970, year_control())),
          yaxis = list(title = metric_label, gridcolor = "#E5E5E5", rangemode = "tozero"),
          hovermode = "closest",
          legend = list(orientation = "v", x = 1.02, y = 1, bgcolor = "rgba(255,255,255,0.8)"),
          plot_bgcolor = "#F8F9FA", paper_bgcolor = "#FFFFFF"
        )
    })
  })
}
