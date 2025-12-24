# modules/emissions_by_region.R
library(plotly)
library(dplyr)
library(tidyr)
library(countrycode)
library(khroma)


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

      
      # Generate 5 colors from the 'bright' scheme
      pt_bright_5 <- as.character(color("bright")(5))
      
      # Create the fixed mapping for continents
      continent_colors <- c(
        "Europe"   = pt_bright_5[1], # Blue
        "Asia"     = pt_bright_5[2], # Red
        "Americas"   = pt_bright_5[3], # Green
        "Africa" = pt_bright_5[4], # Yellow
        "Oceania"  = pt_bright_5[5]  # Cyan
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

      # Define metric explanations
      metric_explanation <- switch(metric(),
        "total" = "Total greenhouse gas emissions in Million tonnes of CO2 equivalent (MtCO2e).",
        "per_capita" = "Average emissions per person (tCO2e/capita).",
        "per_gdp" = "Emissions intensity relative to economic output (tCO2e/million $ GDP).",
        "accumulated" = "Cumulative sum of annual emissions from start of dataset up to selected year."
      )

      # Update layout for titles, explanations, and axis lines
      plot |>
        layout(
          title = list(
            text = "How have emissions evolved over time?",
            x = 0.05
          ),
          xaxis = list(
            title = "Year",
            gridcolor = "#E5E5E5",
            range = c(1970, year_control()),
            showline = TRUE,
            linewidth = 2,
            linecolor = "black",
            mirror = FALSE # Only bottom line
          ),
          yaxis = list(
            title = metric_label,
            gridcolor = "#E5E5E5",
            rangemode = "tozero",
            showline = TRUE,
            linewidth = 2,
            linecolor = "black",
            mirror = FALSE # Only left line
          ),
          annotations = list(
            list(
              x = 0,
              y = 1.08,
              xref = "paper",
              yref = "paper",
              text = paste0("<i>", metric_explanation, "</i>"),
              showarrow = FALSE,
              font = list(size = 12, color = "gray")
            )
          ),
          hovermode = "closest",
          legend = list(orientation = "v", x = 1.02, y = 1, bgcolor = "rgba(255,255,255,0.8)"),
          plot_bgcolor = "#F8F9FA", paper_bgcolor = "#FFFFFF",
          margin = list(t = 80) # Add margin for title/explanation
        )
    })
  })
}
