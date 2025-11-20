# modules/emissions_by_region.R
library(plotly)
library(dplyr)
library(tidyr)
library(countrycode)

# Chargement et préparation des données (peut être mutualisé)
.edgar <- read.csv("data/GHG_by_total_gdp_capita.csv", stringsAsFactors = FALSE)
.edgar$country_code <- toupper(trimws(.edgar$country_code))
.edgar$country_clean <- .edgar$country
.edgar$country_clean[.edgar$country == "Spain and Andorra"] <- "Spain"
.edgar$country_clean[.edgar$country == "France and Monaco"] <- "France"
.edgar$country_clean[.edgar$country == "Italy, San Marino and the Holy See"] <- "Italy"
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
    total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE),
    emissions_per_capita = mean(emissions_per_capita, na.rm = TRUE),
    emissions_per_GDP = mean(emissions_per_GDP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(continent, year) |>
  dplyr::group_by(continent) |>
  dplyr::mutate(accumulated_emissions = cumsum(total_emissions)) |>
  dplyr::ungroup()

.country_data <- .edgar

mod_emissions_by_region_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Global Emissions by Continents and Countries"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Select Regions"),
        selectInput(ns("continents"), "Continents:",
                    choices = sort(unique(.continent_data$continent)),
                    selected = c("Europe","Asia"), multiple = TRUE),
        selectInput(ns("countries"), "Countries (optional):",
                    choices = sort(unique(.country_data$country)),
                    selected = NULL, multiple = TRUE),
        hr(),
        h4("Metric"),
        radioButtons(ns("metric"), NULL,
                     choices = c("Total Emissions" = "total",
                                 "Per Capita" = "per_capita",
                                 "Per GDP" = "per_gdp",
                                 "Total Accumulated" = "accumulated"),
                     selected = "total"),
        hr(),
        h4("Time Control"),
        sliderInput(ns("year_control"), "Show data through year:",
                    min = 1970, max = 2023, value = 2023, step = 1, sep = "",
                    animate = animationOptions(interval = 500, loop = FALSE)),
        helpText("Click play to animate changes year by year"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'per_gdp'", ns("metric")),
          helpText("Note: Per GDP data only available from 1990 onwards")
        )
      ),
      mainPanel(
        width = 9,
        plotlyOutput(ns("emissionsPlot"), height = "600px"),
        br(),
        verbatimTextOutput(ns("emissionsInfo"))
      )
    )
  )
}

mod_emissions_by_region_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$emissionsPlot <- renderPlotly({
      req(input$year_control)
      metric_col <- switch(input$metric,
                           "total" = "total_emissions",
                           "per_capita" = "emissions_per_capita",
                           "per_gdp" = "emissions_per_GDP",
                           "accumulated" = "accumulated_emissions")
      metric_label <- switch(input$metric,
                             "total" = "Emissions (MtCO2e)",
                             "per_capita" = "Emissions per Capita (tCO2e/person)",
                             "per_gdp" = "Emissions per GDP (kgCO2e/$)",
                             "accumulated" = "Accumulated Emissions (MtCO2e)")

      continent_filtered <- if (!is.null(input$continents) && length(input$continents) > 0) {
        .continent_data |> dplyr::filter(continent %in% input$continents, year <= input$year_control)
      } else {.continent_data[0, ]}

      country_filtered <- NULL
      if (!is.null(input$countries) && length(input$countries) > 0) {
        country_filtered <- .country_data |> dplyr::filter(country %in% input$countries, year <= input$year_control)
      }

      continent_colors <- c("Africa"="#D2691E","Asia"="#DC143C","Europe"="#4169E1","Americas"="#228B22","Oceania"="#9370DB")
      default_line_color <- "#BDBDBD"
      plot <- plot_ly()

      if (!is.null(continent_filtered) && nrow(continent_filtered) > 0) {
        for (cont in unique(continent_filtered$continent)) {
          data_subset <- continent_filtered |> dplyr::filter(continent == cont)
          y_values <- data_subset[[metric_col]]
          highlight_color <- continent_colors[cont]; if (is.na(highlight_color)) highlight_color <- "#1f77b4"
          color_matrix <- matrix(highlight_color, nrow = nrow(data_subset), ncol = 1)
          plot <- plot |>
            add_trace(
              data = data_subset, x = ~year, y = y_values,
              type = 'scatter', mode = 'lines+markers',
              name = cont,
              line = list(color = default_line_color, width = 3),
              marker = list(size = 6, color = default_line_color),
              customdata = color_matrix,
              hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                    'Year: %{x}<br>',
                                    metric_label, ': %{y:.2f}<br>',
                                    '<extra></extra>')
            )
        }
      }

      if (!is.null(country_filtered) && nrow(country_filtered) > 0) {
        for (ctry in unique(country_filtered$country)) {
          data_subset <- country_filtered |> dplyr::filter(country == ctry)
          cont <- unique(data_subset$continent)[1]
          y_values <- data_subset[[metric_col]]
          base_color <- continent_colors[cont]; if (is.na(base_color)) base_color <- "#1f77b4"
          color_matrix <- matrix(base_color, nrow = nrow(data_subset), ncol = 1)
          plot <- plot |>
            add_trace(
              data = data_subset, x = ~year, y = y_values,
              type = 'scatter', mode = 'lines+markers',
              name = ctry,
              line = list(color = default_line_color, width = 2, dash = 'dash'),
              marker = list(size = 4, color = default_line_color),
              customdata = color_matrix,
              hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                    'Year: %{x}<br>',
                                    metric_label, ': %{y:.2f}<br>',
                                    '<extra></extra>')
            )
        }
      }

      plot |>
        layout(
          xaxis = list(title = "Year", gridcolor = '#E5E5E5', range = c(1970, input$year_control)),
          yaxis = list(title = metric_label, gridcolor = '#E5E5E5', rangemode = "tozero"),
          hovermode = "closest",
          legend = list(orientation = "v", x = 1.02, y = 1, bgcolor = 'rgba(255,255,255,0.8)'),
          plot_bgcolor = '#F8F9FA', paper_bgcolor = '#FFFFFF'
        ) |>
        htmlwidgets::onRender(
          "function(el, x) {
             const defaultColor = '#BDBDBD';
             const originalColors = (x.data || []).map(function(trace) {
               if (!trace.customdata || !trace.customdata.length) return defaultColor;
               const firstRow = trace.customdata[0];
               return Array.isArray(firstRow) ? (firstRow[0] || defaultColor) : (trace.customdata[0] || defaultColor);
             });
             function applyColors(activeIndex) {
               originalColors.forEach(function(color, idx) {
                 const target = (activeIndex !== null && idx === activeIndex) ? color : defaultColor;
                 Plotly.restyle(el, {'line.color': [target], 'marker.color': [target]}, [idx]);
               });
             }
             applyColors(null);
             el.on('plotly_hover', function(evt) {
               const curve = evt.points[0].curveNumber;
               applyColors(curve);
             });
             el.on('plotly_unhover', function() { applyColors(null); });
           }"
        )
    })

    output$emissionsInfo <- renderText({
      req(input$year_control)
      metric_col <- switch(input$metric,
                           "total" = "total_emissions",
                           "per_capita" = "emissions_per_capita",
                           "per_gdp" = "emissions_per_GDP",
                           "accumulated" = "accumulated_emissions")
      if ((input$metric %in% c("total","accumulated")) &&
          !is.null(input$continents) && length(input$continents) > 0) {
        total <- .continent_data |>
          dplyr::filter(continent %in% input$continents, year <= input$year_control) |>
          dplyr::summarise(total = sum(get(metric_col), na.rm = TRUE)) |>
          dplyr::pull(total)
        paste0("Total for selected regions through ", input$year_control, ":\n",
               format(round(total, 2), big.mark = ","), " MtCO2e")
      } else if (is.null(input$continents) || length(input$continents) == 0) {
        paste0("Selected metric: ", input$metric, "\n",
               "Showing values for chosen countries")
      } else {
        paste0("Selected metric: ", input$metric, "\n",
               "Showing average values for selected continents")
      }
    })
  })
}
