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

mod_global_heatmap_server <- function(id, map_year, map_metric) {
  moduleServer(id, function(input, output, session) {
    output$map_explanation <- renderUI({
      text <- switch(map_metric(),
        "total" = "Total greenhouse gas emissions in Million tonnes of CO2 equivalent (MtCO2e).",
        "per_capita" = "Average emissions per person (tCO2e/capita).",
        "per_gdp" = "Emissions intensity relative to economic output (tCO2e/million $ GDP).",
        "accumulated" = "Cumulative sum of annual emissions from start of dataset up to selected year."
      )
      tags$p(text, style = "font-size: 0.9em; color: #7f8c8d; font-style: italic; margin-bottom: 15px;")
    })

    output$choroplethMap <- renderLeaflet({
      metric_label <- switch(map_metric(),
        "total" = "MtCO2e",
        "per_capita" = "tCO2e/capita",
        "per_gdp" = "kgCO2e/$",
        "accumulated" = "MtCO2e (Cumulative)"
      )

      # Get world geometry
      world <- ne_countries(scale = 110, returnclass = "sf")

      # Prepare data based on metric
      if (map_metric() == "accumulated") {
        country_emissions <- .edgar_hm |>
          dplyr::filter(year <= map_year()) |>
          dplyr::group_by(country_code, country_clean) |>
          dplyr::summarise(metric_value = sum(total_emissions_MtCO2e, na.rm = TRUE), .groups = "drop")
      } else {
        metric_col <- switch(map_metric(),
          "total" = "total_emissions_MtCO2e",
          "per_capita" = "emissions_per_capita",
          "per_gdp" = "emissions_per_GDP"
        )
        country_emissions <- .edgar_hm |>
          dplyr::filter(year == map_year()) |>
          dplyr::select(country_code, country_clean, metric_value = dplyr::all_of(metric_col))
      }

      # Add ISO matching
      country_emissions <- country_emissions |>
        dplyr::mutate(
          iso_match = country_code,
          iso_match = ifelse(is.na(iso_match) | iso_match == "" | nchar(iso_match) != 3,
            countrycode::countrycode(country_clean, origin = "country.name", destination = "iso3c"),
            iso_match
          ),
          iso_match = toupper(trimws(iso_match))
        ) |>
        dplyr::select(iso_match, country_clean, metric_value)

      # Aggregation
      emissions_by_iso <- country_emissions |>
        dplyr::filter(!is.na(iso_match)) |>
        dplyr::group_by(iso_match) |>
        dplyr::summarise(value_iso = sum(metric_value, na.rm = TRUE), .groups = "drop")

      emissions_by_name <- country_emissions |>
        dplyr::group_by(country_clean) |>
        dplyr::summarise(value_name = sum(metric_value, na.rm = TRUE), .groups = "drop")

      # Join with world data
      world_data <- world |>
        dplyr::left_join(emissions_by_iso, by = c("iso_a3" = "iso_match")) |>
        dplyr::left_join(emissions_by_name, by = c("name_long" = "country_clean")) |>
        dplyr::mutate(
          value = dplyr::coalesce(value_iso, value_name),
          value_display = ifelse(is.na(value), "No data",
            paste0(format(round(value, 2), big.mark = ","), " ", metric_label)
          ),
          tooltip = sprintf("<strong>%s</strong><br/>Year: %s<br/>%s", name, map_year(), value_display)
        ) |>
        dplyr::select(-value_iso, -value_name)

      # Color palette
      # A "Value-Linked" Heat Scale
      # Light/Bright (Low Value) -> Dark/Saturated (High Value)
      pal <- colorNumeric(
        palette = c(
          "#FFF7BC", # Very low (Lightest Value)
          "#FEE391",
          "#FEC44F", # Mid-low
          "#FB9A29", # Mid (Matches your 'Orange' sector anchor)
          "#EC7014",
          "#CC4C02", # High
          "#802405" # Very high (Darkest Value - heavy GHG)
        ),
        domain = world_data$value,
        na.color = "#F2F2F2" # Very light grey so it disappears into the map
      )

      # Create map
      map <- leaflet(world_data, options = leafletOptions(
        worldCopyJump = FALSE,
        maxBoundsViscosity = 1.0,
        minZoom = 1.5
      )) %>%
        addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(
          noWrap = TRUE,
          bounds = list(c(-90, -180), c(90, 180)) # Restrict tiles to valid world coordinates
        )) %>%
        setView(lng = 0, lat = 20, zoom = 2) %>%
        setMaxBounds(lng1 = -179.9, lat1 = -89.9, lng2 = 179.9, lat2 = 89.9) %>%
        addPolygons(
          fillColor = ~ pal(value), fillOpacity = 0.8,
          color = "#FFFFFF", weight = 1,
          highlightOptions = highlightOptions(
            weight = 3, color = "#000000",
            fillOpacity = 0.9, bringToFront = TRUE
          ),
          label = lapply(world_data$tooltip, htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px", direction = "auto"
          )
        )

      # Add Legend
      map <- leaflet::addLegend(
        map,
        pal = pal, values = world_data$value,
        title = paste("Emissions (", metric_label, ")<br>Year:", map_year()),
        position = "bottomright", opacity = 0.7,
        labFormat = labelFormat(big.mark = ",")
      )

      map
    })
  })
}
