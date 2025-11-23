# modules/global_heatmap.R
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(htmltools)

# Updated path to cleaned data
.edgar_hm <- read.csv("data/data_cleaned/GHG_total_gdp_capita.csv", stringsAsFactors = FALSE)

# Re-mapping columns using direct assignment to avoid issues
colnames(.edgar_hm) <- c("country_code", "country", "year", "total_emissions_MtCO2e", "emissions_per_capita", "emissions_per_GDP")
.edgar_hm$country_code <- toupper(trimws(.edgar_hm$country_code))
.edgar_hm$country_clean <- .edgar_hm$country
.edgar_hm$country_clean[.edgar_hm$country == "Spain and Andorra"] <- "Spain"
.edgar_hm$country_clean[.edgar_hm$country == "France and Monaco"] <- "France"
.edgar_hm$country_clean[.edgar_hm$country == "Italy, San Marino and the Holy See"] <- "Italy"
.edgar_hm$country_clean[.edgar_hm$country == "Switzerland and Liechtenstein"] <- "Switzerland"
.edgar_hm$country_clean[.edgar_hm$country == "Denmark (with Faroe Islands and Greenland)"] <- "Denmark"
.edgar_hm$country_clean[.edgar_hm$country == "Serbia and Montenegro"] <- "Serbia"

mod_global_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("choroplethMap"), height = "600px")
  )
}

mod_global_heatmap_server <- function(id, map_year, map_metric) {
  moduleServer(id, function(input, output, session) {
    output$choroplethMap <- renderLeaflet({
      metric_col <- switch(map_metric(),
        "total" = "total_emissions_MtCO2e",
        "per_capita" = "emissions_per_capita",
        "per_gdp" = "emissions_per_GDP"
      )
      metric_label <- switch(map_metric(),
        "total" = "MtCO2e",
        "per_capita" = "tCO2e/person",
        "per_gdp" = "kgCO2e/$"
      )

      world <- ne_countries(scale = "medium", returnclass = "sf")

      country_emissions <- .edgar_hm |>
        dplyr::filter(year == map_year()) |>
        dplyr::mutate(
          iso_match = country_code,
          iso_match = ifelse(is.na(iso_match) | iso_match == "" | nchar(iso_match) != 3,
            countrycode(country_clean, origin = "country.name", destination = "iso3c"),
            iso_match
          ),
          iso_match = toupper(trimws(iso_match))
        ) |>
        dplyr::select(iso_match, country_clean, metric_value = dplyr::all_of(metric_col))

      emissions_by_iso <- country_emissions |>
        dplyr::filter(!is.na(iso_match)) |>
        dplyr::group_by(iso_match) |>
        dplyr::summarise(value_iso = sum(metric_value, na.rm = TRUE), .groups = "drop")

      emissions_by_name <- country_emissions |>
        dplyr::group_by(country_clean) |>
        dplyr::summarise(value_name = sum(metric_value, na.rm = TRUE), .groups = "drop")

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

      pal <- colorNumeric(
        palette = c("#FFD700", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
        domain = world_data$value, na.color = "#D3D3D3"
      )

      leaflet(world_data) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = 0, lat = 20, zoom = 2) |>
        addPolygons(
          fillColor = ~ pal(value), fillOpacity = 0.7,
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
        ) |>
        addLegend(
          pal = pal, values = ~value,
          title = paste("Emissions (", metric_label, ")<br>Year:", map_year()),
          position = "bottomright", opacity = 0.7,
          labFormat = labelFormat(big.mark = ",")
        )
    })
  })
}
