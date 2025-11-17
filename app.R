# ==============================================================================
# Global Emissions Dashboard - Complete R Shiny App (CORRECTED VERSION)
# ==============================================================================

# Install required packages (run once)
# install.packages(c("shiny", "plotly", "dplyr", "tidyr", "leaflet", "sf", 
#                    "rnaturalearth", "rnaturalearthdata", "countrycode", "bslib"))

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# ==============================================================================
# DATA LOADING AND PREPARATION
# ==============================================================================

# Load EDGAR data for country emissions analyses (Diagrams 5 & 6)
edgar <- read.csv("data/edgar_emissions_processed.csv", stringsAsFactors = FALSE)
edgar$country_code <- toupper(trimws(edgar$country_code))

# Manual mapping for special country names in EDGAR that countrycode can't handle
edgar$country_clean <- edgar$country
edgar$country_clean[edgar$country == "Spain and Andorra"] <- "Spain"
edgar$country_clean[edgar$country == "France and Monaco"] <- "France"
edgar$country_clean[edgar$country == "Italy, San Marino and the Holy See"] <- "Italy"
edgar$country_clean[edgar$country == "Switzerland and Liechtenstein"] <- "Switzerland"
edgar$country_clean[edgar$country == "Denmark (with Faroe Islands and Greenland)"] <- "Denmark"
edgar$country_clean[edgar$country == "Serbia and Montenegro"] <- "Serbia"

# Add continent using countrycode package
edgar$continent <- countrycode(
  sourcevar = edgar$country_clean,
  origin = "country.name",
  destination = "continent",
  warn = FALSE
)

# Remove special entities (International Aviation, Shipping, etc.)
edgar <- edgar %>%
  filter(!is.na(continent)) %>%
  filter(!grepl("International|EU27|EARTH", country, ignore.case = TRUE))

# Calculate accumulated emissions for each country
edgar <- edgar %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(accumulated_emissions = cumsum(total_emissions_MtCO2e)) %>%
  ungroup()

# Aggregate by continent and year
continent_data <- edgar %>%
  group_by(continent, year) %>%
  summarise(
    total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE),
    emissions_per_capita = mean(emissions_per_capita, na.rm = TRUE),
    emissions_per_GDP = mean(emissions_per_GDP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(continent, year) %>%
  group_by(continent) %>%
  mutate(accumulated_emissions = cumsum(total_emissions)) %>%
  ungroup()

# Country data is already in edgar dataframe
country_data <- edgar

# Read company emissions data for Diagram 7 only
company_emissions <- read.csv("data/emissions_medium_granularity.csv", stringsAsFactors = FALSE)

# Clean column names (remove spaces)
colnames(company_emissions) <- gsub(" ", "_", colnames(company_emissions))

# ==============================================================================
# USER INTERFACE
# ==============================================================================

ui <- navbarPage(
  title = "Global Emissions Dashboard",
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  
  # ==========================================================================
  # TAB 1: Diagram 5 - Emissions by Continents and Countries (Line Chart)
  # ==========================================================================
  tabPanel(
    "Emissions by Region",
    fluidPage(
      titlePanel("Global Emissions by Continents and Countries"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Select Regions"),
          selectInput(
            "continents", 
            "Continents:",
            choices = sort(unique(continent_data$continent)),
            selected = c("Europe", "Asia"),
            multiple = TRUE
          ),
          selectInput(
            "countries", 
            "Countries (optional):",
            choices = sort(unique(country_data$country)),
            selected = NULL,
            multiple = TRUE
          ),
          hr(),
          h4("Metric"),
          radioButtons(
            "metric", 
            NULL,
            choices = c(
              "Total Emissions" = "total",
              "Per Capita" = "per_capita",
              "Per GDP" = "per_gdp",
              "Total Accumulated" = "accumulated"
            ),
            selected = "total"
          ),
          hr(),
          h4("Time Control"),
          sliderInput(
            "year_control", 
            "Show data through year:",
            min = 1970,
            max = 2023,
            value = 2023,
            step = 1,
            sep = "",
            animate = animationOptions(interval = 500, loop = FALSE)
          ),
          helpText("Click play to animate changes year by year"),
          conditionalPanel(
            condition = "input.metric == 'per_gdp'",
            helpText("Note: Per GDP data only available from 1990 onwards")
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("emissionsPlot", height = "600px"),
          br(),
          verbatimTextOutput("emissionsInfo")
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 2: Diagram 6 - Choropleth Heatmap
  # ==========================================================================
  tabPanel(
    "Global Heatmap",
    fluidPage(
      titlePanel("Global Emissions Choropleth Map"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Time Controls"),
          sliderInput(
            "map_year", 
            "Year:",
            min = 1970,
            max = 2023,
            value = 2023,
            step = 1,
            sep = "",
            animate = animationOptions(interval = 1000, loop = TRUE)
          ),
          hr(),
          h4("Metric"),
          radioButtons(
            "map_metric", 
            NULL,
            choices = c(
              "Total Emissions" = "total",
              "Per Capita" = "per_capita",
              "Per GDP" = "per_gdp"
            ),
            selected = "total"
          ),
          hr(),
          helpText("Hover over countries to see details"),
          helpText("Color scale: Gold → Orange → Red (AFM hot palette)"),
          conditionalPanel(
            condition = "input.map_metric == 'per_gdp'",
            helpText("Note: Per GDP data only available from 1990 onwards")
          )
        ),
        mainPanel(
          width = 9,
          leafletOutput("choroplethMap", height = "600px")
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 3: Diagram 7 - Race Bar Chart (Top Companies)
  # ==========================================================================
  tabPanel(
    "Top Companies",
    fluidPage(
      titlePanel("Top 10 Emitting Companies"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Time Controls"),
          sliderInput(
            "race_year", 
            "Year:",
            min = min(company_emissions$year, na.rm = TRUE),
            max = max(company_emissions$year, na.rm = TRUE),
            value = min(company_emissions$year, na.rm = TRUE),
            step = 1,
            sep = "",
            animate = animationOptions(interval = 500, loop = FALSE)
          ),
          hr(),
          h4("Display Options"),
          checkboxInput(
            "show_percentage", 
            "Show percentage of total", 
            value = TRUE
          ),
          checkboxInput(
            "show_rank", 
            "Show rank numbers", 
            value = TRUE
          ),
          hr(),
          helpText("Click play to see rankings change over time")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("raceBarChart", height = "600px"),
          br(),
          wellPanel(
            h4("Summary Statistics"),
            verbatimTextOutput("totalEmissionsText")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # ==========================================================================
  # DIAGRAM 5: Line Chart - Continents and Countries
  # ==========================================================================
  
  output$emissionsPlot <- renderPlotly({
    
    # Validate inputs
    req(input$year_control)
    
    # Select the appropriate metric column
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
    
    # Filter continent data (optional selection)
    if (!is.null(input$continents) && length(input$continents) > 0) {
      continent_filtered <- continent_data %>%
        filter(
          continent %in% input$continents,
          year <= input$year_control
        )
    } else {
      continent_filtered <- continent_data[0, ]
    }
    
    # Filter country data if countries are selected
    country_filtered <- NULL
    if (!is.null(input$countries) && length(input$countries) > 0) {
      country_filtered <- country_data %>%
        filter(
          country %in% input$countries,
          year <= input$year_control
        )
    }
    
    # Define color palette for continents
    continent_colors <- c(
      "Africa" = "#D2691E",      # Orange/Brown
      "Asia" = "#DC143C",        # Red
      "Europe" = "#4169E1",      # Blue
      "Americas" = "#228B22",    # Green
      "Oceania" = "#9370DB"      # Purple
    )
    default_line_color <- "#BDBDBD"
    
    # Initialize plot
    plot <- plot_ly()
    
    # Add continent lines (thicker, solid when highlighted)
    if (!is.null(continent_filtered) && nrow(continent_filtered) > 0) {
      for (cont in unique(continent_filtered$continent)) {
        data_subset <- continent_filtered %>% filter(continent == cont)
        
        # Get the metric values
        y_values <- data_subset[[metric_col]]
        highlight_color <- continent_colors[cont]
        if (is.na(highlight_color)) {
          highlight_color <- "#1f77b4"
        }
        color_matrix <- matrix(highlight_color, nrow = nrow(data_subset), ncol = 1)
        
        plot <- plot %>%
          add_trace(
            data = data_subset,
            x = ~year,
            y = y_values,
            type = 'scatter',
            mode = 'lines+markers',
            name = cont,
            line = list(
              color = default_line_color, 
              width = 3
            ),
            marker = list(size = 6, color = default_line_color),
            customdata = color_matrix,
            hovertemplate = paste(
              '<b>%{fullData.name}</b><br>',
              'Year: %{x}<br>',
              metric_label, ': %{y:.2f}<br>',
              '<extra></extra>'
            )
          )
      }
    }
    
    # Add country lines (thinner, dashed when highlighted)
    if (!is.null(country_filtered) && nrow(country_filtered) > 0) {
      for (ctry in unique(country_filtered$country)) {
        data_subset <- country_filtered %>% filter(country == ctry)
        cont <- unique(data_subset$continent)[1]
        
        # Get the metric values
        y_values <- data_subset[[metric_col]]
        
        # Get continent color
        base_color <- continent_colors[cont]
        if (is.na(base_color)) {
          base_color <- "#1f77b4"
        }
        color_matrix <- matrix(base_color, nrow = nrow(data_subset), ncol = 1)
        
        plot <- plot %>%
          add_trace(
            data = data_subset,
            x = ~year,
            y = y_values,
            type = 'scatter',
            mode = 'lines+markers',
            name = ctry,
            line = list(
              color = default_line_color, 
              width = 2, 
              dash = 'dash'
            ),
            marker = list(size = 4, color = default_line_color),
            customdata = color_matrix,
            hovertemplate = paste(
              '<b>%{fullData.name}</b><br>',
              'Year: %{x}<br>',
              metric_label, ': %{y:.2f}<br>',
              '<extra></extra>'
            )
          )
      }
    }
    
    # Layout configuration
    plot <- plot %>%
      layout(
        xaxis = list(
          title = "Year",
          gridcolor = '#E5E5E5',
          range = c(1970, input$year_control)
        ),
        yaxis = list(
          title = metric_label,
          gridcolor = '#E5E5E5',
          rangemode = "tozero"
        ),
        hovermode = "closest",
        legend = list(
          orientation = "v", 
          x = 1.02, 
          y = 1,
          bgcolor = 'rgba(255, 255, 255, 0.8)'
        ),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      ) %>%
      htmlwidgets::onRender(
        "function(el, x) {
          const defaultColor = '#BDBDBD';
          const originalColors = (x.data || []).map(function(trace) {
            if (!trace.customdata || !trace.customdata.length) {
              return defaultColor;
            }
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
          el.on('plotly_unhover', function() {
            applyColors(null);
          });
        }"
      )
    
    plot
  })
  
  output$emissionsInfo <- renderText({
    req(input$year_control)
    
    metric_col <- switch(input$metric,
                        "total" = "total_emissions",
                        "per_capita" = "emissions_per_capita",
                        "per_gdp" = "emissions_per_GDP",
                        "accumulated" = "accumulated_emissions")
    
    if ((input$metric == "total" || input$metric == "accumulated") &&
        !is.null(input$continents) && length(input$continents) > 0) {
      total <- continent_data %>%
        filter(
          continent %in% input$continents,
          year <= input$year_control
        ) %>%
        summarise(total = sum(get(metric_col), na.rm = TRUE)) %>%
        pull(total)
      
      paste0(
        "Total for selected regions through ", input$year_control, ":\n",
        format(round(total, 2), big.mark = ","), " MtCO2e"
      )
    } else if (is.null(input$continents) || length(input$continents) == 0) {
      paste0(
        "Selected metric: ", input$metric, "\n",
        "Showing values for chosen countries")
    } else {
      paste0(
        "Selected metric: ", input$metric, "\n",
        "Showing average values for selected continents"
      )
    }
  })
  
  # ==========================================================================
  # DIAGRAM 6: Choropleth Map
  # ==========================================================================
  
  output$choroplethMap <- renderLeaflet({
    
    # Select appropriate metric
    metric_col <- switch(input$map_metric,
                        "total" = "total_emissions_MtCO2e",
                        "per_capita" = "emissions_per_capita",
                        "per_gdp" = "emissions_per_GDP")
    
    metric_label <- switch(input$map_metric,
                          "total" = "MtCO2e",
                          "per_capita" = "tCO2e/person",
                          "per_gdp" = "kgCO2e/$")
    
    # Get world map data
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Get emissions data for selected year and ensure ISO codes align with map data
    country_emissions <- edgar %>%
      filter(year == input$map_year) %>%
      mutate(
        iso_match = country_code,
        iso_match = ifelse(
          is.na(iso_match) | iso_match == "" | nchar(iso_match) != 3,
          countrycode(country_clean, origin = "country.name", destination = "iso3c"),
          iso_match
        ),
        iso_match = toupper(trimws(iso_match))
      ) %>%
      select(iso_match, country_clean, metric_value = all_of(metric_col))
    
    emissions_by_iso <- country_emissions %>%
      filter(!is.na(iso_match)) %>%
      group_by(iso_match) %>%
      summarise(value_iso = sum(metric_value, na.rm = TRUE), .groups = "drop")
    
    emissions_by_name <- country_emissions %>%
      group_by(country_clean) %>%
      summarise(value_name = sum(metric_value, na.rm = TRUE), .groups = "drop")
    
    # Merge with world map using ISO codes (with name fallback)
    world_data <- world %>%
      left_join(emissions_by_iso, by = c("iso_a3" = "iso_match")) %>%
      left_join(emissions_by_name, by = c("name_long" = "country_clean")) %>%
      mutate(
        value = dplyr::coalesce(value_iso, value_name),
        value_display = ifelse(
          is.na(value),
          "No data",
          paste0(format(round(value, 2), big.mark = ","), " ", metric_label)
        ),
        tooltip = sprintf(
          "<strong>%s</strong><br/>Year: %s<br/>%s",
          name,
          input$map_year,
          value_display
        )
      ) %>%
      select(-value_iso, -value_name)
    
    # Create color palette (AFM hot style: gold → orange → red)
    pal <- colorNumeric(
      palette = c("#FFD700", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
      domain = world_data$value,
      na.color = "#D3D3D3"  # Light gray for missing data
    )
    
    # Create leaflet map
    leaflet(world_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor = ~pal(value),
        fillOpacity = 0.7,
        color = "#FFFFFF",  # White borders
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",  # Black border on hover
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(world_data$tooltip, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~value,
        title = paste("Emissions (", metric_label, ")<br>Year:", input$map_year),
        position = "bottomright",
        opacity = 0.7,
        labFormat = labelFormat(big.mark = ",")
      )
  })
  
  # ==========================================================================
  # DIAGRAM 7: Race Bar Chart - Top Companies
  # ==========================================================================
  
  output$raceBarChart <- renderPlotly({
    
    # Aggregate emissions per company and determine dominant commodity
    company_totals <- company_emissions %>%
      filter(year == input$race_year) %>%
      group_by(parent_entity) %>%
      summarise(emissions = sum(total_emissions_MtCO2e, na.rm = TRUE), .groups = "drop")
    total_companies <- nrow(company_totals)
    
    dominant_commodity <- company_emissions %>%
      filter(year == input$race_year) %>%
      group_by(parent_entity, commodity) %>%
      summarise(commodity_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE), .groups = "drop") %>%
      group_by(parent_entity) %>%
      slice_max(order_by = commodity_emissions, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(parent_entity, primary_commodity = commodity)
    
    year_data <- company_totals %>%
      left_join(dominant_commodity, by = "parent_entity") %>%
      arrange(desc(emissions)) %>%
      slice_head(n = 10)
    
    total_all <- sum(company_totals$emissions, na.rm = TRUE)
    top10_total <- sum(year_data$emissions, na.rm = TRUE)
    top10_share <- ifelse(total_all > 0, (top10_total / total_all) * 100, NA_real_)
    
    # Add percentage, rank, and factor ordering
    year_data <- year_data %>%
      mutate(
        percentage = ifelse(total_all > 0, (emissions / total_all) * 100, 0),
        rank = row_number(),
        primary_commodity = ifelse(is.na(primary_commodity), "Other / Mixed", primary_commodity),
        # Create display text
        display_text = if (input$show_percentage) {
          paste0(round(emissions, 1), " MtCO2e (", round(percentage, 1), "%)")
        } else {
          paste0(round(emissions, 1), " MtCO2e")
        }
      ) %>%
      mutate(parent_entity = factor(parent_entity, levels = parent_entity))
    
    # Define colors by commodity type
    commodity_colors <- c(
      "Oil & NGL" = "#8B0000",           # Dark red
      "Natural Gas" = "#DC143C",         # Crimson
      "Bituminous Coal" = "#2F4F4F",     # Dark slate gray
      "Anthracite Coal" = "#000000",     # Black
      "Thermal Coal" = "#696969",        # Dim gray
      "Lignite Coal" = "#808080",        # Gray
      "SubBituminous Coal" = "#A9A9A9",  # Dark gray
      "Metallurgical Coal" = "#555555",  # Very dark gray
      "Cement" = "#A9A9A9"               # Dark gray
    )
    
    # Get color for each company's primary commodity
    year_data$bar_color <- sapply(year_data$primary_commodity, function(c) {
      ifelse(c %in% names(commodity_colors), 
             commodity_colors[c], 
             "#8B4513")  # Brown for others
    })
    year_data$legend_group <- year_data$primary_commodity
    year_data$legend_label <- paste0(year_data$primary_commodity)
    
    # Create race bar chart
    plot <- plot_ly(
      data = year_data,
      y = ~parent_entity,
      x = ~emissions,
      type = 'bar',
      orientation = 'h',
      marker = list(
        color = ~bar_color,
        line = list(color = 'rgb(8,48,107)', width = 0.5)
      ),
      legendgroup = ~legend_group,
      showlegend = FALSE,
      text = ~display_text,
      textposition = 'outside',
      textfont = list(size = 11),
      hovertemplate = paste(
        '<b>%{y}</b><br>',
        'Commodity: %{customdata[1]}<br>',
        'Emissions: %{x:.1f} MtCO2e',
        '<extra></extra>'
      ),
      customdata = ~cbind(primary_commodity)
    )
    
    # Add invisible traces for legend entries
    for (commodity in names(commodity_colors)) {
      plot <- plot %>%
        add_trace(
          x = 0,
          y = "",
          type = 'bar',
          orientation = 'h',
          marker = list(color = commodity_colors[[commodity]]),
          legendgroup = commodity,
          showlegend = TRUE,
          name = commodity,
          hoverinfo = 'none',
          opacity = 0,
          inherit = FALSE
        )
    }
    
    # Optional rank annotations
    rank_annotations <- NULL
    if (input$show_rank) {
      rank_annotations <- lapply(seq_len(nrow(year_data)), function(i) {
        list(
          x = 0,
          y = as.character(year_data$parent_entity[i]),
          text = paste0("#", year_data$rank[i]),
          xanchor = 'right',
          xref = 'x',
          yref = 'y',
          showarrow = FALSE,
          font = list(size = 12, color = 'black', family = 'Arial Black')
        )
      })
    }
    share_annotation <- NULL
    if (!is.na(top10_share)) {
      share_annotation <- list(
        x = 1,
        y = 1.08,
        xref = 'paper',
        yref = 'paper',
        text = paste0("Top 10 share: ", round(top10_share, 1), "%"),
        showarrow = FALSE,
        font = list(size = 13, color = '#333'),
        align = 'right'
      )
    }
    annotations <- rank_annotations
    if (is.null(annotations)) annotations <- list()
    if (!is.null(share_annotation)) annotations <- c(annotations, list(share_annotation))
    
    # Layout configuration
    subtitle_html <- NULL
    if (!is.na(top10_share)) {
      subtitle_html <- paste0(
        "<span style='font-size:13px;color:#555;'>Top 10 emit ",
        format(round(top10_total, 1), big.mark = ","), " MtCO2e (",
        round(top10_share, 1), "%) out of ",
        format(round(total_all, 1), big.mark = ","), " MtCO2e total (",
        total_companies, " companies)</span>"
      )
    }
    plot <- plot %>%
      layout(
        title = list(
          text = paste0(
            "<b>Top 10 Emitting Companies - ", input$race_year, "</b>",
            if (!is.null(subtitle_html)) paste0("<br>", subtitle_html) else ""
          ),
          font = list(size = 18)
        ),
        xaxis = list(
          title = "Emissions (MtCO2e)",
          gridcolor = '#E5E5E5'
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 11),
          categoryorder = 'array',
          categoryarray = as.character(year_data$parent_entity),
          autorange = 'reversed'
        ),
        showlegend = FALSE,
        margin = list(l = 200, r = 100, t = 60, b = 60),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF',
        annotations = annotations
      )
    
    plot
  })
  
  output$totalEmissionsText <- renderText({
    
    # Calculate total for all companies
    total_all <- company_emissions %>%
      filter(year == input$race_year) %>%
      summarise(total = sum(total_emissions_MtCO2e, na.rm = TRUE)) %>%
      pull(total)
    
    # Calculate total for top 10
    top10_total <- company_emissions %>%
      filter(year == input$race_year) %>%
      group_by(parent_entity) %>%
      summarise(emissions = sum(total_emissions_MtCO2e, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(emissions)) %>%
      head(10) %>%
      summarise(total = sum(emissions)) %>%
      pull(total)
    
    # Calculate percentage
    percentage <- (top10_total / total_all) * 100
    
    # Count total companies
    total_companies <- company_emissions %>%
      filter(year == input$race_year) %>%
      distinct(parent_entity) %>%
      nrow()
    
    paste0(
      "Year: ", input$race_year, "\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "Total companies in dataset: ", total_companies, "\n\n",
      "All companies emissions: ", format(round(total_all, 2), big.mark = ","), " MtCO2e\n",
      "Top 10 emissions: ", format(round(top10_total, 2), big.mark = ","), " MtCO2e\n\n",
      "Top 10 represent: ", round(percentage, 1), "% of total emissions"
    )
  })
}

# ==============================================================================
# RUN THE APPLICATION
# ==============================================================================

shinyApp(ui = ui, server = server)
