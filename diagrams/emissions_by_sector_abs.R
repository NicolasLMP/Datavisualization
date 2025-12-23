library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)
library(plotly)
library(purrr)
library(khroma)

# Load data
data <- read.csv("data/data_cleaned/GHG_by_sector_and_country.csv", stringsAsFactors = FALSE)
# data$Continent <- countrycode(data$Country, origin = "country.name", destination = "continent")

# UI function
mod_emissions_by_sectors_abs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Column for the Main Plot (75% width: 9 out of 12)
      column(
        width = 8,
        plotlyOutput(ns("plot"), height = "400px")
      ),
      
      # Column for the Donut Chart (25% width: 3 out of 12)
      column(
        width = 4,
        # Note: The donut chart's title will be defined dynamically in the server
        plotlyOutput(ns("donut"), height = "400px")
      )
    )
  )
}

# Server function

mod_emissions_by_sectors_abs_server <- function(id, sectors, countries) {
  moduleServer(id, function(input, output, session) {
    
    last_hover_data <- reactiveVal(NULL)
    
    selected_data <- reactive({
      req(sectors())
      req(countries())
      data %>%
        filter(Sector %in% sectors(), Country %in% countries())
    })
    
    output$plot <- renderPlotly({
      df <- selected_data()
      req(nrow(df) > 0)
      
      hover_text <- pmap_chr(df, function(Sector, Country, year, CO2e, CO2, CH4, N2O, F_gases, ...) {
        total <- as.numeric(CO2e)
        co2 <- as.numeric(CO2)
        ch4 <- as.numeric(CH4)
        n2o <- as.numeric(N2O)
        fg <- as.numeric(F_gases)
        
        paste0(
          "<b>", Sector, "</b><br>",
          "<b>", year, "</b><br>",
          "CO₂e: ", round(total, 2), " Mt<br>"
        )
      })
      
      # sector_colors <- c(
      #   "Agriculture" = "#4DC3B3",
      #   "Buildings" = "#F28E5C",
      #   "Fuel Exploitation" = "#6574B9",
      #   "Industrial Combustion" = "#D970C4",
      #   "Power Industry" = "#A7CE47",
      #   "Processes" = "#F1D54A",
      #   "Transport" = "#E9BE86",
      #   "Waste" = "#B5B5B5"
      # )

      
      # sector_colors <- c(
      #   "Agriculture"           = "#629E92", # Muted Sage Green
      #   "Buildings"             = "#E2A03F", # Ochre/Gold
      #   "Fuel Exploitation"     = "#5790AB", # Steel Blue
      #   "Industrial Combustion" = "#A35C7A", # Dusty Rose/Mauve
      #   "Power Industry"        = "#004E82", # Deep Navy
      #   "Processes"             = "#C9BB6A", # Soft Straw
      #   "Transport"             = "#B6512C", # Terracotta
      #   "Waste"                 = "#7A7A7A"  # Cool Grey
      # )
      # 
      # sector_colors <- sector_colors[names(sector_colors) %in% unique(df$Sector)]
      
      palette_okabe <- color("okabe ito")(8)
      
      # Map the categories to the palette dynamically
      sector_names <- c(
        "Waste", "Agriculture", "Buildings", "Fuel Exploitation", 
        "Industrial Combustion", "Power Industry", "Processes", 
        "Transport"
      )
      sector_colors <- setNames(as.character(palette_okabe[1:length(sector_names)]), sector_names)
      
      plot_ly(
        df, x = ~year, y = ~CO2e, color = ~Sector, colors = sector_colors,
        type = 'scatter', mode = 'lines+markers', text = hover_text,
        hoverinfo = 'text', source = session$ns("abs")
      ) %>%
        config(modeBarButtonsToRemove = c("select2d", "lasso2d")) %>% # Removes selection tools
        layout(
          title = paste0("What are the emission per sector in ", 
                         paste(unique(df$Country), 
                               collapse = ", "), "?"),
          xaxis = list(title = "Year",
                       fixedrange = TRUE),
          yaxis = list(title = "Emissions (Mt CO₂e)", 
                       rangemode = "tozero",
                       zerolinecolor="#EBEBEB",
                       fixedrange = TRUE),
          dragmode = FALSE
        )
    })
    
    output$donut <- renderPlotly({
      df <- selected_data()
      req(nrow(df) > 0)
      
      hover <- event_data("plotly_hover", source = session$ns("abs"))
      
      if (!is.null(hover)) {
        sector_levels <- unique(df$Sector)
        hovered_sector <- sector_levels[ hover$curveNumber + 1 ]
        
        row <- df %>% filter(year == hover$x, Sector == hovered_sector)
        
        if (nrow(row) == 1) {
          last_hover_data(list(
            row = row,
            sector = hovered_sector,
            year = hover$x
          ))
        }
        
      } else if (is.null(last_hover_data())) {
        latest_year <- max(df$year, na.rm = TRUE)
        row <- df %>%
          filter(year == latest_year) %>%
          arrange(Sector) %>%
          slice(1)
        
        if (nrow(row) == 0) return(NULL) # Safety check
        
        hovered_sector <- row$Sector
        hover_year <- latest_year
        
        last_hover_data(list(
          row = row,
          sector = hovered_sector,
          year = hover_year
        ))
        
      }
      current_data <- last_hover_data()
      req(current_data)
      
      row <- current_data$row
      hovered_sector <- current_data$sector
      hover_year <- current_data$year
      
      
      # Gas breakdown for donut
      # 1. Prepare Gas Data
      df_gas <- data.frame(
        gas = c("CO2", "CH4", "N2O", "F-gases"),
        value = c(row$CO2, row$CH4, row$N2O, row$F_gases)
      ) %>% 
        filter(!is.na(value) & value > 0) %>%
        mutate(pct = value / sum(value))
      
      # 2. Grouping Logic: Only group if there is MORE than one small gas
      small_gases <- df_gas %>% filter(pct < 0.05)
      main_gases <- df_gas %>% filter(pct >= 0.05)
      
      if(nrow(small_gases) > 1) {
        # Combine multiple small gases into "Others"
        others_label <- paste0("Others (", paste(small_gases$gas, collapse = ", "), ")")
        df_gas <- bind_rows(
          main_gases,
          data.frame(gas = others_label, value = sum(small_gases$value), pct = sum(small_gases$pct))
        )
      } else {
        # If only one gas is small (or none), just keep the original data frame
        # No changes needed to df_gas
      }
      
      # 3. Dynamic color mapping
      # gas_colors_map <- c("CO2" = "#2A9D8F", "CH4" = "#E9C46A", "N2O" = "#F4A261", "F-gases" = "#C34A36")
      
      # gas_colors_map <- c(
      #   "CO2"     = "#2E5A88", # Deep Atmospheric Blue
      #   "CH4"     = "#D99527", # Soft Amber
      #   "N2O"     = "#4E8474", # Seafoam/Deep Teal
      #   "F-gases" = "#956385", # Muted Orchid
      #   "Others"  = "#B2B2B2"  # Silver Grey
      # )
      paul_tol_bright <- as.character(color("bright")(4))
      
      # Paul Tol's recommended neutral grey for qualitative schemes
      paul_tol_grey <- "#BBBBBB"
      
      # Create the fixed mapping
      gas_colors_fixed <- c(
        "CO2"     = paul_tol_bright[1],
        "CH4"     = paul_tol_bright[2],
        "N2O"     = paul_tol_bright[3],
        "F-gases" = paul_tol_bright[4],
        "Others"  = paul_tol_grey
      )
      
      # current_colors <- sapply(df_gas$gas, function(g) {
      #   if (grepl("Others", g)) return("#D3D3D3") # Grey for combined Others
      #   return(gas_colors_map[g])
      # })
      current_colors <- sapply(df_gas$gas, function(g) {
        if (grepl("Others", g)) {
          return(unname(gas_colors_fixed["Others"]))
        } else {
          # Return the specific color for CO2, CH4, etc.
          return(unname(gas_colors_fixed[g]))
        }
      })
      
      plot_ly(
        df_gas, 
        labels = ~gas, 
        values = ~value, 
        type = "pie", 
        hole = 0.6,
        # textinfo = "label+percent", 
        textposition = "outside",
        hoverinfo = "none",
        texttemplate = "<b>%{label}</b><br>%{percent:.2%}",
        marker = list(colors = unname(current_colors)),
        # domain = list(x = c(0, 1), y = c(0, 1)) # Keeps donut centered
        domain = list(x = c(0.2, 0.8), y = c(0.2, 0.8))
      ) %>%
        layout(
          title = list(
            text = paste0("What is the gas composition of<br>", 
                          paste(unique(df$Country), collapse = ", "), "'s <br>", 
                          hovered_sector, " in ", hover_year, "?"),
            #font = list(size = 14),
            # --- ALIGN TITLE TO TOP ---
            y = 1,
            yanchor = "top",
            pad = list(t = 12) # Optional: slight padding from the very edge
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",   
            x = 0.5,             
            xanchor = "center",  
            y = -0.1,            
            yanchor = "top"      
          ),
          # Ensure top margin is large enough to show the title clearly
          margin = list(t = 70, b = 80, l = 30, r = 30)
        )
    })
  })
}