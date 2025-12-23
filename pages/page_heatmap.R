mod_page_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("How do emissions vary across the globe?"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$head(
          tags$style(HTML(paste0("
            /* 1. Slider Bar and Selection Range */
            .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to {
              background: #0072B2 !important;
              border-top: 1px solid #0072B2 !important;
              border-bottom: 1px solid #0072B2 !important;
            }
            
            /* 2. THE HANDLE - Only Border Blue */
            .irs-handle { 
              border: 2px solid #0072B2 !important;
              background-color: white !important;
              box-shadow: none !important;
            }

            /* 3. THE ARROW (The triangle pointer below the year bubble) */
            .irs-single:after {
              border-top-color: #0072B2 !important;
            }
            
            /* 4. Radio Button Selection Color */
            input[type='radio']:checked {
              accent-color: #0072B2;
            }
            
            .sidebar-header { font-weight: bold; margin-bottom: 10px; }
          ")))
        ),
        
        sliderInput(ns("map_year"), "Select year:",
                    min = 1970, max = 2023, value = 2023,
                    step = 1, sep = ""
        ),
        
        radioButtons(ns("map_metric"), "Select metric:",
                     choices = c(
                       "Total Emissions" = "total",
                       "Per Capita" = "per_capita",
                       "Per GDP" = "per_gdp"
                     ),
                     selected = "total"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'per_gdp'", ns("map_metric")),
          tags$p(
            style = "font-size: 0.85em; color: #7f8c8d; font-style: italic; margin-top: 10px;",
            "Note: Per GDP data is available starting from 1990."
          )
        )
      ),
      mainPanel(
        width = 9,
        mod_global_heatmap_ui(ns("heatmap_plot")),
        # Spacer instead of hr()
        tags$div(style = "height: 30px;"), 
        wellPanel(
          style = "background-color: #fcfcfc; border-left: 5px solid #0072B2;",
          h4("Research questions", style = "font-weight: bold;"),
          tags$ul(
            tags$li("How have emission intensities changed geographically from 1970 to 2024?"),
            tags$li("Which regions show the most dramatic increases or decreases?"),
            tags$li("How does emission per GDP vary across countries in a given year?"),
            tags$li("Are there spatial clusters of high/low emitters?")
          )
        )
      )
    )
  )
}

mod_page_heatmap_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Call the heatmap plot module
        mod_global_heatmap_server("heatmap_plot",
            map_year = reactive(input$map_year),
            map_metric = reactive(input$map_metric)
        )

        # Update year slider based on metric
        observeEvent(input$map_metric, {
            if (input$map_metric %in% c("per_capita", "per_gdp")) {
                updateSliderInput(session, "map_year", min = 1990, value = max(1990, input$map_year))
            } else {
                updateSliderInput(session, "map_year", min = 1970)
            }
        })
    })
}
