# pages/page_ai_analysis.R

# Load data locally to populate choices
local_data <- read.csv("data/data_cleaned/GHG_by_sector_and_country.csv", stringsAsFactors = FALSE)
available_countries <- sort(unique(local_data$Country))
# Ensure GLOBAL TOTAL is available and prominent
available_countries <- c("GLOBAL TOTAL", available_countries[available_countries != "GLOBAL TOTAL"])

mod_page_ai_analysis_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("AI Generated Analysis"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Control Panel"),

                # 1. Year Selection
                sliderInput(ns("year_input"),
                    "Select Year:",
                    min = min(local_data$year),
                    max = max(local_data$year),
                    value = 2022,
                    step = 1,
                    sep = ""
                ),
                hr(),

                # 2. Country Selection
                selectizeInput(ns("country_input"),
                    "Select Economies to Compare:",
                    choices = available_countries,
                    selected = c("China", "United States", "EU27", "GLOBAL TOTAL"),
                    multiple = TRUE,
                    options = list(maxItems = 6)
                ), # Limit to avoid overcrowding

                hr(),
                h4("About this Analysis"),
                helpText("This radar chart analyzes the 'Pollution Fingerprint' of diverse economies."),
                tags$ul(
                    tags$li("Axes: The 8 major emission sectors."),
                    tags$li("Scale: Percentage (%) share of that sector in the total."),
                    tags$li("Goal: Compare structural differences.")
                ),
                hr(),
                helpText(icon("search"), " Experiment by comparing developed vs. developing nations.")
            ),
            mainPanel(
                width = 9,
                # Pass the plot module UI
                mod_ai_analysis_ui(ns("ai_plot")),
                hr(),
                wellPanel(
                    style = "background-color: #f8f9fa;",
                    h4("AI Insights", style = "color: #2c3e50; font-weight: bold;"),
                    tags$p("This interactive chart reveals the structural DNA of each economy at a glance."),
                    tags$ul(
                        tags$li(tags$strong("China:"), " Typically heavy in Power & Industry."),
                        tags$li(tags$strong("USA:"), " Often shows a distinct Transport spike."),
                        tags$li(tags$strong("Developing Nations:"), " May show larger Agriculture shares.")
                    ),
                    tags$p("Use the controls on the left to travel through time or compare different regions.")
                )
            )
        )
    )
}

mod_page_ai_analysis_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # call module with reactive inputs
        mod_ai_analysis_server("ai_plot",
            selected_year = reactive(input$year_input),
            selected_countries = reactive(input$country_input)
        )
    })
}
