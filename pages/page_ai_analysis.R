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
                    h4("Research Question", style = "color: #2c3e50; font-weight: bold;"),
                    tags$p("This specific graph was designed to answer the following research question:", style = "font-style: italic; color: #555;"),
                    tags$blockquote(
                        style = "border-left: 5px solid #0072B2; margin: 20px 0; padding: 10px 20px; background: #f1f1f1; font-size: 1.1em;",
                        tags$strong("Do nations with similar total emission levels share the same underlying economic structure?")
                    ),
                    tags$p("The visualization reveals the answer is often ", tags$strong("no.")),
                    tags$p("By comparing the 'shapes' of high-emitting nations, the dashboard demonstrates distinct structural realities (e.g., China's 'Industrial Diamond' shape vs. the USA's 'Transport-Heavy' shape), implying that decarbonization strategies must be tailored to these fingerprints.")
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
