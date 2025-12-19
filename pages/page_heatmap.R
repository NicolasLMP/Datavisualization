mod_page_heatmap_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Map of the green house gas emissions "),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Time Controls"),
                sliderInput(ns("map_year"), "Year:",
                    min = 1970, max = 2023, value = 2023,
                    step = 1, sep = ""
                ),
                hr(),
                h4("Metric"),
                radioButtons(ns("map_metric"), "Select metric:",
                    choices = c(
                        "Total Emissions" = "total",
                        "Per Capita" = "per_capita",
                        "Per GDP" = "per_gdp",
                        "Total Accumulated" = "accumulated"
                    ),
                    selected = "total"
                ),
                hr(),
                helpText(
                    style = "margin-top: 15px; font-size: 0.9em; color: #7f8c8d;",
                    icon("info-circle"),
                    " Use the slider to explore specific years."
                ),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'per_gdp'", ns("map_metric")),
                    helpText("Note: Per GDP data is available starting from 1990.")
                )
            ),
            mainPanel(
                width = 9,
                mod_global_heatmap_ui(ns("heatmap_plot")),
                hr(),
                wellPanel(
                    style = "background-color: #f8f9fa;",
                    h4("Research questions", style = "color: #2c3e50;"),
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
