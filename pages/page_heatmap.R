# pages/page_heatmap.R

mod_page_heatmap_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Global Emissions Choropleth Map"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Time Controls"),
                sliderInput(ns("map_year"), "Year:",
                    min = 1970, max = 2023, value = 2023,
                    step = 1, sep = "", animate = animationOptions(interval = 1000, loop = TRUE)
                ),
                hr(),
                h4("Metric"),
                radioButtons(ns("map_metric"), NULL,
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
                    condition = sprintf("input['%s'] == 'per_gdp'", ns("map_metric")),
                    helpText("Note: Per GDP data only available from 1990 onwards")
                )
            ),
            mainPanel(
                width = 9,
                mod_global_heatmap_ui(ns("heatmap_plot"))
            )
        )
    )
}

mod_page_heatmap_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Call the diagram module
        mod_global_heatmap_server("heatmap_plot",
            map_year = reactive(input$map_year),
            map_metric = reactive(input$map_metric)
        )
    })
}
