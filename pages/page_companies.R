mod_page_companies_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Most emitting companies"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Information"),
                helpText("This page visualizes the top emitting companies."),
                hr(),
                helpText("The animation on the right shows the evolution of the top 10 emitters over time (1970-2022)."),
                hr(),
                helpText("Note: The 'Vs World' comparison below uses data from the latest available year (2022).")
            ),
            mainPanel(
                width = 9,
                h4("Top Companies vs World Emissions"),
                mod_company_vs_world_ui(ns("companies_vs_world")),
                mod_top_companies_ui(ns("companies_plot")),
                hr(),
                wellPanel(
                    style = "background-color: #f8f9fa;",
                    h4("Research questions", style = "color: #2c3e50;"),
                    tags$ul(
                        tags$li("How do the world emissions develop over time?"),
                        tags$li("How do the summed up emissions of the top emitting companies develop over time?"),
                        tags$li("How do the summed up emissions of the top emitting companies develop over time in relation to the worlds emission?"),
                        tags$li("How do the top 10 emitters compare to the total?"),
                        tags$li("What percentage of total emissions do the top 10 represent?"),
                        tags$li("How have the rankings changed over time?"),
                        tags$li("What is the concentration of emissions among major polluters?")
                    )
                )
            )
        )
    )
}

mod_page_companies_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Call the diagram module
        # race_year is ignored by the GIF module, but we pass a dummy value
        mod_top_companies_server("companies_plot",
            race_year = reactive(2022),
            show_percentage = reactive(TRUE),
            show_rank = reactive(FALSE)
        )

        # Pass fixed year to the Vs World chart
        mod_company_vs_world_server("companies_vs_world",
            race_year = reactive(2022)
        )
    })
}
