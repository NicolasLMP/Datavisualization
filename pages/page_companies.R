mod_page_companies_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("How much do top companies emit?"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Information"),
                helpText("This page visualizes the global 174 top emitting companies over time.")
            ),
            mainPanel(
                width = 9,
                #h4("Top Companies vs World Emissions"),
                mod_company_vs_world_ui(ns("companies_vs_world")),
                mod_top_companies_ui(ns("companies_plot")),
                hr(),
                wellPanel(
                  style = "background-color: #fcfcfc; border-left: 5px solid #0072B2;",
                  h4("Research Questions", style = "color: #0072B2; font-weight: bold;"),
                  tags$ul(
                    tags$li("How do global emissions develop over time?"),
                    tags$li("How do the total emissions of top-emitting companies trend over time?"),
                    tags$li("What is the relationship between corporate emissions and world total emissions?"),
                    tags$li("How do the top 10 emitters compare to the global total?"),
                    tags$li("What percentage of total emissions do the top 10 companies represent?"),
                    tags$li("How have company rankings changed over the years?"),
                    tags$li("What is the concentration of emissions among the world's major polluters?")
                  )
                ),
                tags$div(style = "height: 30px;")
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
