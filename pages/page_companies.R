# pages/page_companies.R

mod_page_companies_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Top 10 Emitting Companies"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Time Controls"),
                # We need to know the min/max year.
                # Reading data here to set limits correctly or using safe defaults.
                # Using 1970-2023 as safe default based on other files.
                sliderInput(ns("race_year"), "Year:",
                    min = 1854, max = 2022, # Approximate range for companies data
                    value = 2022,
                    step = 1, sep = "",
                    animate = animationOptions(interval = 500, loop = FALSE)
                ),
                hr(),
                h4("Display Options"),
                checkboxInput(ns("show_percentage"), "Show percentage of total", value = TRUE),
                checkboxInput(ns("show_rank"), "Show rank numbers", value = TRUE),
                hr(),
                helpText("Click play to see rankings change over time")
            ),
            mainPanel(
                width = 9,
                mod_top_companies_ui(ns("companies_plot"))
            )
        )
    )
}

mod_page_companies_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Update slider range based on actual data
        data_path <- "data/data_cleaned/GHG_by_sector_and_companies.csv"
        if (file.exists(data_path)) {
            df <- read.csv(data_path, stringsAsFactors = FALSE)
            min_year <- min(df$year, na.rm = TRUE)
            max_year <- max(df$year, na.rm = TRUE)
            updateSliderInput(session, "race_year", min = min_year, max = max_year, value = max_year)
        }

        # Call the diagram module
        mod_top_companies_server("companies_plot",
            race_year = reactive(input$race_year),
            show_percentage = reactive(input$show_percentage),
            show_rank = reactive(input$show_rank)
        )
    })
}
