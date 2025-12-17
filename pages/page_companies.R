# pages/page_companies.R

mod_page_companies_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Most emitting companies"),
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
                helpText("Click play to see rankings change over time")
            ),
            mainPanel(
                width = 9,
                mod_top_companies_ui(ns("companies_plot")),
                hr(),
                wellPanel(
                    style = "background-color: #f8f9fa;",
                    h4("Research questions", style = "color: #2c3e50;"),
                    tags$ul(
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
        # Update slider range based on actual data
        data_path <- "data/data_cleaned/GHG_by_sector_and_companies.csv"
        if (file.exists(data_path)) {
            df <- read.csv(data_path, stringsAsFactors = FALSE)
            min_year <- min(df$year, na.rm = TRUE)
            max_year <- max(df$year, na.rm = TRUE)
            updateSliderInput(session, "race_year", min = min_year, max = max_year, value = max_year)
        }

        # Call the diagram module with fixed display options
        mod_top_companies_server("companies_plot",
            race_year = reactive(input$race_year),
            show_percentage = reactive(TRUE), # Always show percentage
            show_rank = reactive(FALSE) # Never show rank numbers
        )
    })
}
