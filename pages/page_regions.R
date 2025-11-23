# pages/page_regions.R

mod_page_regions_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Global Emissions by Continents and Countries"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("Select Regions"),
                # Note: Choices will be populated/updated if needed, but for now we hardcode common ones
                # or rely on the server to update if we want dynamic choices.
                # For simplicity in this refactor, we'll keep the static list or rely on the diagram module's data if we want to extract it.
                # However, to keep the page pure, we should ideally define choices here.
                # Let's assume standard continents.
                selectInput(ns("continents"), "Continents:",
                    choices = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                    selected = c("Europe", "Asia"), multiple = TRUE
                ),
                selectInput(ns("countries"), "Countries (optional):",
                    choices = NULL, # Will be updated by server if we want dynamic, or we can load data here.
                    # For now, let's leave it empty and let the user type or populate it if we load data.
                    # To properly populate this, we might need to read the data in this module or pass it.
                    # For this step, I will initialize it as NULL and we can refine if needed.
                    selected = NULL, multiple = TRUE
                ),
                hr(),
                h4("Metric"),
                radioButtons(ns("metric"), NULL,
                    choices = c(
                        "Total Emissions" = "total",
                        "Per Capita" = "per_capita",
                        "Per GDP" = "per_gdp",
                        "Total Accumulated" = "accumulated"
                    ),
                    selected = "total"
                ),
                hr(),
                h4("Time Control"),
                sliderInput(ns("year_control"), "Show data through year:",
                    min = 1970, max = 2023, value = 2023, step = 1, sep = "",
                    animate = animationOptions(interval = 500, loop = FALSE)
                ),
                helpText("Click play to animate changes year by year"),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'per_gdp'", ns("metric")),
                    helpText("Note: Per GDP data only available from 1990 onwards")
                )
            ),
            mainPanel(
                width = 9,
                mod_emissions_by_region_ui(ns("emissions_plot"))
            )
        )
    )
}

mod_page_regions_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # We need to populate countries choices.
        # To avoid reading data multiple times, we could read it here or in global scope.
        # For now, let's read the country list from the cleaned data to populate the selector.
        # This duplicates the read but ensures the UI works.
        data_path <- "data/data_cleaned/GHG_total_gdp_capita.csv"
        if (file.exists(data_path)) {
            df <- read.csv(data_path, stringsAsFactors = FALSE)
            countries <- sort(unique(df$Country))
            updateSelectInput(session, "countries", choices = countries)
        }

        # Call the diagram module
        mod_emissions_by_region_server(
            id = "emissions_plot",
            continents = reactive(input$continents),
            countries = reactive(input$countries),
            metric = reactive(input$metric),
            year_control = reactive(input$year_control)
        )
    })
}
