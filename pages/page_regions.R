# pages/page_regions.R

mod_page_regions_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Global emissions by continents and countries"),

        # Comparison mode checkbox
        checkboxInput(ns("comparison_mode"), "Enable side-by-side comparison", value = FALSE),
        conditionalPanel(
            condition = sprintf("!input['%s']", ns("comparison_mode")),
            # Single plot mode
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    h4("Select Regions"),
                    selectInput(ns("continents"), "Continents:",
                        choices = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                        selected = c("Europe", "Asia"), multiple = TRUE
                    ),
                    selectInput(ns("countries"), "Countries :",
                        choices = NULL,
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
                    mod_emissions_by_region_ui(ns("emissions_plot")),
                    hr(),
                    wellPanel(
                        style = "background-color: #f8f9fa;",
                        h4("Research questions", style = "color: #2c3e50;"),
                        tags$ul(
                            tags$li("How do GHG emissions evolve over time for different continents/countries?"),
                            tags$li("Who are the largest emitters in absolute value vs per capita vs per GDP?"),
                            tags$li("How do cumulative emissions compare between countries?"),
                            tags$li("What are the trends of increase or reduction in emissions?"),
                            tags$li("Which regions/countries are decoupling emissions from economic growth?")
                        )
                    )
                )
            )
        ),
        conditionalPanel(
            condition = sprintf("input['%s']", ns("comparison_mode")),
            # Side-by-side comparison mode
            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4("Plot 1 Settings"),
                        selectInput(ns("continents1"), "Continents:",
                            choices = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                            selected = c("Europe"), multiple = TRUE
                        ),
                        selectInput(ns("countries1"), "Countries :",
                            choices = NULL,
                            selected = NULL, multiple = TRUE
                        ),
                        radioButtons(ns("metric1"), "Metric:",
                            choices = c(
                                "Total Emissions" = "total",
                                "Per Capita" = "per_capita",
                                "Per GDP" = "per_gdp",
                                "Total Accumulated" = "accumulated"
                            ),
                            selected = "total"
                        )
                    ),
                    mod_emissions_by_region_ui(ns("emissions_plot1"))
                ),
                column(
                    6,
                    wellPanel(
                        h4("Plot 2 Settings"),
                        selectInput(ns("continents2"), "Continents:",
                            choices = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                            selected = c("Asia"), multiple = TRUE
                        ),
                        selectInput(ns("countries2"), "Countries :",
                            choices = NULL,
                            selected = NULL, multiple = TRUE
                        ),
                        radioButtons(ns("metric2"), "Metric:",
                            choices = c(
                                "Total Emissions" = "total",
                                "Per Capita" = "per_capita",
                                "Per GDP" = "per_gdp",
                                "Total Accumulated" = "accumulated"
                            ),
                            selected = "per_capita"
                        )
                    ),
                    mod_emissions_by_region_ui(ns("emissions_plot2"))
                )
            ),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        h4("Shared Time Control"),
                        sliderInput(ns("year_control_compare"), "Show data through year:",
                            min = 1970, max = 2023, value = 2023, step = 1, sep = "",
                            animate = animationOptions(interval = 500, loop = FALSE)
                        ),
                        helpText("This time control applies to both plots")
                    )
                )
            )
        )
    )
}

mod_page_regions_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Populate countries choices
        data_path <- "data/data_cleaned/GHG_total_gdp_capita.csv"
        if (file.exists(data_path)) {
            df <- read.csv(data_path, stringsAsFactors = FALSE)
            countries <- sort(unique(df$Country))
            updateSelectInput(session, "countries", choices = countries)
            updateSelectInput(session, "countries1", choices = countries)
            updateSelectInput(session, "countries2", choices = countries)
        }

        # Single plot mode
        mod_emissions_by_region_server(
            id = "emissions_plot",
            continents = reactive(input$continents),
            countries = reactive(input$countries),
            metric = reactive(input$metric),
            year_control = reactive(input$year_control)
        )

        # Comparison mode - Plot 1
        mod_emissions_by_region_server(
            id = "emissions_plot1",
            continents = reactive(input$continents1),
            countries = reactive(input$countries1),
            metric = reactive(input$metric1),
            year_control = reactive(input$year_control_compare)
        )

        # Comparison mode - Plot 2
        mod_emissions_by_region_server(
            id = "emissions_plot2",
            continents = reactive(input$continents2),
            countries = reactive(input$countries2),
            metric = reactive(input$metric2),
            year_control = reactive(input$year_control_compare)
        )
    })
}
