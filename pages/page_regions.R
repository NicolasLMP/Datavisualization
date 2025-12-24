# pages/page_regions.R

mod_page_regions_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("What are the emissions of the continents and countries?"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # Custom CSS to style the Play Button and Slider in Navy/Okabe Blue
            tags$head(
              tags$style(HTML(sprintf("
      /* Style the slider bar and handle */
      .js-irs-0 .irs-bar { background: #1D3557; border-top: 1px solid #1D3557; border-bottom: 1px solid #1D3557; }
      .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-single { background: #1D3557; }
      .js-irs-0 .irs-handle { border: 1px solid #1D3557; background-color: white; }
      
      /* Style the Play Button */
      .slider-animate-container .slider-animate-button {
        color: #1D3557 !important;
        font-size: 24px;
        transition: transform 0.2s ease-in-out;
      }
      .slider-animate-container .slider-animate-button:hover {
        color: #0072B2 !important; /* Okabe Blue on hover */
        transform: scale(1.1);
      }
      
      /* Nicer help text grouping */
      .help-block-container { 
        display: flex; 
        align-items: center; 
        margin-top: 10px; 
        background: #f0f7fb; 
        padding: 8px; 
        border-radius: 6px;
      }
    ")))
            ),
            
            selectInput(ns("continents"), "Select continents:",
                        choices = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                        selected = c("Europe", "Asia"), multiple = TRUE
            ),
            selectInput(ns("countries"), "Select countries:",
                        choices = NULL,
                        selected = NULL, multiple = TRUE
            ),
            hr(),
            radioButtons(ns("metric"), "Select metric:",
                         choices = c(
                           "Total Emissions" = "total",
                           "Per Capita" = "per_capita",
                           "Per GDP" = "per_gdp",
                           "Total Accumulated" = "accumulated"
                         ),
                         selected = "total"
            ),
            hr(),
            sliderInput(ns("year_control"), "Watch the evolution over the years:",
                        min = 1970, max = 2023, value = 2023, step = 1, sep = "",
                        animate = animationOptions(interval = 500, loop = FALSE)
            ),
            
            # Styled Tip Section
            tags$div(class = "help-block-container",
                     tags$span(icon("circle-info"), style = "margin-right: 10px; font-size: 1.2em;"),
                     helpText("Tip: Click play to animate changes year by year.", style = "margin: 0;")
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'per_gdp'", ns("metric")),
              tags$div(class = "help-block-container",
                       tags$span(icon("circle-info"), style = "margin-right: 10px; font-size: 1.2em;"),
                       helpText("Note: Per GDP data is only available from 1990 onwards.", style = "margin: 0;")
              )
            )
          ),
            mainPanel(
                width = 9,
                mod_emissions_by_region_ui(ns("emissions_plot")),
                hr(),
                wellPanel(
                  style = "background-color: #fcfcfc; border-left: 5px solid #0072B2;",
                  h4("Research questions", style = "font-weight: bold;"),
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
    })
}
