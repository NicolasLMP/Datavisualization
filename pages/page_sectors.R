# pages/page_sectors.R

mod_page_sectors_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Emissions by sectors and country"),
        
        # Comparison mode checkbox
        checkboxInput(ns("comparison_mode"), "Enable side-by-side comparison", value = FALSE),
        conditionalPanel(
          condition = sprintf("!input['%s']", ns("comparison_mode")),
          
          # Single plot mode
          sidebarLayout(
              sidebarPanel(
                  width = 3,
                  h4("Select Sectors"),
                  checkboxGroupInput(
                    ns("sectors"), "Select sectors:",
                    choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                                "Industrial Combustion", "Power Industry",
                                "Processes", "Transport", "Waste"),
                    selected = c("Industrial Combustion", "Power Industry")
                  ),
                  h4("Select Regions"),
                  
                  selectInput(ns("countries"), "Choose Country:", choices = NULL),
                  
                  h4("Relative Plot Type"),
                  radioButtons(
                    ns("rel_plot_type"), "Select relative plot type:",
                    choices = c(
                      "Relative (%)" = "rel",
                      "Relative Stacked (%)" = "stacked"
                    ),
                    inline = TRUE
                  )
              ),
              mainPanel(
                  width = 9,
                  mod_emissions_by_sectors_abs_ui(ns("emissions_by_sectors_abs_plot")),
                  
                  # Show relative plot depending on toggle
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'rel'", ns("rel_plot_type")),
                    mod_emissions_by_sectors_rel_ui(ns("emissions_by_sectors_rel_plot"))
                  ),
                  
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'stacked'", ns("rel_plot_type")),
                    mod_emissions_by_sectors_rel_stacked_ui(ns("emissions_by_sectors_rel_stacked_plot"))
                  ),
                  
                  hr(),
                  wellPanel(
                    style = "background-color: #f8f9fa;",
                    h4("Research questions", style = "color: #2c3e50;"),
                    tags$ul(
                      tags$li("How are the sectors in relation to another?"),
                      tags$li("What sectors are improving?"),
                      tags$li("What sectors are very influential?"),
                      tags$li("How do countries compare to another?"),
                      tags$li("What are the emissions of the sectors in each country?"),
                      tags$li("How is the gas distribution on a sector for a year?"),
                      tags$li("How does the gas distribution on a sector develop over time?")
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
              2,
              wellPanel(
                h4("Select Sectors"),
                checkboxGroupInput(
                  ns("sectors1"), "Select sectors:",
                  choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                              "Industrial Combustion", "Power Industry",
                              "Processes", "Transport", "Waste"),
                  selected = c("Industrial Combustion", "Power Industry")
                ),
                h4("Relative Plot Type"),
                radioButtons(
                  ns("rel_plot_type1"), "Select relative plot type:",
                  choices = c(
                    "Relative (%)" = "rel",
                    "Relative Stacked (%)" = "stacked"
                  ),
                  inline = TRUE
                )
              ),
            ),
            column(
              5,
              wellPanel(
                h4("Select Sectors"),
                selectInput(ns("countries1"), "Choose Country:", choices = NULL),
              ),
            ),
            column(
              5,
              wellPanel(
                h4("Select Sectors"),
                selectInput(ns("countries2"), "Choose Country:", choices = NULL),
                
              ),
            ),
          ),
          fluidRow(
            column(
              6,
              mod_emissions_by_sectors_abs_ui(ns("emissions_by_sectors_abs_plot1")),
              # Show relative plot depending on toggle
              conditionalPanel(
                condition = sprintf("input['%s'] == 'rel'", ns("rel_plot_type1")),
                mod_emissions_by_sectors_rel_ui(ns("emissions_by_sectors_rel_plot1"))
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'stacked'", ns("rel_plot_type1")),
                mod_emissions_by_sectors_rel_stacked_ui(ns("emissions_by_sectors_rel_stacked_plot1"))
              )
            ),
            column(
              6,
              mod_emissions_by_sectors_abs_ui(ns("emissions_by_sectors_abs_plot2")),
              # Show relative plot depending on toggle
              conditionalPanel(
                condition = sprintf("input['%s'] == 'rel'", ns("rel_plot_type1")),
                mod_emissions_by_sectors_rel_ui(ns("emissions_by_sectors_rel_plot2"))
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'stacked'", ns("rel_plot_type1")),
                mod_emissions_by_sectors_rel_stacked_ui(ns("emissions_by_sectors_rel_stacked_plot2"))
              )
            )
          )
        ),
      )
}

mod_page_sectors_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Populate countries choices
        data_path <- "data/data_cleaned/GHG_by_sector_and_country.csv"
        if (file.exists(data_path)) {
            df <- read.csv(data_path, stringsAsFactors = FALSE)
            countries <- sort(unique(df$Country))
            updateSelectInput(session, "countries", choices = countries, selected = "Germany")
            updateSelectInput(session, "countries1", choices = countries, selected = "Germany")
            updateSelectInput(session, "countries2", choices = countries, selected = "France and Monaco")
        }

        # Single plot mode
        mod_emissions_by_sectors_abs_server(
            id = "emissions_by_sectors_abs_plot",
            sectors = reactive(input$sectors),
            countries = reactive(input$countries) 
        )
        mod_emissions_by_sectors_rel_server(
          id = "emissions_by_sectors_rel_plot",
          sectors = reactive(input$sectors),
          countries = reactive(input$countries) 
        )
        mod_emissions_by_sectors_rel_stacked_server(
          id = "emissions_by_sectors_rel_stacked_plot",
          sectors = reactive(input$sectors),
          countries = reactive(input$countries) 
        )
        
        # Comparison mode - Plot 1
        mod_emissions_by_sectors_abs_server(
          id = "emissions_by_sectors_abs_plot1",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries1)
        )
        
        mod_emissions_by_sectors_abs_server(
          id = "emissions_by_sectors_abs_plot1",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries2)
        )
        mod_emissions_by_sectors_rel_server(
          id = "emissions_by_sectors_rel_plot1",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries) 
        )
        mod_emissions_by_sectors_rel_stacked_server(
          id = "emissions_by_sectors_rel_stacked_plot1",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries1) 
        )
        
        # Comparison mode - Plot 2
        
        mod_emissions_by_sectors_abs_server(
          id = "emissions_by_sectors_abs_plot2",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries2)
        )
        mod_emissions_by_sectors_rel_server(
          id = "emissions_by_sectors_rel_plot2",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries2) 
        )
        mod_emissions_by_sectors_rel_stacked_server(
          id = "emissions_by_sectors_rel_stacked_plot2",
          sectors = reactive(input$sectors1),
          countries = reactive(input$countries2) 
        )
    })
}
