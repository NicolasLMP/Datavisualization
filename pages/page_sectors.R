# pages/page_sectors.R

mod_page_sectors_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* Remove excessive top margin from the title */
        h2 { 
          margin-top: 10px !important; 
          margin-bottom: 20px !important; 
        }
    
        /* Reduce padding at the top of the main dashboard area */
        .container-fluid { 
          padding-top: 10px !important; 
        }
      
        /* Keep labels bold but use default text color */
        .control-label { 
          font-weight: bold !important; 
          margin-bottom: 8px;
        }
        
        /* Sidebar background styling */
        .well { 
          background-color: #f8f9fa; 
          border: 1px solid #e9ecef; 
        }

        /* Adjusting checkbox and radio button colors to Okabe-Ito Blue */
        input[type='checkbox']:checked, 
        input[type='radio']:checked {
          accent-color: #0072B2; /* Normal Blue */
        }
        
        /* For older browser compatibility */
        input[type='checkbox']:checked, 
        input[type='radio']:checked {
          background-color: #0072B2 !important;
          border-color: #0072B2 !important;
        }
      ")))
    ),
    
    titlePanel("What are the sector emissions in each country?"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(
          ns("sectors"), "Select sectors:",
          choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                      "Industrial Combustion", "Power Industry",
                      "Processes", "Transport", "Waste"),
          selected = c("Industrial Combustion", "Power Industry", "Processes", "Fuel Exploitation")
        ),
        
        selectInput(ns("countries"), "Choose Country:", choices = NULL),
        
        radioButtons(
          ns("rel_plot_type"), "Select relative plot type:",
          choices = c(
            "Relative Stacked (%)" = "stacked",
            "Relative (%)" = "rel"
          ),
          selected = "stacked",
          inline = TRUE
        )
      ),
      mainPanel(
        width = 9,
        mod_emissions_by_sectors_abs_ui(ns("emissions_by_sectors_abs_plot")),
        
        # Instruction for interactivity
        tags$p(
          style = "color: #555; font-style: italic; margin-top: 10px;",
          tags$span(icon("info-circle"), style = "color: #0072B2; margin-right: 5px;"),
          "Tip: Hover over the data points in the line above chart to see the specific gas composition for that year and sector."
        ),
        
        tags$div(style = "height: 10px;"),
        
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
          style = "background-color: #fcfcfc; border-left: 5px solid #0072B2;",
          h4("Sector Definitions", style = "font-weight: bold;"),
          
          # Row 1: Agriculture & Power Industry
          fluidRow(
            column(6,
                   tags$p(tags$b("Agriculture:"), tags$br(), 
                          "Agricultural soils, crop residues burning, enteric fermentation, manure management, and indirect N2O emissions.")
            ),
            column(6,
                   tags$p(tags$b("Power Industry:"), tags$br(), 
                          "Power and heat generation plants (public & autoproducers).",
                          tags$br(), " ") # The requested line break
            )
          ),
          
          # Row 2: Buildings & Processes
          fluidRow(
            column(6,
                   tags$p(tags$b("Buildings:"), tags$br(), 
                          "Small scale non-industrial stationary combustion (heating and cooling).")
            ),
            column(6,
                   tags$p(tags$b("Processes:"), tags$br(), 
                          "Emissions from production of cement, iron and steel, aluminum, chemicals, solvents, etc.")
            )
          ),
          
          # Row 3: Fuel Exploitation & Transport
          fluidRow(
            column(6,
                   tags$p(tags$b("Fuel Exploitation:"), tags$br(), 
                          "Production, transformation, and refining of fuels.")
            ),
            column(6,
                   tags$p(tags$b("Transport:"), tags$br(), 
                          "Mobile combustion (road, rail, ship, and aviation).")
            )
          ),
          
          # Row 4: Industrial Combustion & Waste
          fluidRow(
            column(6,
                   tags$p(tags$b("Industrial Combustion:"), tags$br(), 
                          "Combustion for industrial manufacturing processes.")
            ),
            column(6,
                   tags$p(tags$b("Waste:"), tags$br(), 
                          "Solid waste disposal and waste water treatment.")
            )
          )
        ),
        
        tags$div(style = "height: 30px;"),
        
        # Research Questions
        wellPanel(
          style = "background-color: #fcfcfc; border-left: 5px solid #0072B2;",
          h4("Research questions", style = "font-weight: bold;"),
          tags$ul(
            tags$li("How are the sectors in relation to another?"),
            tags$li("What sectors are improving?"),
            tags$li("What sectors are very influential?"),
            tags$li("How do countries compare to another?"),
            tags$li("What are the emissions of the sectors in each country?"),
            tags$li("How is the gas distribution on a sector for a year?"),
            tags$li("How does the gas distribution on a sector develop over time?")
          )
        ),
        tags$div(style = "height: 30px;")
      )
    )
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
    }
    
    # Single plot mode server calls
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
  })
}