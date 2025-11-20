library(shiny)
source("modules/emissions_by_sector_abs.R")
source("modules/emissions_by_sector_rel.R")
source("modules/emissions_by_sector_rel_stacked.R")


ui <- navbarPage(
  title = "Greenhouse Gas Emissions",
  
  tabPanel("Emissions",
           fluidPage(
             
             # Row 1: selectors side by side
             fluidRow(
               column(
                 width = 2,
                 checkboxGroupInput(
                   "sectors", "Select sectors:",
                   choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                               "Industrial Combustion", "Power Industry",
                               "Processes", "Transport", "Waste"),
                   selected = c("Industrial Combustion", "Power Industry")
                 ),
                 
                 
               ),
               column(
                 width = 5,
                 selectInput(
                   "countries_1", "Select Country:",
                   choices = "Loading...",
                   selected = ""
                 )
               ),
               column(
                 width = 5,
                 
                 selectInput(
                   "countries_2", "Select Country:",
                   choices = "Loading...",
                   selected = ""
                 )
               )
             ),
             
             br(),
             fluidRow(
               column(6, mod_emissions_by_sectors_abs_ui("abs1")),
               column(6, mod_emissions_by_sectors_abs_ui("abs2"))
             ),
             br(),
             fluidRow(
               column(6, mod_emissions_by_sectors_rel_ui("rel1")),
               column(6, mod_emissions_by_sectors_rel_ui("rel2"))
             ),
             br(),
             fluidRow(
               column(6, mod_emissions_by_sectors_rel_stacked_ui("stacked1")),
               column(6, mod_emissions_by_sectors_rel_stacked_ui("stacked2"))
             )
           )
           
  ),
  
  tabPanel("About",
           fluidPage(
             h3("About this dashboard"),
             p("This dashboard visualizes global CO₂ emissions by sector over time.")
           )
  )
)

server <- function(input, output, session) {
  data <- read_excel("data/EDGAR_2024_GHG_booklet_2024.xlsx",
                     sheet = "GHG_by_sector_and_country")
  
  updateSelectInput(session, "countries_1",
                    choices = sort(unique(data$Country)),
                    selected = "GLOBAL TOTAL")
  updateSelectInput(session, "countries_2",
                    choices = sort(unique(data$Country)),
                    selected = "Germany")
  
  
  mod_emissions_by_sectors_abs_server("abs1", sectors = reactive(input$sectors), countries = reactive(input$countries_1))
  mod_emissions_by_sectors_rel_server("rel1", sectors = reactive(input$sectors), countries = reactive(input$countries_1))
  mod_emissions_by_sectors_rel_stacked_server("stacked1", sectors = reactive(input$sectors), countries = reactive(input$countries_1))
  
  mod_emissions_by_sectors_abs_server("abs2", sectors = reactive(input$sectors), countries = reactive(input$countries_2))
  mod_emissions_by_sectors_rel_server("rel2", sectors = reactive(input$sectors), countries = reactive(input$countries_2))
  mod_emissions_by_sectors_rel_stacked_server("stacked2", sectors = reactive(input$sectors), countries = reactive(input$countries_2))
  
}

shinyApp(ui, server)