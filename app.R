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
                 width = 6,
                 checkboxGroupInput(
                   "shared_sectors_1", "Select sectors (Group 1):",
                   choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                               "Industrial Combustion", "Power Industry",
                               "Processes", "Transport", "Waste"),
                   selected = c("Industrial Combustion", "Power Industry")
                 )
               ),
               column(
                 width = 6,
                 checkboxGroupInput(
                   "shared_sectors_2", "Select sectors (Group 2):",
                   choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                               "Industrial Combustion", "Power Industry",
                               "Processes", "Transport", "Waste"),
                   selected = c("Industrial Combustion", "Power Industry")
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
  mod_emissions_by_sectors_abs_server("abs1", sectors = reactive(input$shared_sectors_1))
  mod_emissions_by_sectors_rel_server("rel1", sectors = reactive(input$shared_sectors_1))
  mod_emissions_by_sectors_rel_stacked_server("stacked1", sectors = reactive(input$shared_sectors_1))
  
  mod_emissions_by_sectors_abs_server("abs2", sectors = reactive(input$shared_sectors_2))
  mod_emissions_by_sectors_rel_server("rel2", sectors = reactive(input$shared_sectors_2))
  mod_emissions_by_sectors_rel_stacked_server("stacked2", sectors = reactive(input$shared_sectors_2))
  
}

shinyApp(ui, server)