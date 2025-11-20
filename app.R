library(shiny)
source("modules/emissions_by_sector_abs.R")
source("modules/emissions_by_sector_rel.R")
source("modules/emissions_by_sector_rel_stacked.R")


ui <- navbarPage(
  title = "Greenhouse Gas Emissions",
  
  tabPanel("Emissions",
    fluidPage(
      # Shared input for all modules
      checkboxGroupInput("shared_sectors", "Select sectors:",
                         choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                                     "Industrial Combustion", "Power Industry",
                                     "Processes", "Transport", "Waste"),
                         selected = c("Industrial Combustion", "Power Industry")),
       # Three plots in the same page
       fluidRow(
         column(12, mod_emissions_by_sectors_rel_ui("rel"))
       ),
       fluidRow(
         column(12, mod_emissions_by_sectors_abs_ui("abs"))
       ),
       fluidRow(
         column(12, mod_emissions_by_sectors_rel_stacked_ui("stacked"))
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
  mod_emissions_by_sectors_abs_server("abs", sectors = reactive(input$shared_sectors))
  mod_emissions_by_sectors_rel_server("rel", sectors = reactive(input$shared_sectors))
  mod_emissions_by_sectors_rel_stacked_server("stacked", sectors = reactive(input$shared_sectors))
  
}

shinyApp(ui, server)