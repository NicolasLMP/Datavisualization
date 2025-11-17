library(readxl)
library(tidyr)
library(dplyr)
library(dygraphs)


data <- read_excel("GHG_Sources/data/EDGAR_2024_GHG_booklet_2024.xlsx", 
                   sheet = "GHG_by_sector_and_country",
)
colnames(data)
global_data <- subset(data, Country == "GLOBAL TOTAL" & Substance == "CO2")
colnames(global_data)


long_data <- global_data %>%
  pivot_longer(cols = matches("^[0-9]{4}$"),  # selects all columns with 4-digit year names
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(year)) %>%
  select(sector = Sector, year, value)


long_data$year <- as.Date(paste0(long_data$year, "-01-01"))


ts_data <- xts(long_data[-1], order.by = long_data$year)
dygraph(ts_data) %>%
  dyRangeSelector() %>%
  dyOptions(stackedGraph = FALSE)

checkboxGroupInput("sectors", "Select sectors:",
                   choices = c("Agriculture", "Buildings", "Fuel Exploitation",
                               "Industrial Combustion", "Power Industry",
                               "Processes", "Transport", "Waste"),
                   selected = c("Agriculture", "Buildings"))

output$plot <- renderDygraph({
  selected <- input$sectors
  dygraph(ts_data[, selected])
})