library(dplyr)
library(tidyr)
library(countrycode)

# Load data
data <- read.csv("data/data_cleaned/GHG_total_gdp_capita.csv", stringsAsFactors = FALSE)
colnames(data) <- c("country_code", "country", "year", "total_emissions_MtCO2e", "emissions_per_capita", "emissions_per_GDP")
data$country_code <- toupper(trimws(data$country_code))

# Clean country names
data$country_clean <- data$country
data$country_clean[data$country == "Switzerland and Liechtenstein"] <- "Switzerland"
data$country_clean[data$country == "Denmark (with Faroe Islands and Greenland)"] <- "Denmark"
data$country_clean[data$country == "Serbia and Montenegro"] <- "Serbia"

# Add continent
data$continent <- countrycode(data$country_clean, "country.name", "continent", warn = FALSE)

# Filter
data_filtered <- data %>%
    filter(!is.na(continent)) %>%
    filter(!grepl("International|EU27|EARTH", country, ignore.case = TRUE))

print(paste("Rows after filtering:", nrow(data_filtered)))
print(head(data_filtered))

# Continent aggregation
continent_data <- data_filtered %>%
    group_by(continent, year) %>%
    summarise(
        total_emissions_MtCO2e = sum(total_emissions_MtCO2e, na.rm = TRUE),
        emissions_per_capita = mean(emissions_per_capita, na.rm = TRUE),
        emissions_per_GDP = mean(emissions_per_GDP, na.rm = TRUE),
        .groups = "drop"
    )

print("Continent Data Head:")
print(head(continent_data))
