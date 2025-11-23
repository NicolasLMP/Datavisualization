library(tidyverse)
library(readr)

# Define file paths
gdp_path <- "data/GHG_per_gdp_by_country.csv"
capita_path <- "data/GHG_per_capita_by_country.csv"
total_path <- "data/GHG_totals_by_country.csv"

# Read data
gdp_data <- read_csv(gdp_path, show_col_types = FALSE)
capita_data <- read_csv(capita_path, show_col_types = FALSE)
total_data <- read_csv(total_path, show_col_types = FALSE)

# Function to reshape data
reshape_data <- function(data, value_name) {
    data %>%
        pivot_longer(
            cols = -c(`EDGAR Country Code`, Country),
            names_to = "year",
            values_to = value_name
        ) %>%
        mutate(year = as.numeric(year))
}

# Reshape each dataset
gdp_long <- reshape_data(gdp_data, "ghg_per_gdp")
capita_long <- reshape_data(capita_data, "ghg_per_capita")
total_long <- reshape_data(total_data, "ghg_total")

# Join datasets
# We use full_join to ensure we keep all years (1970-2023) even if GDP is missing for some
combined_data <- total_long %>%
    full_join(capita_long, by = c("EDGAR Country Code", "Country", "year")) %>%
    full_join(gdp_long, by = c("EDGAR Country Code", "Country", "year"))

# Save the processed data
write_csv(combined_data, "data/data_cleaned/GHG_total_gdp_capita.csv")
