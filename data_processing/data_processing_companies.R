library(tidyverse)
library(readr)

# Define file path
companies_path <- "data/GHG_by_sector_and_company.csv"

# Read data
companies_data <- read_csv(companies_path, show_col_types = FALSE)

# Filter out Nation State parent entities
# The user specified removing "nation state parent entity like poland, former union soviet etcc"
# We filter based on parent_type == "Nation State"
companies_cleaned <- companies_data %>%
    filter(parent_type != "Nation State")

# Load OWID data for World emissions
owid_path <- "data/owid-co2-data.csv"
owid_data <- read_csv(owid_path, show_col_types = FALSE)

# Extract World emissions
# We select 'total_ghg' as the world emissions metric
world_emissions <- owid_data %>%
    filter(country == "World") %>%
    select(year, world = total_ghg)

# Join with companies data
companies_with_world <- companies_cleaned %>%
    left_join(world_emissions, by = "year")

# Save the processed data
write_csv(companies_with_world, "data/data_cleaned/GHG_by_sector_and_companies.csv")
