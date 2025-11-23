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

# Save the processed data
write_csv(companies_cleaned, "data/GHG_by_sector_and_companies.csv")
