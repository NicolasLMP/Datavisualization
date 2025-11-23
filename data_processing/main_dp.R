# Main Data Processing Script

# Execute data processing for countries
message("Processing countries data...")
source("data_processing/data_processing_countries.R")

# Execute data processing for companies
message("Processing companies data...")
source("data_processing/data_processing_companies.R")

# Execute data processing for sectors
message("Processing sectors data...")
source("data_processing/data_processing_sectors.R")

message("Data processing complete.")
