library(readxl)
library(readr)

data <- read_excel("../data/EDGAR_2024_GHG_booklet_2024.xlsx",
                   sheet = "GHG_by_sector_and_country",
                   guess_max = 10000)

long_data <- data %>%
  filter(!is.na(Substance)) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year))

sector_gas_data <- long_data %>%
  group_by(year, Sector, Substance, Country, `EDGAR Country Code`) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

wide_sector_gas <- sector_gas_data %>%
  pivot_wider(names_from = Substance, values_from = total) %>%
  mutate(CO2e = rowSums(select(., CO2, `GWP_100_AR5_CH4`, `GWP_100_AR5_N2O`, `GWP_100_AR5_F-gases`), na.rm = TRUE)) %>%
  rename(CH4 = `GWP_100_AR5_CH4`,
         N2O = `GWP_100_AR5_N2O`,
         F_gases = `GWP_100_AR5_F-gases`,
         Country_code = `EDGAR Country Code`)

# Compute relative values (% of total per year)
wide_sector_gas <- wide_sector_gas %>%
  group_by(year, Country) %>%
  mutate(year_total = sum(CO2e, na.rm = TRUE),
         relative = (CO2e / year_total) * 100) %>%
  ungroup()

write_csv(wide_sector_gas, "data/data_cleaned/GHG_by_sector_and_country.csv")

unique(data$Substance)
