library(ggplot2)
library(gganimate)
library(dplyr)

# Load data
data_path <- "data/data_cleaned/GHG_by_sector_and_companies.csv"
if (!file.exists(data_path)) stop("Data file not found")
df <- read.csv(data_path, stringsAsFactors = FALSE)
colnames(df) <- gsub(" ", "_", colnames(df))

# Aggregate by company/year and get dominant commodity (sector)
df_agg <- df %>%
  group_by(year, parent_entity) %>%
  summarise(
    value = sum(total_emissions_MtCO2e, na.rm = TRUE),
    dominant_commodity = commodity[which.max(total_emissions_MtCO2e)],
    .groups = "drop"
  ) %>%
  filter(year >= 1900) # Start from 1900

# Get top 10 for each year and assign ranks
df_top10 <- df_agg %>%
  group_by(year) %>%
  slice_max(order_by = value, n = 10) %>%
  mutate(
    rank = rank(-value, ties.method = "first"),
    value_label = paste0(round(value), " MtCO2e")
  ) %>%
  ungroup()

# Define color palette for commodities/sectors
commodity_colors <- c(
  "Oil & NGL" = "#E63946",
  "Natural Gas" = "#457B9D",
  "Coal" = "#1D3557",
  "Cement" = "#A8DADC",
  "Metallurgical Coal" = "#2A9D8F",
  "Thermal Coal" = "#264653",
  "Bituminous Coal" = "#2B2D42",
  "Sub-Bituminous Coal" = "#8D99AE",
  "Lignite" = "#D90429",
  "Anthracite" = "#000000"
)

# Create horizontal bar race chart
p <- ggplot(df_top10) +
  geom_col(aes(x = rank, y = value, group = parent_entity, fill = dominant_commodity),
    width = 0.8, color = "white", size = 0.5
  ) +
  geom_text(
    data = df_top10, aes(x = rank, y = 0, label = parent_entity, group = parent_entity),
    hjust = 1.1, size = 5.5, fontface = "bold", color = "black"
  ) +
  scale_fill_manual(values = commodity_colors) +
  coord_flip(clip = "off") +
  scale_x_reverse(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    plot.margin = margin(1, 4, 1, 12, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(
    title = NULL,
    subtitle = "Year: {closest_state}",
    x = NULL,
    y = "Total Emissions (MtCO2e)"
  ) +
  transition_states(year, transition_length = 5, state_length = 2) +
  ease_aes("cubic-in-out")

# Save
if (!dir.exists("www")) dir.create("www")
anim_save("www/companies_race_v4.gif", p, fps = 20, duration = 60, width = 1200, height = 700, end_pause = 40)
print("GIF generated successfully at www/companies_race_v4.gif")
