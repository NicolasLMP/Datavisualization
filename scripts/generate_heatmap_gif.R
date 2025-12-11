library(ggplot2)
library(gganimate)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)

# Load data
data_path <- "data/data_cleaned/GHG_total_gdp_capita.csv"
if (!file.exists(data_path)) stop("Data file not found")
df <- read.csv(data_path, stringsAsFactors = FALSE)
colnames(df) <- c("country_code", "country", "year", "total_emissions_MtCO2e", "emissions_per_capita", "emissions_per_GDP")

# Clean country codes and values
df$country_code <- toupper(trimws(df$country_code))
# Ensure numeric
df$total_emissions_MtCO2e <- as.numeric(df$total_emissions_MtCO2e)

# Get world map (small scale for speed)
world <- ne_countries(scale = 110, returnclass = "sf")

# Map data to ISO codes if needed (simple approach)
df <- df %>%
    mutate(iso_match = ifelse(nchar(country_code) == 3, country_code, countrycode(country, "country.name", "iso3c"))) %>%
    filter(!is.na(iso_match))

# Join data to map
# We want to keep the map geometry but fill with data.
# We need to replicate the map for each year present in the data.
years <- sort(unique(df$year))
map_expanded <- lapply(years, function(y) {
    yearly_df <- df[df$year == y, ]
    merged <- merge(world, yearly_df, by.x = "iso_a3", by.y = "iso_match", all.x = TRUE)
    merged$year <- y
    return(merged)
})
map_data <- do.call(rbind, map_expanded)

# Create animation
p <- ggplot(map_data) +
    geom_sf(aes(fill = total_emissions_MtCO2e), color = "#444444", size = 0.1) +
    scale_fill_gradientn(
        colors = c("#FFD700", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
        na.value = "#D3D3D3",
        name = "Emissions (MtCO2e)"
    ) +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "#aad3df", color = NA), # Match ocean color
        legend.position = "bottom"
    ) +
    labs(title = "Global Emissions: {closest_state}") +
    transition_states(year, transition_length = 1, state_length = 1) +
    ease_aes("linear")

# Save GIF
if (!dir.exists("www")) dir.create("www")
anim_save("www/heatmap.gif", p, nframes = length(years) * 2, fps = 5, width = 800, height = 500)
print("GIF generated successfully at www/heatmap.gif")
