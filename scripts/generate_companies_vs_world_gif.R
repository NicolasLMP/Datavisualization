library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)

# Load data
data_path <- "data/data_cleaned/GHG_by_sector_and_companies.csv"
if (!file.exists(data_path)) stop("Data file not found")
df <- read.csv(data_path, stringsAsFactors = FALSE)
colnames(df) <- gsub(" ", "_", colnames(df))

# Filter to start from 1900 and aggregate by year
summary_data <- df %>%
    filter(year >= 1900) %>%
    group_by(year) %>%
    summarise(
        `Top Emitting Companies` = sum(total_emissions_MtCO2e, na.rm = TRUE),
        `World Total` = first(world),
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = c(`Top Emitting Companies`, `World Total`),
        names_to = "Category",
        values_to = "Emissions"
    )

# Create animated horizontal bar chart
p <- ggplot(summary_data, aes(x = Emissions, y = Category, fill = Category)) +
    geom_col(width = 0.6, color = "white", size = 1) +
    geom_text(aes(label = paste0(round(Emissions), " MtCO2e")),
        hjust = -0.1, size = 6, fontface = "bold", color = "black"
    ) +
    scale_fill_manual(
        values = c("Top Emitting Companies" = "#E63946", "World Total" = "#457B9D")
    ) +
    scale_x_continuous(
        labels = scales::comma,
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.2))
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 16) +
    theme(
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        plot.margin = margin(1, 4, 1, 2, "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "none"
    ) +
    labs(
        title = "GHG Emissions: Top Companies vs World Total",
        subtitle = "Year: {closest_state}",
        x = "Emissions (MtCO2e)",
        y = NULL
    ) +
    transition_states(year, transition_length = 5, state_length = 2) +
    ease_aes("cubic-in-out")

# Save
if (!dir.exists("www")) dir.create("www")
anim_save("www/companies_vs_world.gif", p, fps = 20, duration = 60, width = 1000, height = 500, end_pause = 40)
print("GIF generated successfully at www/companies_vs_world.gif")
