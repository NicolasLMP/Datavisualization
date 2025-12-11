# 🌍 Global Greenhouse Gas Emissions Dashboard

A comprehensive interactive dashboard for exploring and analyzing global greenhouse gas emissions data from 1900 to 2022. Built with R Shiny, this application provides insights into emissions trends across countries, regions, sectors, and companies.

![Dashboard Preview](https://img.shields.io/badge/R-Shiny-blue?logo=r)
![Status](https://img.shields.io/badge/status-active-success)
![License](https://img.shields.io/badge/license-MIT-green)

##  Features

###  Home Page
- **Hero Section** with mission statement and overview
- **Key Statistics** displaying current emission metrics
- **Quick Navigation** cards to all dashboard sections
- **Discovery Sections** highlighting key insights

### Global Trends
- Interactive visualizations of worldwide emission patterns
- Regional and country-specific comparisons
- Time series analysis from 1900 to present
- Per capita and per GDP emission metrics

### Interactive Map
- Choropleth map showing emissions by country
- Year-by-year animation capability
- Multiple metric views (total, per capita, per GDP)
- Hover tooltips with detailed information

### Sector Analysis
- Breakdown of emissions by economic sector
- Absolute and relative emission comparisons
- Stacked area charts showing sector evolution
- Industry-specific trends

### Company Tracking
- **Animated race chart** of top 10 emitting companies (1900-2022)
- **Companies vs World comparison** showing corporate impact
- Color-coded by primary commodity (Oil, Coal, Natural Gas, etc.)
- Historical rankings and changes over time

### About & Methodology
- Detailed data source information
- Methodology explanations
- Data processing notes
- Research questions addressed

## Getting Started

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "gganimate",
  "dplyr",
  "tidyr",
  "leaflet",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "countrycode",
  "htmltools",
  "scales",
  "gifski"  # For GIF generation
))
```

### Installation

1. Clone the repository:
```bash
git clone https://github.com/NicolasLMP/Datavisualization.git
cd Datavisualization
```

2. Open the project in RStudio or navigate to the directory in R

3. Run the application:
```r
shiny::runApp()
```

## 📁 Project Structure

```
Datavisualization/
├── app.R                          # Main application file
├── pages/                         # Page modules
│   ├── page_home.R               # Home page
│   ├── page_regions.R            # Global trends page
│   ├── page_heatmap.R            # Interactive map page
│   ├── page_sectors.R            # Sector analysis page
│   ├── page_companies.R          # Company tracking page
│   └── page_about.R              # About page
├── diagrams/                      # Visualization modules
│   ├── emissions_by_region.R
│   ├── global_heatmap.R
│   ├── top_companies.R
│   ├── emissions_by_sector_abs.R
│   ├── emissions_by_sector_rel.R
│   ├── emissions_by_sector_rel_stacked.R
│   └── emissions_top_companies_vs_world.R
├── scripts/                       # GIF generation scripts
│   ├── generate_companies_gif.R
│   └── generate_companies_vs_world_gif.R
├── data/                          # Data directory
│   └── data_cleaned/             # Processed datasets
├── www/                           # Static assets (GIFs, images)
└── README.md                      # This file
```

##  Design & Branding

### Color Palette
- **Primary Blue**: #1D3557 (atmosphere/sky)
- **Accent Blue**: #457B9D (interactive elements)
- **Teal**: #2A9D8F (sustainability)
- **Red**: #E63946 (urgency/heat)
- **Orange**: #F77F00 (energy)

### Typography
- **Font**: Inter (via Google Fonts)
- **Style**: Clean, modern sans-serif

## Data Sources

- **EDGAR** (Emissions Database for Global Atmospheric Research)
- **Global Carbon Project**
- **Corporate emissions databases**
- **Country-level economic indicators**

**Coverage**: 1900-2022 | 195+ countries | Top 10 companies

## Generating Animated GIFs

The dashboard includes animated visualizations. To regenerate them:

```r
# Generate companies race chart
source("scripts/generate_companies_gif.R")

# Generate companies vs world comparison
source("scripts/generate_companies_vs_world_gif.R")
```

**Note**: GIF generation requires the `gganimate` and `gifski` packages and may take 30-60 seconds per animation.

## Customization

### Changing Colors
Edit the `bslib::bs_theme()` section in `app.R`:
```r
theme = bslib::bs_theme(
  primary = "#457B9D",    # Change primary color
  secondary = "#2A9D8F",  # Change secondary color
  ...
)
```

### Modifying Statistics
Update the statistics cards in `pages/page_home.R` to reflect your data.

## Research Questions Addressed

- How have global emissions evolved since 1900?
- Which countries and regions are the largest emitters?
- How do emissions per capita and per GDP vary across countries?
- What is the contribution of different economic sectors?
- How do top companies' emissions compare to world totals?
- How have corporate rankings changed over time?

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Authors

**Nicolas LMP**
- GitHub: [@NicolasLMP](https://github.com/NicolasLMP)

**Konstantin Blank**

## Acknowledgments

- Data providers: EDGAR, Global Carbon Project
- R Shiny community
- All contributors to the open-source packages used

## Contact

For questions or feedback, please open an issue on GitHub.

---

**Built with ❤️ using R Shiny**
