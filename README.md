# Global Greenhouse Gas Emissions Dashboard

A comprehensive interactive dashboard for exploring and analyzing global greenhouse gas emissions data from 1854 to 2023. Built with **R Shiny**, this application moves beyond simple country rankings to provide a multi-dimensional view of the climate crisis, allowing users to investigate accountability through distinct lenses: National Territories, Economic Sectors, and Corporate Producers ("Carbon Majors").

![Dashboard Preview](https://img.shields.io/badge/R-Shiny-blue?logo=r)
![Status](https://img.shields.io/badge/status-active-success)
![License](https://img.shields.io/badge/license-MIT-green)

## Key Features

### 1. Global & Regional Analysis
-   **Metric Inversion:** Instantly toggle between **Total Emissions**, **Per Capita**, and **Per GDP** to see how the global hierarchy shifts (e.g., verifying the "Great Divergence" between Global North and South).
-   **Historical Trends:** Track emission trajectories from 1970 to 2023.

### 2. Interactive Map
-   **Choropleth Visualization:** Explore emissions intensity spatially using Leaflet.
-   **Accumulated Emissions:** Visualize the "weight of history" by mapping cumulative emissions since 1970, highlighting the historical debt of industrialized nations.

### 3. Sectoral Breakdown
-   **Absolute & Relative Views:** Decompose national totals into 8 IPCC-defined sectors (Power, Buildings, Transport, etc.).
-   **Chemical Composition:** View the specific breakdown of Greenhouse Gases ($CO_2$, $CH_4$, $N_2O$) for each sector via interactive Donut Charts.
-   **Economic Structure:** Use Stacked Area charts to visualize the changing "energy mix" of a nation over time.

### 4. Corporate Analysis (Carbon Majors)
-   **Supply-Side Focus:** Shifts attention to the 174 largest fossil fuel and cement producers.
-   **Companies vs. World:** Quantify the market share of these corporate giants relative to global totals.
-   **Animated Race Chart:** Watch the rise and fall of corporate empires (Standard Oil, British Coal) and the modern dominance of State-Owned Entities (Saudi Aramco, Gazprom) from 1900 to 2023.

### 5. AI Analysis: The Sector Fingerprint
-   **Structural DNA:** A novel **Radar Chart** (Spider Plot) that visualizes the "shape" of an economy.
-   **Comparative Insight:** Compare the structural fingerprints of different nations (e.g., China's "Industrial Diamond" vs. USA's "Transport Spike") to understand underlying economic drivers.
-   *Note: This visualization module was produced with the assistance of Generative AI (Google Gemini).*

### 6. Full Report
-   **Downloadable Analysis:** Access the full PDF report directly from the dashboard, detailing methodology, data sources, and key findings.

---

## Technology Stack

-   **Language:** R (v4.0+)
-   **Framework:** Shiny
-   **Visualization:** `plotly` (Interactivity), `leaflet` (Mapping), `ggplot2` (Static), `gganimate` (GIFs).
-   **UI/UX:** `bslib` (Theming).

## Project Structure

```
Datavisualization/
├── app.R                          # Main application entry point
├── pages/                         # UI/Server Modules for each page
│   ├── page_home.R               # Landing Page & Key Stats
│   ├── page_regions.R            # Global Trends Line Charts
│   ├── page_heatmap.R            # Interactive Leaflet Map
│   ├── page_sectors.R            # Sectoral Deep-Dive
│   ├── page_companies.R          # Carbon Majors Analysis
│   ├── page_ai_analysis.R        # AI Radar Chart
│   ├── page_download.R           # Report Download Page
│   └── page_about.R              # Methodology & Sources
├── diagrams/                      # Reusable Plotting Functions
│   ├── emissions_by_region.R
│   ├── global_heatmap.R
│   ├── top_companies.R
│   ├── ai_analysis.R             # Radar Chart Logic
│   └── ...
├── data/                          # Processed Datasets (CSV)
│   └── data_cleaned/
├── scripts/                       # Data Processing & GIF Generation
│   ├── main_dp.R                 # ETL Pipeline Orchestrator
│   └── generate_companies_gif.R  # Animation Script
├── www/                           # Static Assets (Images, GIFs, PDF Report)
└── final_report.tex               # LaTeX Source for the Report
```

## Design System

The dashboard utilizes scientifically developed, colorblind-safe palettes to ensure accessibility:

-   **Theme:** Okabe-Ito Blue (`#0072B2`) for primary UI elements.
-   **Categorical Data:** **Okabe-Ito Palette** (8 hues) for distinguishable sectors and regions.
-   **Sequential Data:** **Value-Linked Heat Scale** (Yellow-to-Red) for identifying emission hotspots.

## Installation & Usage

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/NicolasLMP/Datavisualization.git
    cd Datavisualization
    ```

2.  **Install Dependencies:**
    Open `Datavisualization.Rproj` in RStudio and run:
    ```r
    install.packages(c("shiny", "bslib", "plotly", "leaflet", "tidyverse", "sf", "countrycode"))
    ```

3.  **Run the App:**
    ```r
    shiny::runApp()
    ```

## Data Sources

1.  **EDGAR (v8.0):** Country and Sectoral emissions (1970-2023).
2.  **Carbon Majors Database:** Corporate production data (1854-2023).
3.  **Our World In Data:** Global historical baselines.
4.  **World Bank:** GDP and Population metrics.

## Team

**University of Southern Denmark** | *Data Visualisation*

*   **Nicolas Lambropoulos**
*   **Konstantin Blank**

---
*Built in R Shiny.*
