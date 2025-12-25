# pages/page_about.R

mod_page_about_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            style = "max-width: 1200px; margin: auto; padding: 20px;",
            h2("About this dashboard", style = "color: #2c3e50; margin-bottom: 30px;"),
            wellPanel(
                style = "background-color: #f8f9fa;",
                h3("Overview", style = "color: #34495e;"),
                p("This interactive dashboard provides comprehensive visualizations of global greenhouse gas (GHG) emissions data, enabling exploration of emission patterns across regions, countries, and major corporate emitters from 1970 to 2024."),
                p("The visualizations are designed to answer critical research questions about emission trends, regional disparities, corporate responsibility, and the relationship between economic growth and environmental impact.")
            ),
            hr(),
            h3("Data sources", style = "color: #2c3e50; margin-top: 30px;"),
            wellPanel(
                style = "background-color: #ffffff; border-left: 4px solid #3498db;",
                h4("EDGAR - Emissions Database for Global Atmospheric Research", style = "color: #2980b9;"),
                tags$ul(
                    tags$li(strong("Provider:"), " European Commission Joint Research Centre (JRC)"),
                    tags$li(strong("Coverage:"), " Global GHG emissions by country (1970-2023)"),
                    tags$li(strong("Metrics:"), " Total emissions, per capita emissions, emissions per GDP"),
                    tags$li(strong("Source:"), tags$a(href = "https://edgar.jrc.ec.europa.eu/", target = "_blank", "EDGAR Database"))
                )
            ),
            wellPanel(
                style = "background-color: #ffffff; border-left: 4px solid #e74c3c;",
                h4("Carbon Majors Database", style = "color: #c0392b;"),
                tags$ul(
                    tags$li(strong("Provider:"), " Climate Accountability Institute"),
                    tags$li(strong("Coverage:"), " Corporate and state-owned entity emissions (1854-2022)"),
                    tags$li(strong("Focus:"), " Top emitting companies by commodity type"),
                    tags$li(strong("Source:"), tags$a(href = "https://carbonmajors.org/", target = "_blank", "Carbon Majors"))
                )
            ),
            hr(),
            h3("Visualization methodology", style = "color: #2c3e50; margin-top: 30px;"),
            wellPanel(
                style = "background-color: #f8f9fa;",
                h4("Design principles", style = "color: #34495e;"),
                tags$ul(
                    tags$li(strong("Interactive exploration:"), " All visualizations support user interaction through filters, sliders, and animations"),
                    tags$li(strong("Multiple perspectives:"), " Data can be viewed in absolute terms, per capita, or per GDP to reveal different insights"),
                    tags$li(strong("Temporal analysis:"), " Time-based controls and animations show emission trends over decades"),
                    tags$li(strong("Comparative analysis:"), " Side-by-side comparison mode enables direct regional/metric comparisons"),
                    tags$li(strong("Clear visual encoding:"), " Color palettes, labels, and legends are designed for clarity and accessibility")
                )
            ),
            wellPanel(
                style = "background-color: #ffffff;",
                h4("Visual channels and encodings", style = "color: #34495e;"),
                tags$ul(
                    tags$li(strong("Color:"), " Represents commodity types (companies), emission intensity (heatmap), and regional groupings"),
                    tags$li(strong("Size/Height:"), " Encodes emission magnitude in bar charts and line plots"),
                    tags$li(strong("Position:"), " Shows temporal progression (x-axis) and emission values (y-axis)"),
                    tags$li(strong("Animation:"), " Reveals temporal changes and ranking dynamics over time")
                )
            ),
            hr(),
            h3("Key features", style = "color: #2c3e50; margin-top: 30px;"),
            fluidRow(
                column(
                    4,
                    wellPanel(
                        style = "background-color: #ecf0f1; min-height: 280px;",
                        h4("Global page", style = "color: #16a085; font-weight: bold;"),
                        tags$ul(
                            tags$li("Multi-metric analysis (total, per capita, per GDP, accumulated)"),
                            tags$li("Continental and country-level filtering"),
                            tags$li("Temporal animation (1970-2023)"),
                            tags$li("Side-by-side comparison mode")
                        )
                    )
                ),
                column(
                    4,
                    wellPanel(
                        style = "background-color: #ecf0f1; min-height: 280px;",
                        h4("Map page", style = "color: #2980b9; font-weight: bold;"),
                        tags$ul(
                            tags$li("Metrics: Total, Per Capita, Per GDP, and Total Accumulated"),
                            tags$li("Blue ocean basemap for context"),
                            tags$li("Light gray for missing data"),
                            tags$li("Interactive country tooltips")
                        )
                    )
                ),
                column(
                    4,
                    wellPanel(
                        style = "background-color: #ecf0f1; min-height: 280px;",
                        h4("Companies page", style = "color: #c0392b; font-weight: bold;"),
                        tags$ul(
                            tags$li("Top 10 emitting companies ranking"),
                            tags$li("Commodity-based color coding"),
                            tags$li("Aggregate comparison (Top 10 vs All Companies)"),
                            tags$li("Historical ranking evolution (1854-2022)")
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    4,
                    wellPanel(
                        style = "background-color: #ecf0f1; min-height: 280px;",
                        h4("AI Analysis", style = "color: #8e44ad; font-weight: bold;"),
                        tags$ul(
                            tags$li("Sector Fingerprint: Radar chart analysis of economic structures"),
                            tags$li("Interactive comparison of multiple countries/continents"),
                            tags$li("Analysis of emission drivers (Power vs Transport vs Industry)"),
                            tags$li("Temporal evolution of economic 'shapes' (1990-2022)")
                        )
                    )
                ),
                column(
                    4,
                    wellPanel(
                        style = "background-color: #ecf0f1; min-height: 280px;",
                        h4("Sectors page", style = "color: #d35400; font-weight: bold;"),
                        tags$ul(
                            tags$li("Deep dive into emission sources (Energy, Agriculture, Waste, etc.)"),
                            tags$li("Toggle between Absolute (MtCO₂e) and Relative (%) views"),
                            tags$li("Stacked area charts for historical composition"),
                            tags$li("Interactive breakouts of sub-sectors")
                        )
                    )
                ),
                column(
                    4,
                    wellPanel(
                        style = "background-color: #ecf0f1; min-height: 280px;",
                        h4("Data & Reports", style = "color: #7f8c8d; font-weight: bold;"),
                        tags$ul(
                            tags$li("Generate and download PDF reports of current logical state"),
                            tags$li("Access raw data and key insights offline"),
                            tags$li("Transparent data sourcing and citation")
                        )
                    )
                )
            ),
            hr(),
            wellPanel(
                style = "background-color: #fff3cd; border-left: 4px solid #ffc107;",
                h4("Data processing notes", style = "color: #856404;"),
                tags$ul(
                    tags$li("All emission values are expressed in megatonnes of CO₂ equivalent (MtCO₂e)"),
                    tags$li("Per GDP metrics are only available from 1990 onwards"),
                    tags$li("Country codes follow ISO 3166-1 alpha-3 standard"),
                    tags$li("Accumulated emissions represent cumulative totals from 1970 to the selected year"),
                    tags$li("Company data excludes nation-state entities to focus on corporate emitters")
                )
            ),
            hr(),
            div(
                style = "text-align: center; color: #7f8c8d; margin-top: 40px;",
                p("Dashboard created for data visualization course | 2024"),
                p(style = "font-size: 0.9em;", "Data sources: EDGAR (JRC) & Carbon Majors Database")
            )
        )
    )
}

mod_page_about_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # No server logic needed for static about page
    })
}
