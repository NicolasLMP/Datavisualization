# pages/page_home.R

mod_page_home_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # Hero Section
        tags$div(
            style = "background: linear-gradient(135deg, #1D3557 0%, #457B9D 100%);
                     color: white; padding: 60px 20px; text-align: center; margin-bottom: 40px;
                     border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
            tags$h1("Global Greenhouse Gas Emissions Dashboard",
                style = "font-size: 3em; margin-bottom: 20px; font-weight: bold;"
            ),
            tags$h3("Tracking Climate Impact from 1900 to Present",
                style = "font-size: 1.5em; margin-bottom: 20px; opacity: 0.9;"
            ),
            tags$p("Explore comprehensive data on global emissions, from countries and regions to individual companies.
                   Understand the evolution of our climate impact and identify key contributors to greenhouse gas emissions.",
                style = "font-size: 1.1em; max-width: 800px; margin: 0 auto; line-height: 1.6;"
            )
        ),

        # Key Statistics Section
        tags$h2("Key Statistics", style = "text-align: center; margin: 40px 0 30px 0; color: #1D3557;"),
        fluidRow(
            column(
                3,
                wellPanel(
                    style = "background: linear-gradient(135deg, #E63946 0%, #DC143C 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    tags$div(icon("globe", class = "fa-3x"), style = "margin-bottom: 15px;"),
                    tags$h3("37,000+", style = "font-size: 2.5em; margin: 10px 0;"),
                    tags$p("MtCO2e World Total (2022)", style = "font-size: 1.1em;")
                )
            ),
            column(
                3,
                wellPanel(
                    style = "background: linear-gradient(135deg, #2A9D8F 0%, #1D7874 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    tags$div(icon("industry", class = "fa-3x"), style = "margin-bottom: 15px;"),
                    tags$h3("Top 10", style = "font-size: 2.5em; margin: 10px 0;"),
                    tags$p("Companies Tracked", style = "font-size: 1.1em;")
                )
            ),
            column(
                3,
                wellPanel(
                    style = "background: linear-gradient(135deg, #457B9D 0%, #1D3557 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    tags$div(icon("map", class = "fa-3x"), style = "margin-bottom: 15px;"),
                    tags$h3("195+", style = "font-size: 2.5em; margin: 10px 0;"),
                    tags$p("Countries Analyzed", style = "font-size: 1.1em;")
                )
            ),
            column(
                3,
                wellPanel(
                    style = "background: linear-gradient(135deg, #F77F00 0%, #D62828 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    tags$div(icon("calendar", class = "fa-3x"), style = "margin-bottom: 15px;"),
                    tags$h3("123", style = "font-size: 2.5em; margin: 10px 0;"),
                    tags$p("Years of Data (1900-2022)", style = "font-size: 1.1em;")
                )
            )
        ),

        # What You'll Discover Section
        tags$h2("What You'll Discover", style = "text-align: center; margin: 50px 0 30px 0; color: #1D3557;"),
        fluidRow(
            column(
                6,
                wellPanel(
                    style = "background-color: #f8f9fa; border-left: 5px solid #E63946; padding: 25px;",
                    tags$h4(icon("chart-line"), " Emission Trends", style = "color: #1D3557; margin-bottom: 15px;"),
                    tags$ul(
                        style = "line-height: 1.8;",
                        tags$li("How global emissions have evolved since 1900"),
                        tags$li("Regional and country-specific emission patterns"),
                        tags$li("Sector-by-sector breakdown of emissions"),
                        tags$li("Per capita and per GDP emission intensities")
                    )
                )
            ),
            column(
                6,
                wellPanel(
                    style = "background-color: #f8f9fa; border-left: 5px solid #2A9D8F; padding: 25px;",
                    tags$h4(icon("building"), " Corporate Impact", style = "color: #1D3557; margin-bottom: 15px;"),
                    tags$ul(
                        style = "line-height: 1.8;",
                        tags$li("Top emitting companies and their evolution"),
                        tags$li("Corporate emissions vs. world total comparison"),
                        tags$li("Commodity-based emission analysis"),
                        tags$li("Historical rankings and changes over time")
                    )
                )
            )
        ),

        # Navigation Cards
        tags$h2("Explore the Dashboard", style = "text-align: center; margin: 50px 0 30px 0; color: #1D3557;"),
        fluidRow(
            column(
                3,
                tags$a(
                    href = "#",
                    onclick = "document.querySelector('a[data-value=\"Global\"]').click(); return false;",
                    style = "text-decoration: none;",
                    wellPanel(
                        style = "background-color: white; border: 2px solid #457B9D; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        tags$div(icon("globe-americas", class = "fa-4x"), style = "color: #457B9D; margin-bottom: 20px;"),
                        tags$h4("Global Trends", style = "color: #1D3557; margin-bottom: 10px;"),
                        tags$p("Explore worldwide emission patterns and regional comparisons",
                            style = "color: #666; font-size: 0.95em;"
                        )
                    )
                )
            ),
            column(
                3,
                tags$a(
                    href = "#",
                    onclick = "document.querySelector('a[data-value=\"Map\"]').click(); return false;",
                    style = "text-decoration: none;",
                    wellPanel(
                        style = "background-color: white; border: 2px solid #2A9D8F; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        tags$div(icon("map-marked-alt", class = "fa-4x"), style = "color: #2A9D8F; margin-bottom: 20px;"),
                        tags$h4("Interactive Map", style = "color: #1D3557; margin-bottom: 10px;"),
                        tags$p("Visualize emissions geographically across countries and time",
                            style = "color: #666; font-size: 0.95em;"
                        )
                    )
                )
            ),
            column(
                3,
                tags$a(
                    href = "#",
                    onclick = "document.querySelector('a[data-value=\"Sectors\"]').click(); return false;",
                    style = "text-decoration: none;",
                    wellPanel(
                        style = "background-color: white; border: 2px solid #F77F00; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        tags$div(icon("industry", class = "fa-4x"), style = "color: #F77F00; margin-bottom: 20px;"),
                        tags$h4("By Sector", style = "color: #1D3557; margin-bottom: 10px;"),
                        tags$p("Analyze emissions by economic sector and industry",
                            style = "color: #666; font-size: 0.95em;"
                        )
                    )
                )
            ),
            column(
                3,
                tags$a(
                    href = "#",
                    onclick = "document.querySelector('a[data-value=\"Companies\"]').click(); return false;",
                    style = "text-decoration: none;",
                    wellPanel(
                        style = "background-color: white; border: 2px solid #E63946; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        tags$div(icon("building", class = "fa-4x"), style = "color: #E63946; margin-bottom: 20px;"),
                        tags$h4("Top Companies", style = "color: #1D3557; margin-bottom: 10px;"),
                        tags$p("Track the world's largest corporate emitters over time",
                            style = "color: #666; font-size: 0.95em;"
                        )
                    )
                )
            )
        ),

        # Footer
        tags$div(
            style = "margin-top: 60px; padding: 30px; background-color: #f8f9fa;
                     text-align: center; border-radius: 10px;",
            tags$p("Data sources: EDGAR, Global Carbon Project, and corporate emissions databases",
                style = "color: #666; margin-bottom: 10px;"
            ),
            tags$p("Last updated: 2024 | Covering emissions from 1900 to 2022",
                style = "color: #666;"
            )
        )
    )
}

mod_page_home_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # No server logic needed for static home page
    })
}
