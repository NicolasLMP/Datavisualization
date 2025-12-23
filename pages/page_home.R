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
      tags$h3("Tracking Climate Impact from 1854 to Present",
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
        wellPanel( # World Total - Sky Blue
          style = "background: linear-gradient(135deg, #56B4E9 0%, #0072B2 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          tags$div(icon("globe", class = "fa-3x"), style = "margin-bottom: 15px;"),
          tags$h3("53,000+", style = "font-size: 2.5em; margin: 10px 0;"),
          tags$p("MtCO2e World Total (2023)", style = "font-size: 1.1em;")
        )
      ),
      column(
        3,
        wellPanel( # Companies - Teal
          style = "background: linear-gradient(135deg, #009E73 0%, #007858 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          tags$div(icon("industry", class = "fa-3x"), style = "margin-bottom: 15px;"),
          tags$h3("Top 170+", style = "font-size: 2.5em; margin: 10px 0;"),
          tags$p("Companies Tracked", style = "font-size: 1.1em;")
        )
      ),
      column(
        3,
        wellPanel( # Countries - Navy
          style = "background: linear-gradient(135deg, #1D3557 0%, #457B9D 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          tags$div(icon("map", class = "fa-3x"), style = "margin-bottom: 15px;"),
          tags$h3("195+", style = "font-size: 2.5em; margin: 10px 0;"),
          tags$p("Countries Analyzed", style = "font-size: 1.1em;")
        )
      ),
      column(
        3,
        wellPanel( # Years - Grey
          style = "background: linear-gradient(135deg, #999999 0%, #666666 100%);
                             color: white; text-align: center; padding: 30px; border: none; border-radius: 10px;
                             box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          tags$div(icon("calendar", class = "fa-3x"), style = "margin-bottom: 15px;"),
          tags$h3("169", style = "font-size: 2.5em; margin: 10px 0;"),
          tags$p("Years of Data (1854-2023)", style = "font-size: 1.1em;")
        )
      )
    ),
    tags$div(style = "height: 30px;"),
    
    # What You'll Discover Section
    tags$h2("Scope and Insights", style = "text-align: center; margin: 50px 0 30px 0; color: #1D3557;"),
    fluidRow(
      column(
        6,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 5px solid #0072B2; padding: 25px;",
          tags$h4(icon("chart-line"), " Emission Trends", style = "color: #1D3557; margin-bottom: 15px; font-weight: bold;"),
          tags$ul(
            style = "line-height: 1.8;",
            tags$li("How global emissions have evolved since 1854"),
            tags$li("Regional and country-specific emission patterns"),
            tags$li("Sector-by-sector breakdown of emissions"),
            tags$li("Per capita and per GDP emission intensities")
          )
        )
      ),
      column(
        6,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 5px solid #0072B2; padding: 25px;",
          tags$h4(icon("building"), " Corporate Impact", style = "color: #1D3557; margin-bottom: 15px; font-weight: bold;"),
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
    
    tags$div(style = "height: 30px;"),
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
            style = "background-color: white; border: 2px solid #56B4E9; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            tags$div(icon("globe-americas", class = "fa-4x"), style = "color: #56B4E9; margin-bottom: 20px;"),
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
            style = "background-color: white; border: 2px solid #0072B2; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            tags$div(icon("map-marked-alt", class = "fa-4x"), style = "color: #0072B2; margin-bottom: 20px;"),
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
            style = "background-color: white; border: 2px solid #009E73; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            tags$div(icon("industry", class = "fa-4x"), style = "color: #009E73; margin-bottom: 20px;"),
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
            style = "background-color: white; border: 2px solid #999999; padding: 30px;
                                 text-align: center; transition: all 0.3s; cursor: pointer; border-radius: 10px;
                                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            tags$div(icon("building", class = "fa-4x"), style = "color: #999999; margin-bottom: 20px;"),
            tags$h4("Top Companies", style = "color: #1D3557; margin-bottom: 10px;"),
            tags$p("Track the world's largest corporate emitters over time",
                   style = "color: #666; font-size: 0.95em;"
            )
          )
        )
      )
    )
  )
}

mod_page_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed for static home page
  })
}