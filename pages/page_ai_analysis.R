# pages/page_ai_analysis.R

mod_page_ai_analysis_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("AI Generated Analysis"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h4("About this Analysis"),
                helpText("This radar chart analyzes the 'Pollution Fingerprint' of major economies compared to the Global Average."),
                hr(),
                tags$ul(
                    tags$li("Axes: The 8 major emission sectors."),
                    tags$li("Scale: Percentage (%) share of that sector in the country's total."),
                    tags$li("Goal: Identify structural differences in how economies pollute.")
                ),
                hr(),
                helpText(icon("search"), " Notice how different the shapes are. No two economies are standardized.")
            ),
            mainPanel(
                width = 9,
                mod_ai_analysis_ui(ns("ai_plot")),
                hr(),
                wellPanel(
                    style = "background-color: #f8f9fa;",
                    h4("AI Insights", style = "color: #2c3e50; font-weight: bold;"),
                    tags$p("This chart reveals the structural DNA of each economy:"),
                    tags$ul(
                        tags$li(tags$strong("China:"), " Heavily skewed towards Power & Industry (Manufacturing hub)."),
                        tags$li(tags$strong("USA:"), " A unique spike in Transport relative to the global average (Car-centric)."),
                        tags$li(tags$strong("EU27:"), " More balanced, but significant agricultural footprint relative to its size."),
                        tags$li(tags$strong("Global Average:"), " The dashboard baseline (Dotted line).")
                    )
                )
            )
        )
    )
}

mod_page_ai_analysis_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        mod_ai_analysis_server("ai_plot")
    })
}
