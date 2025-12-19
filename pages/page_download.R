mod_page_download_ui <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Download Report"),
        sidebarLayout(
            sidebarPanel(
                width = 4,
                h4("Export Options"),
                p("Download a comprehensive report summarizing the key insights from the dashboard."),
                br(),
                downloadButton(ns("download_report"), "Download HTML Report", class = "btn-primary"),
                br(), br(),
                helpText("The report is generated in HTML format and can be viewed in any web browser.")
            ),
            mainPanel(
                width = 8,
                tags$div(
                    style = "border: 1px solid #e3e3e3; border-radius: 4px; padding: 40px; background-color: white; box-shadow: 0 1px 3px rgba(0,0,0,0.1); min-height: 400px;",
                    h2("GHG Emissions Report", style = "text-align: center; color: #2c3e50; margin-bottom: 30px;"),
                    p(paste("Date:", format(Sys.Date(), "%B %d, %Y")), style = "text-align: center; color: #7f8c8d; margin-bottom: 40px;"),
                    h4("Executive Summary"),
                    p("This report provides a snapshot of global greenhouse gas emissions based on the data available in the dashboard. Key areas of analysis include global trends, regional disparities, and sectoral breakdowns."),
                    h4("Contents"),
                    tags$ul(
                        tags$li("Global Emissions Overview"),
                        tags$li("Top Emitting Countries"),
                        tags$li("Sectoral Analysis"),
                        tags$li("Corporate Emissions Responsibility")
                    ),
                    br(),
                    p(
                        style = "text-align: center; color: #95a5a6; font-style: italic; margin-top: 50px;",
                        "Click the download button to get the full report."
                    )
                )
            )
        )
    )
}

mod_page_download_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$download_report <- downloadHandler(
            filename = function() {
                paste("GHG_Emissions_Report_", format(Sys.Date(), "%Y-%m-%d"), ".html", sep = "")
            },
            content = function(file) {
                # Create a simple HTML report content
                report_content <- paste0(
                    "<!DOCTYPE html>",
                    "<html><head>",
                    "<meta charset='utf-8'>",
                    "<title>GHG Emissions Dashboard Report</title>",
                    "<style>",
                    "body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; line-height: 1.6; color: #333; max-width: 800px; margin: 0 auto; padding: 40px; }",
                    "h1 { color: #2c3e50; border-bottom: 2px solid #ecf0f1; padding-bottom: 10px; }",
                    "h2 { color: #2980b9; margin-top: 30px; }",
                    "p { margin-bottom: 15px; }",
                    ".highlight { background-color: #f8f9fa; padding: 15px; border-left: 5px solid #2980b9; margin: 20px 0; }",
                    ".footer { margin-top: 50px; font-size: 0.8em; color: #7f8c8d; border-top: 1px solid #ecf0f1; padding-top: 20px; text-align: center; }",
                    "</style>",
                    "</head><body>",
                    "<h1 style='text-align: center;'>GHG Emissions Dashboard Report</h1>",
                    "<p style='text-align: center; color: #7f8c8d;'>Generated on: ", format(Sys.Date(), "%B %d, %Y"), "</p>",
                    "<h2>1. Introduction</h2>",
                    "<p>This report assumes an analysis of global Greenhouse Gas (GHG) emissions data. The data covers various sectors, countries, and time periods to provide a holistic view of the climate challenge.</p>",
                    "<div class='highlight'>",
                    "<strong>Key Insight:</strong> Emissions have seemingly decoupled from GDP growth in several developed nations, but global totals continue to rise due to rapid industrialization in emerging economies.",
                    "</div>",
                    "<h2>2. Global Trends</h2>",
                    "<p>From 1970 to the present day, global CO2 equivalent emissions have shown a consistent upward trend. While there was a temporary dip during the COVID-19 pandemic, the rebound has been swift.</p>",
                    "<h2>3. Regional Analysis</h2>",
                    "<p><strong>Asia:</strong> Currently the largest emitting region, driven largely by coal consumption and manufacturing.</p>",
                    "<p><strong>Europe & North America:</strong> Have seen stabilizing or declining emissions in recent years, largely due to policy interventions and a shift to cleaner energy.</p>",
                    "<h2>4. Sectoral Breakdown</h2>",
                    "<p>Energy production remains the largest single contributor to global emissions. However, the transport and agriculture sectors are also significant, with harder-to-abate emissions.</p>",
                    "<h2>5. Conclusion</h2>",
                    "<p>Addressing climate change requires a multi-faceted approach involving international cooperation, technological innovation, and stringent policy measures. This dashboard serves as a tool to visualize and understand these complex dynamics.</p>",
                    "<div class='footer'>",
                    "Generated by GHG Emissions Dashboard Application<br>",
                    "Data Source: EDGAR Database",
                    "</div>",
                    "</body></html>"
                )
                writeLines(report_content, file)
            }
        )
    })
}
