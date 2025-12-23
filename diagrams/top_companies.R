# diagrams/top_companies.R

mod_top_companies_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "text-align: center; margin-bottom: 20px;",
      tags$h3("Who are the top 10 emitting companies?", style = "color: #2c3e50; font-weight: bold;"),
      tags$img(
        src = "companies_race_v4.gif", style = "max-width: 100%; height: auto;",
        alt = "Animation not generated. Please run scripts/generate_companies_gif.R"
      )
    )
  )
}

mod_top_companies_server <- function(id, race_year, show_percentage, show_rank) {
  moduleServer(id, function(input, output, session) {
    # Logic replaced by static GIF as per user request
  })
}
