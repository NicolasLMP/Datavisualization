# modules/company_vs_world.R

mod_company_vs_world_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "text-align: center; margin-bottom: 20px;",
      tags$img(
        src = "companies_vs_world.gif", style = "max-width: 100%; height: auto;",
        alt = "Animation not generated. Please run scripts/generate_companies_vs_world_gif.R"
      )
    )
  )
}

mod_company_vs_world_server <- function(id, race_year) {
  moduleServer(id, function(input, output, session) {
    # Logic replaced by static GIF as per user request
  })
}
