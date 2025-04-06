#' @importFrom shiny NS selectInput moduleServer

#' @export
# Module for period selection
mod_miles_select_ui <- function(id) {
  ns <- NS(id)
  selectInput(
    ns("period"),
    "Select Period",
    choices = c("Entering Residency", "Mid Intern", "End Intern",
                "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating",
                "Interim Review"),
    selected = "Mid Intern"  # Default value for testing
  )
}

#' @export
mod_miles_select_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$period)
  })
}
