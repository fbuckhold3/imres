#' UI function for scholarship table module
#'
#' @description Creates a UI component for displaying and interacting with scholarship
#' activities. This module shows a table of existing scholarship activities and provides
#' options to add new ones.
#'
#' @param id Character. The module ID used for namespacing.
#'
#' @return A tagList containing the UI elements for the scholarship table.
#'
#' @export
#'
#' @importFrom shiny NS tagList div h3 p br actionButton uiOutput
#' @importFrom DT DTOutput
#'
scholarship_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "scholarship-section mb-4",
      h3("Your Scholarship Activities", class = "mb-3"),
      p("This table shows your scholarship activities recorded in the system:"),
      DTOutput(ns("scholarship_table")),
      # Container for the achievement notifications
      uiOutput(ns("achievement_notifications")),
      br(),
      p("If you've completed additional scholarly activities that aren't reflected here, please add them below:"),
      actionButton(ns("add_scholarship"), "Add New Scholarship Activity", class = "btn-primary"),
      br(), br(),
      actionButton(ns("next_btn"), "Next Section", class = "btn-primary mt-3")
    )
  )
}

#' Server function for scholarship table module
#'
#' @description Server logic for the scholarship table module. Processes scholarship
#' data, renders a table display, and shows achievement notifications for specific
#' scholarship activities.
#'
#' @param id Character. The module ID used for namespacing.
#' @param schol_data Reactive. Data frame containing scholarship activity records.
#' @param rdm_dict Reactive. Dictionary for mapping scholarship activity codes to descriptions.
#' @param record_id Reactive. The current user's record ID.
#'
#' @return A list containing reactive values:
#'   \item{next_clicked}{Reactive. TRUE when the "Next Section" button is clicked.}
#'
#' @export
#'
#' @importFrom shiny moduleServer reactive req renderUI observeEvent showModal modalDialog modalButton
#' @importFrom DT renderDT
#' @importFrom shiny icon tags
#'
scholarship_table_server <- function(id, schol_data, rdm_dict, record_id) {
  moduleServer(id, function(input, output, session) {
    # Process the scholarship data
    scholarship_results <- reactive({
      req(record_id())
      process_scholarship_data(schol_data, record_id(), rdm_dict)
    })
    # Render the table
    output$scholarship_table <- renderDT({
      req(scholarship_results())
      if (nrow(scholarship_results()$table_data) == 0) {
        # Return an empty styled table with a message
        empty_df <- data.frame(
          Scholarship_Type = "No scholarship activities found",
          Description = "",
          stringsAsFactors = FALSE
        )
        create_styled_dt(empty_df, caption = "Scholarship Activities")
      } else {
        # Return the processed data in a styled table
        create_styled_dt(scholarship_results()$table_data, caption = "Scholarship Activities")
      }
    })
    # Render achievement notifications
    output$achievement_notifications <- renderUI({
      req(scholarship_results())
      # Get completion status
      completed_ps <- scholarship_results()$completed_ps
      completed_rca <- scholarship_results()$completed_rca
      if (!completed_ps && !completed_rca) {
        return(NULL)  # Don't show anything if neither is completed
      }
      # Create notification messages
      messages <- list()
      if (completed_ps) {
        messages <- append(messages, tags$div(
          tags$p(
            tags$span(icon("check-circle"), class = "text-success"),
            tags$strong("Achievement: "),
            "You have completed a Patient Safety Review",
            class = "alert alert-success p-2 mt-3"
          )
        ))
      }
      if (completed_rca) {
        messages <- append(messages, tags$div(
          tags$p(
            tags$span(icon("check-circle"), class = "text-success"),
            tags$strong("Achievement: "),
            "You have completed a Root Cause Analysis",
            class = "alert alert-success p-2 mt-3"
          )
        ))
      }
      # Return the messages
      tagList(
        div(
          class = "achievement-notifications mt-3",
          messages
        )
      )
    })
    # You can add logic for the add_scholarship button here
    observeEvent(input$add_scholarship, {
      # Show a modal or form to add new scholarship activity
      # This is just a placeholder - implement based on your needs
      showModal(modalDialog(
        title = "Add New Scholarship Activity",
        p("Form to add new scholarship would go here."),
        # Add your form elements here
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("submit_scholarship"), "Submit")
        )
      ))
    })
    # Return values if needed
    return(list(
      next_clicked = reactive(input$next_btn)
    ))
  })
}
