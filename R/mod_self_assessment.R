# R/mod_self_assessment_ui.R
mod_self_assessment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ### Card 0: Period selection
    div(id = ns("period_selection_card"),
        card(
          card_header("Select Assessment Period"),
          card_body(
            mod_miles_select_ui(ns("period_selector")),
            div(
              style = "display:flex;justify-content:space-between;",
              actionButton(ns("period_next"),        "Begin Assessment", class = "btn-primary"),
              div(id = ns("custom_period_next_container"), style = "display:none;",
                  actionButton(ns("custom_period_next"), "Begin Assessment", class = "btn-primary")
              )
            )
          )
        )
    ),

    ### Card 1: Intro
    div(id = ns("intro_card"), class = "hidden-card",
        card(
          card_header("Welcome & Intern Intro"),
          card_body(
            p("Welcome to the IMSLU Residency Program! ..."),
            radioButtons(ns("hs_mo"),      "High school in MO?", choices = c(Yes="1", No="0"), inline=TRUE),
            radioButtons(ns("college_mo"), "College in MO?",    choices = c(Yes="1", No="0"), inline=TRUE),
            radioButtons(ns("med_mo"),     "Med school in MO?", choices = c(Yes="1", No="0"), inline=TRUE),
            actionButton(ns("intro_next"), "Next Section", class = "btn-primary")
          )
        )
    ),

    ### Card 2: Plus/Delta
    div(id = ns("section1_card"), class = "hidden-card",
        card(
          card_header("1. Plus/Delta Review"),
          card_body(
            textAreaInput(ns("plus"),  "What are you doing well?", rows=3),
            textAreaInput(ns("delta"), "Where do you need to improve?", rows=3),
            actionButton(ns("section1_next"), "Next Section", class = "btn-primary")
          )
        )
    ),

    ### Card 3: Skills & Extra (dynamic)
    div(id = ns("section2_card"), class = "hidden-card",
        card(
          card_header("2. Skills & Extra Reflections"),
          card_body(
            uiOutput(ns("card2_ui")),
            actionButton(ns("section2_next"), "Next Section", class = "btn-primary")
          )
        )
    ),

    ### Card 4: Program Review
    div(id = ns("section3_card"), class = "hidden-card",
        card(
          card_header("3. Review of Program"),
          card_body(
            sliderInput(ns("program_satisfaction"), "Overall satisfaction:", min=1, max=10, value=5),
            textAreaInput(ns("program_strengths"),     "Program strengths:", rows=3),
            textAreaInput(ns("program_improvements"),  "Suggested improvements:", rows=3),
            actionButton(ns("section3_next"), "Next Section", class = "btn-primary")
          )
        )
    ),

    ### Card 5: Scholarship
    div(id = ns("section4_card"), class = "hidden-card",
        card(
          card_header("4. Scholarship"),
          card_body(
            checkboxGroupInput(ns("scholarship_activities"), "Scholarship activities:", choices=NULL),
            conditionalPanel(condition=sprintf("input['%s'].includes('Other')", ns("scholarship_activities")),
                             textInput(ns("other_scholarship"), "Specify other:")),
            numericInput(ns("num_publications"), "# Publications:", 0, min=0),
            actionButton(ns("section4_next"), "Next Section", class="btn-primary")
          )
        )
    ),

    ### Card 6: Milestones
    div(id = ns("section5_card"), class = "hidden-card",
        card(
          card_header("5. Milestone Self-Assessment"),
          card_body(
            selectInput(ns("milestone1"), "Milestone 1:", choices=c("Not Started","In Progress","Completed")),
            selectInput(ns("milestone2"), "Milestone 2:", choices=c("Not Started","In Progress","Completed")),
            selectInput(ns("milestone3"), "Milestone 3:", choices=c("Not Started","In Progress","Completed")),
            textAreaInput(ns("milestone_reflection"), "Reflection:", rows=3),
            actionButton(ns("section5_next"), "Next Section", class="btn-primary")
          )
        )
    ),

    ### Card 7: Goals & Submit
    div(id = ns("section6_card"), class = "hidden-card",
        card(
          card_header("6. Goals & Submission"),
          card_body(
            textAreaInput(ns("goal1"), "Goal 1:", rows=2),
            dateInput(ns("goal1_deadline"), "Deadline:"),
            actionButton(ns("submit"), "Submit Self-Assessment", class="btn-success")
          )
        )
    ),

    ### Card 8: Completion
    div(id = ns("completion_card"), class = "hidden-card",
        card(
          card_header("Completed"),
          card_body(p("Thank you! Your responses have been recorded."))
        )
    )
  )
}

mod_self_assessment_server <- function(
    id,
    resident_name,
    resident_data,
    s_miles,
    p_miles,
    data_dict,
    redcap_uri,
    redcap_token
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Clear radio defaults after UI renders
    session$onFlushed(function() {
      updateRadioButtons(session, "hs_mo",      selected = character(0))
      updateRadioButtons(session, "college_mo", selected = character(0))
      updateRadioButtons(session, "med_mo",     selected = character(0))
    }, once = TRUE)

    # Load selected period
    period_sel <- mod_miles_select_server("period_selector")

    # Initialize buffer storage
    responses <- reactiveValues()

    # Card 2: dynamic UI based on period
    output$card2_ui <- renderUI({
      req(period_sel())
      ps <- period_sel()
      # Helper to parse "1, Yes | 2, No" strings safely
      parse_choices <- function(raw) {
        if (is.null(raw) || is.na(raw) || nchar(raw) == 0) return(character(0))
        items <- strsplit(raw, "\\|")[[1]]
        kv <- lapply(items, function(x) {
          parts <- trimws(strsplit(x, ",")[[1]])
          if (length(parts) < 2) return(NULL)
          val <- parts[1]
          lab <- paste(parts[-1], collapse=",")
          setNames(val, lab)
        })
        unlist(kv)
      }

      if (ps == "Entering Residency") {
        ume_flds <- data_dict %>%
          filter(form_name=="s_eval", field_annotation=="ume") %>%
          select(field_name, field_type, select_choices_or_calculations, field_label)

        tagList(
          lapply(seq_len(nrow(ume_flds)), function(i) {
            fld  <- ume_flds$field_name[i]
            type <- ume_flds$field_type[i]
            lbl  <- ume_flds$field_label[i]
            raw  <- ume_flds$select_choices_or_calculations[i]
            switch(type,
                   checkbox = checkboxGroupInput(ns(fld), lbl, choices = parse_choices(raw)),
                   radio    = radioButtons(ns(fld),    lbl, choices = parse_choices(raw)),
                   text     = textInput(ns(fld),       lbl),
                   notes    = textAreaInput(ns(fld),   lbl, rows = 2),
                   textInput(ns(fld), lbl)
            )
          })
        )
      }

     else if (ps == "Graduating") {
        tagList(
          textAreaInput(ns("s_e_discussion"),   "Anything to discuss further?", rows = 2),
          textAreaInput(ns("s_e_board_concern"),"Any concerns about boards?", rows = 2),
          textAreaInput(ns("s_e_board_help"),   "What help do you need for boards?", rows = 2),
          textAreaInput(ns("s_e_board_discu"),  "Board discussion points?", rows = 2),
          selectInput(ns("s_e_grad_next"),      "What will you be doing in July?",
                      choices = parse_choices(
                        data_dict %>% filter(field_name=="s_e_grad_next") %>% pull(select_choices_or_calculations)
                      )),
          textInput(ns("s_e_grad_next_other"),  "If Other, please specify:"),
          checkboxGroupInput(ns("s_e_grad_fellow"), "Which fellowship(s)?",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_grad_fellow") %>% pull(select_choices_or_calculations)
                             )),
          textInput(ns("s_e_grad_fellow_oth"),  "Other fellowship:"),
          selectInput(ns("s_e_grad_where"),     "Where will you work?",
                      choices = parse_choices(
                        data_dict %>% filter(field_name=="s_e_grad_where") %>% pull(select_choices_or_calculations)
                      )),
          selectInput(ns("s_e_grad_loc"),       "Practice location:",
                      choices = parse_choices(
                        data_dict %>% filter(field_name=="s_e_grad_loc") %>% pull(select_choices_or_calculations)
                      )),
          textInput(ns("s_e_grad_loc_other"),   "If Other location:"),
          radioButtons(ns("s_e_grad_fellow_loc"), "Are you at SLU for fellowship?",
                       choices = c("Yes"="1","No"="0"), inline=TRUE),
          textInput(ns("s_e_grad_fellow_loc_else"), "If no, where?"),
          textInput(ns("s_e_grad_email"),       "Best email for future contact:"),
          textInput(ns("s_e_grad_phone"),       "Best phone number:")
        )

      } else {
        tagList(
          checkboxGroupInput(ns("s_e_topic_sel"),
                             "Which 3 core IM topics do you feel least confident about?",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_topic_sel") %>% pull(select_choices_or_calculations)
                             )
          ),
          textInput(ns("s_e_topic_oth"),         "If Other, please specify:"),
          checkboxGroupInput(ns("s_e_learn_style"),
                             "Desired learning experiences:",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_learn_style") %>% pull(select_choices_or_calculations)
                             )
          ),
          textInput(ns("s_e_learn_oth"),         "Other learning style:"),
          checkboxGroupInput(ns("s_e_career_path"),
                             "What career path(s) are you considering?",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_career_path") %>% pull(select_choices_or_calculations)
                             )
          ),
          textInput(ns("s_e_career_oth"),        "Other career path:"),
          checkboxGroupInput(ns("s_e_fellow"),
                             "Which fellowship(s) are you considering?",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_fellow") %>% pull(select_choices_or_calculations)
                             )
          ),
          radioButtons(ns("s_e_track"),
                       "Are you planning to pursue any of the program's formal tracks?",
                       choices = c("Yes"="1","No"="0"), inline=TRUE
          ),
          checkboxGroupInput(ns("s_e_track_type"),
                             "Please select which tracks you are interested in pursuing",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_track_type") %>% pull(select_choices_or_calculations)
                             )
          ),
          textAreaInput(ns("s_e_discussion"),   "Anything you'd like to discuss further?", rows=2),
          textAreaInput(ns("s_e_step3"),        "Would you like Step 3 coaching?", rows=2),
          textInput(ns("s_e_step3_contact"),    "Preferred contact for Step 3 help"),
          dateInput(ns("s_e_step3_date_set"),   "When would you like to schedule it?"),
          textAreaInput(ns("s_e_board_concern"),"Any concerns about boards?", rows=2),
          textAreaInput(ns("s_e_board_help"),   "What help do you need for boards?", rows=2),
          textAreaInput(ns("s_e_board_discu"),  "Board discussion points?", rows=2),
          checkboxGroupInput(ns("s_e_mksap_comp"),
                             "Which MKSAP content have you completed?",
                             choices = parse_choices(
                               data_dict %>% filter(field_name=="s_e_mksap_comp") %>% pull(select_choices_or_calculations)
                             )
          )
        )
      }
    })

    # Helper to transition between cards using shinyjs
    transition_section <- function(from, to) {
      shinyjs::hide(id = ns(from))
      shinyjs::show(id = ns(to))
    }

    # Handle "Begin Assessment" for both buttons
    handle_period_next <- function() {
      req(resident_name(), period_sel())
      if (period_sel() == "Entering Residency") {
        transition_section("period_selection_card", "intro_card")
      } else {
        transition_section("period_selection_card", "section1_card")
      }
    }
    observeEvent(input$period_next,        { handle_period_next() })
    observeEvent(input$custom_period_next, { handle_period_next() })

    # Display resident & coach info
    output$resident_name <- renderText(resident_name())
    output$coach_name    <- renderText({
      df <- resident_data %>% filter(name == resident_name(), !is.na(coach))
      if (nrow(df)) df$coach[1] else "Coach not available"
    })

    # Plus/Delta modal (card1)
    output$plus_delta_table <- DT::renderDT({
      dt <- generate_p_d(resident_data, resident_name())
      create_styled_dt(dt, caption = "Plus/Delta Feedback")
    })

    showEvaluationModal <- function() {
      showModal(modalDialog(
        title     = "Your Evaluations (Plus/Delta)",
        DT::dataTableOutput(ns("plus_delta_table")),
        footer    = modalButton("Close"),
        easyClose = TRUE, size = "l", class = "big-modal"
      ))
    }
    observeEvent(input$open_modal,   showEvaluationModal())
    observeEvent(input$reopen_modal, showEvaluationModal())

    # Intern Intro submit (intro_next)
    observeEvent(input$intro_next, {
      req(input$hs_mo, input$college_mo, input$med_mo)
      record_id <- fetch_record_id(resident_name(), NULL, redcap_uri, redcap_token)
      validate(need(record_id, "No record_id found!"))
      hs_val      <- if (input$hs_mo      == "Yes") "1" else "0"
      college_val <- if (input$college_mo == "Yes") "1" else "0"
      med_val     <- if (input$med_mo     == "Yes") "1" else "0"
      payload_intro <- data.frame(
        record_id              = record_id,
        hs_mo                  = hs_val,
        college_mo             = college_val,
        med_mo                 = med_val,
        resident_data_complete = 0,
        stringsAsFactors       = FALSE
      )
      res_intro <- submit_to_redcap(payload_intro, record_id, redcap_uri, redcap_token)
      if (!isTRUE(res_intro$success)) {
        showNotification(paste("Save failed:", res_intro$outcome_message), type = "error")
        return()
      }
      showNotification("Entering Residency data saved.", type = "message")
      transition_section("intro_card", "section1_card")
    })

    # Card1 (Plus/Delta) next
    observeEvent(input$section1_next, {
      req(input$plus, input$delta)
      responses$s_e_plus  <- input$plus
      responses$s_e_delta <- input$delta
      transition_section("section1_card", "section2_card")
    })

    # Card2 next: buffer dynamic inputs
    observeEvent(input$section2_next, {
      req(period_sel())
      ps <- period_sel()
      ume_flds <- data_dict %>% filter(form_name=="s_eval", field_annotation=="ume") %>% pull(field_name)
      if (ps == "Entering Residency") {
        fields_to_buffer <- ume_flds
      } else if (ps == "Graduating") {
        fields_to_buffer <- c(
          "s_e_discussion","s_e_board_concern","s_e_board_help","s_e_board_discu",
          "s_e_grad_next","s_e_grad_next_other","s_e_grad_fellow","s_e_grad_fellow_oth",
          "s_e_grad_where","s_e_grad_loc","s_e_grad_loc_other",
          "s_e_grad_fellow_loc","s_e_grad_fellow_loc_else",
          "s_e_grad_email","s_e_grad_phone"
        )
      } else {
        fields_to_buffer <- c(
          "s_e_topic_sel","s_e_topic_oth",
          "s_e_learn_style","s_e_learn_oth",
          "s_e_career_path","s_e_career_oth",
          "s_e_fellow","s_e_track","s_e_track_type",
          "s_e_discussion","s_e_step3","s_e_step3_contact",
          "s_e_step3_date_set","s_e_step3_date",
          "s_e_board_concern","s_e_board_help","s_e_board_discu",
          "s_e_mksap_comp"
        )
      }
      for (fld in fields_to_buffer) {
        responses[[fld]] <- input[[fld]]
      }
      transition_section("section2_card", "section3_card")
    })

    # Card3 (Review of Program) next: submit s_eval instance
    observeEvent(input$section3_next, {
      req(resident_name(), period_sel())
      responses$s_e_program_satisfaction <- input$program_satisfaction
      responses$s_e_program_strengths    <- input$program_strengths
      responses$s_e_program_improvements <- input$program_improvements
      record_id <- fetch_record_id(resident_name(), NULL, redcap_uri, redcap_token)
      validate(need(record_id, "No record_id found!"))
      next_inst <- generate_new_instance(record_id, "s_eval", NULL, redcap_uri, redcap_token)
      base <- tibble(
        record_id                = record_id,
        redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance   = next_inst,
        s_e_date                 = format(Sys.Date(), "%Y-%m-%d"),
        s_e_period               = get_ccc_session(period_sel()),
        s_eval_complete          = 0
      )
      buf <- reactiveValuesToList(responses)
      df_extra <- buf[names(buf) %in% c(
        "s_e_plus","s_e_delta",
        ume_flds,
        "s_e_program_satisfaction","s_e_program_strengths","s_e_program_improvements"
      )]
      payload_s_eval <- bind_cols(base, as_tibble(df_extra))
      res_s_eval <- submit_to_redcap(payload_s_eval, record_id, redcap_uri, redcap_token)
      if (!isTRUE(res_s_eval$success)) {
        showNotification(paste("Save failed:", res_s_eval$outcome_message), type = "error")
        return()
      }
      showNotification("Plus/Delta, Skills & Program Review saved.", type = "message")
      transition_section("section3_card", "section4_card")
    })

    # Card4 → Scholarship
    observeEvent(input$section4_next, { transition_section("section4_card", "section5_card") })
    # Card5 → Milestones
    observeEvent(input$section5_next, { transition_section("section5_card", "section6_card") })
    # Final submit
    observeEvent(input$submit,          { transition_section("section6_card", "completion_card") })

  })
}


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
