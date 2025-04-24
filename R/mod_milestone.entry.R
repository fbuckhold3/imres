#' Module UI for Milestone Rating
#'
#' This function creates a namespaced UI for the milestone rating interface.
#' It does not include period selection; instead, the period is supplied externally.
#'
#' @param id A character string specifying the module ID.
#'
#' @return A Shiny UI element containing the milestone rating interface.
#' @export
mod_miles_rating_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("moduleContainer"),
        uiOutput(ns("progressSection")),
        uiOutput(ns("mainContent")),
        uiOutput(ns("navigationButtons")),
        uiOutput(ns("resultsCard")),
        actionButton(ns("done"), "Submit Milestones", class = "btn-success")
    )
  )
}

#' Module Server for Milestone Rating
#'
#' This function encapsulates all server-side logic for the milestone rating
#' interface. It expects an externally provided reactive period. Until a valid
#' period is supplied, a blank screen (or prompt) is shown. If the period is
#' "Interim Review," a message is displayed stating that milestone ratings are not
#' necessary. Otherwise, the full rating interface is rendered and the user cannot
#' proceed to the next milestone until a selection is made on the current one.
#'
#' @param id A character string specifying the module ID.
#' @param period A reactive expression returning the selected period.
#'
#' @return A reactive expression containing the collected selections.
#' @export
mod_miles_rating_server <- function(id, period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Mapping of period names to trigger scores.
    thresholds <- list(
      "Entering Residency" = 3,
      "Mid Intern" = 4,
      "End Intern" = 5,
      "Mid PGY2" = 6,
      "End PGY2" = 7,
      "Mid PGY3" = 8,
      "Graduating" = 9
      # "Interim Review" is handled separately.
    )

    # IMAGE SETS (ensure your image files are in the "www" folder)
    imageSets <- list(
      PC = list(
        title = "Patient Care",
        images = c("pc1.png", "pc2.png", "pc3.png", "pc4.png", "pc5.png", "pc6.png"),
        imageTitles = c("History", "Physical Examination", "Clinical Reasoning",
                        "Patient Management - Inpatient", "Patient Management - Outpatient",
                        "Digital Health")
      ),
      MK = list(
        title = "Medical Knowledge",
        images = c("mk1.png", "mk2.png", "mk3.png"),
        imageTitles = c("Applied Foundational Sciences", "Therapeutic Knowledge",
                        "Knowledge of Diagnostic Testing")
      ),
      SBP = list(
        title       = "Systems-Based Practice",
        images      = c("sbp1.png", "sbp2.png", "sbp3.png"),  # was "spb2.png"
        imageTitles = c(
          "Patient Safety and Quality Improvement",
          "System Navigation for Patient-Centered Care",
          "Physician Role in Health Care Systems"
        )
      ),
      PBLI = list(
        title = "Practice-Based Learning and Improvement",
        images = c("pbli1.png", "pbl2.png"),
        imageTitles = c("Evidence-Based and Informed Practice",
                        "Reflective Practice and Commitment to Personal Growth")
      ),
      PROF = list(
        title = "Professionalism",
        images = c("prof1.png", "prof2.png", "prof3.png", "prof4.png"),
        imageTitles = c("Professional Behavior", "Ethical Principles",
                        "Accountability/Conscientiousness",
                        "Knowledge of Systemic and Individual Factors of Well-Being")
      ),
      ICS = list(
        title = "Interpersonal and Communication Skills",
        images = c("ics1.png", "ics2.png", "ics3.png"),
        imageTitles = c("Patient- and Family-Centered Communication",
                        "Interprofessional and Team Communication",
                        "Communication within Health Care Systems")
      )
    )

    # STATE MANAGEMENT
    state <- reactiveValues(
      currentSetIndex   = 1,
      currentImageIndex = 1,
      showingSummary    = FALSE,
      selections        = list(),  # numeric scores
      descriptions      = list(),  # text explanations per milestone
      pendingSelection  = NULL     # holds list(key=…, value=…) when an explanation is needed
    )

    # Derived reactives.
    currentSetName <- reactive({ names(imageSets)[ state$currentSetIndex ] })
    currentSet <- reactive({ imageSets[[ state$currentSetIndex ]] })
    currentImageFile <- reactive({ currentSet()$images[ state$currentImageIndex ] })
    currentImageTitle <- reactive({ currentSet()$imageTitles[ state$currentImageIndex ] })
    selectionKey <- reactive({ paste0(currentSetName(), "_", state$currentImageIndex) })

    totalImages <- reactive({
      sum(sapply(imageSets, function(set) length(set$images)))
    })

    imagesCompleted <- reactive({
      previousImages <- if (state$currentSetIndex > 1) {
        sum(sapply(imageSets[1:(state$currentSetIndex - 1)], function(set) length(set$images)))
      } else { 0 }
      previousImages + state$currentImageIndex - 1
    })

    # Determine UI state based on the external period.
    # "none"   = no period provided,
    # "interim"= period is "Interim Review",
    # "active" = period is provided and not interim.
    uiState <- reactive({
      if (is.null(period()) || period() == "") {
        "none"
      }  else {
        "active"
      }
    })

    # Helper: Build progress UI.
    buildProgressUI <- function(imageSets, currentSetIndex, imagesCompleted, totalImages) {
      progressValue <- (imagesCompleted() / totalImages()) * 100

      progressLabels <- div(
        class = "d-flex justify-content-between mb-2",
        lapply(seq_along(imageSets), function(i) {
          labelClass <- if (i < currentSetIndex) {
            "text-success"
          } else if (i == currentSetIndex) {
            "text-primary fw-bold"
          } else {
            "text-muted"
          }
          span(class = labelClass, imageSets[[i]]$title)
        })
      )

      progressCounter <- div(
        class = "text-center mb-2",
        paste0(imagesCompleted(), " of ", totalImages(), " images viewed (",
               round(progressValue, 1), "%)")
      )

      tagList(
        progressLabels,
        progressCounter,
        div(
          class = "progress mb-3",
          div(
            class = "progress-bar",
            role = "progressbar",
            style = paste0("width: ", progressValue, "%;"),
            "aria-valuenow" = progressValue,
            "aria-valuemin" = "0",
            "aria-valuemax" = "100"
          )
        )
      )
    }

    # Helper: Generate unique button ID.
    # Here we only need the score since the image indices are already captured.
    buildButtonId <- function(score) {
      paste0("box_", score)
    }

    # RENDER UI OUTPUTS
    output$progressSection <- renderUI({
      if (uiState() != "active") return(NULL)
      buildProgressUI(imageSets, state$currentSetIndex, imagesCompleted, totalImages)
    })

    output$navigationButtons <- renderUI({
      if (uiState() != "active") return(NULL)
      # Disable "Next" if no selection exists for the current milestone.
      nextDisabled <- is.null(state$selections[[selectionKey()]])
      div(class = "card",
          div(class = "card-body",
              fluidRow(
                column(4, actionButton(ns("prevButton"), "Previous", class = "btn-primary", width = "100%")),
                column(4, actionButton(ns("summaryButton"), "Show Summary", class = "btn-info", width = "100%")),
                column(4, actionButton(ns("nextButton"), "Next",
                                       class = if(nextDisabled) "btn-primary disabled" else "btn-primary",
                                       width = "100%"))
              )
          )
      )
    })

    output$resultsCard <- renderUI({
      if (uiState() != "active") return(NULL)
      div(class = "card",
          div(class = "card-header", "Your Selections"),
          verbatimTextOutput(ns("selectionSummary"))
      )
    })

    output$mainContent <- renderUI({
      if (uiState() == "none") {
        return(div("Please select a period to begin."))
      }
      req(currentImageFile())
      currentSelection <- state$selections[[ selectionKey() ]]

      div(class = "card",
          div(class = "card-header",
              paste0(currentSet()$title, " - ", currentSetName(), " ", state$currentImageIndex, ": ",
                     currentImageTitle(), " (Image ", state$currentImageIndex, " of ",
                     length(currentSet()$images), ")")
          ),
          div(
            style = "position: relative;",
            # Use a fixed image output ID for consistent rendering.
            imageOutput(ns("mainImage"), height = "auto"),
            div(
              style = "position: relative; height: 40px; width: 1200px; margin-top: 10px;",
              # Render empty boxes without any numbers above them.
              lapply(1:9, function(i) {
                leftPos <- 100 + (i - 1) * 120
                buttonStyle <- paste0(
                  "width: 30px; height: 30px; padding: 0; text-align: center; ",
                  if (!is.null(currentSelection) && currentSelection == i) {
                    "background-color: #4CAF50; color: white;"
                  } else {
                    "background-color: #f0f0f0;"
                  }
                )
                div(
                  style = paste0("position: absolute; left: ", leftPos, "px; top: 0;"),
                  tags$button(
                    id = ns(buildButtonId(i)),
                    class = "action-button",
                    style = buttonStyle,
                    HTML("&nbsp;")  # Use a non-breaking space as placeholder.
                  )
                )
              })
            )
          )
      )
    })

    # Render the main image using a fixed output ID.
    observe({
      if (uiState() != "active") return()
      req(currentImageFile())
      output$mainImage <- renderImage({
        list(
          src = file.path("www", currentImageFile()),
          width = 1200,
          height = "auto",
          alt = paste("Image", state$currentImageIndex)
        )
      }, deleteFile = FALSE)
    })

    # BOX SELECTION & MODAL TRIGGERS
    observe({
      if (uiState() != "active") return()
      lapply(1:9, function(i) {
        # Capture i in local scope.
        local({
          i_local <- i
          observeEvent(input[[buildButtonId(i_local)]], {
            key <- selectionKey()
            currentPeriod <- period()
            # For Interim Review or for values below the threshold, record selection.
            if (currentPeriod == "Interim Review" || is.null(thresholds[[currentPeriod]]) ||
                i_local < thresholds[[currentPeriod]]) {
              state$selections[[key]] <- i_local
            } else {
              state$pendingSelection <- list(key = key, value = i_local)
              showModal(modalDialog(
                title = paste("Explanation for score", i_local),
                textInput(ns("explanation"),
                          label = "Congratulations. This is advanced for a resident at your level. Please explain your rationale:"),
                footer = tagList(
                  actionButton(ns("cancel_explanation"), "Cancel"),
                  actionButton(ns("submit_explanation"), "Submit")
                ),
                easyClose = FALSE
              ))
            }
          })
        })
      })
    })

    # MODAL HANDLERS
    observeEvent(input$submit_explanation, {
      req(input$explanation)
      key <- state$pendingSelection$key
      val <- state$pendingSelection$value

      # store BOTH the score and the text
      state$selections[[key]]    <- val
      state$descriptions[[key]]  <- input$explanation

      state$pendingSelection <- NULL
      removeModal()
    })

    observeEvent(input$cancel_explanation, {
      state$pendingSelection <- NULL
      removeModal()
    })

    # NAVIGATION EVENTS
    nextImage <- function() {
      # Prevent proceeding if no selection exists for the current milestone.
      if (is.null(state$selections[[selectionKey()]])) {
        showNotification("Please make a selection for the current milestone before proceeding.", type = "error")
        return()
      }
      if (state$currentImageIndex < length(currentSet()$images)) {
        state$currentImageIndex <- state$currentImageIndex + 1
      } else if (state$currentSetIndex < length(imageSets)) {
        state$currentSetIndex <- state$currentSetIndex + 1
        state$currentImageIndex <- 1
      }
    }

    prevImage <- function() {
      if (state$currentImageIndex > 1) {
        state$currentImageIndex <- state$currentImageIndex - 1
      } else if (state$currentSetIndex > 1) {
        state$currentSetIndex <- state$currentSetIndex - 1
        state$currentImageIndex <- length(imageSets[[state$currentSetIndex]]$images)
      }
    }

    observeEvent(input$nextButton, { nextImage() })
    observeEvent(input$prevButton, { prevImage() })

    observeEvent(input$summaryButton, {
      state$showingSummary <- TRUE
    })

    observeEvent(input$continueButton, {
      state$showingSummary <- FALSE
    })

    output$selectionSummary <- renderPrint({
      allSelections <- state$selections
      if (length(allSelections) == 0) return("No selections made yet")

      result <- character()
      for (key in names(allSelections)) {
        parts <- strsplit(key, "_")[[1]]
        setName <- parts[1]
        index <- as.numeric(parts[2])
        result <- c(result, paste0(setName, " Image ", index, " (",
                                   imageSets[[setName]]$imageTitles[index], "): ",
                                   allSelections[[key]]))
      }
      paste(result, collapse = "\n")
    })

    # Return reactive selections for use in the parent app.
    return(list(
      done   = reactive(input$done),
      scores = reactive(state$selections),
      desc   = reactive(state$descriptions)
    ))
  })
}
