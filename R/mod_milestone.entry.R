#' Module UI for Milestone Rating (simplified version)
#'
#' @param id module id
#' @export
mod_miles_rating_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("moduleContainer"),
        uiOutput(ns("progressSection")),
        uiOutput(ns("mainContent")),
        # Placeholder for the inline explanation box - moved above navigation buttons
        uiOutput(ns("explanationUI")),
        uiOutput(ns("navigationButtons")),
        # Final submit - now rendered conditionally via server
        uiOutput(ns("submitButtonUI"))
    )
  )
}


#' Module Server for Milestone Rating (simplified version)
#'
#' @param id module id
#' @param period reactive returning the selected period
#' @export
mod_miles_rating_server <- function(id, period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # thresholds for when to require explanation
    thresholds <- list(
      "Entering Residency" = 3,
      "Mid Intern"          = 4,
      "End Intern"          = 5,
      "Mid PGY2"            = 6,
      "End PGY2"            = 7,
      "Mid PGY3"            = 8,
      "Graduating"          = 9
    )

    # image sets (same as before)
    imageSets <- list(
      PC = list(
        title = "Patient Care",
        images = paste0("pc", 1:6, ".png"),
        imageTitles = c("History","Physical Examination","Clinical Reasoning",
                        "Patient Management - Inpatient","Patient Management - Outpatient","Digital Health")
      ),
      MK = list(
        title = "Medical Knowledge",
        images = paste0("mk", 1:3, ".png"),
        imageTitles = c("Applied Foundational Sciences","Therapeutic Knowledge","Knowledge of Diagnostic Testing")
      ),
      SBP = list(
        title = "Systems-Based Practice",
        images = paste0("sbp", 1:3, ".png"),
        imageTitles = c("Patient Safety and Quality Improvement",
                        "System Navigation for Patient-Centered Care",
                        "Physician Role in Health Care Systems")
      ),
      PBLI = list(
        title = "Practice-Based Learning and Improvement",
        images = c("pbli1.png","pbli2.png"),  # Fixed typo in image name from pbl2.png to pbli2.png
        imageTitles = c("Evidence-Based and Informed Practice","Reflective Practice and Commitment to Personal Growth")
      ),
      PROF = list(
        title = "Professionalism",
        images = paste0("prof", 1:4, ".png"),
        imageTitles = c("Professional Behavior","Ethical Principles","Accountability/Conscientiousness",
                        "Knowledge of Systemic and Individual Factors of Well-Being")
      ),
      ICS = list(
        title = "Interpersonal and Communication Skills",
        images = paste0("ics", 1:3, ".png"),
        imageTitles = c("Patient- and Family-Centered Communication",
                        "Interprofessional and Team Communication",
                        "Communication within Health Care Systems")
      )
    )

    # state
    state <- reactiveValues(
      currentSetIndex   = 1,
      currentImageIndex = 1,
      selections        = list(),
      descriptions      = list(),
      pendingSelection  = list(key = NULL, value = NULL),
      currentExplanation = NULL  # Added to track current explanation text
    )

    # helpers
    currentSetName   <- reactive(names(imageSets)[ state$currentSetIndex ])
    currentSet       <- reactive(imageSets[[ state$currentSetIndex ]])
    currentImageFile <- reactive(currentSet()$images[ state$currentImageIndex ])
    selectionKey     <- reactive(paste0(currentSetName(), "_", state$currentImageIndex))

    # Fixed imagesDone reactive to avoid the "non-function" error
    totalImages <- reactive(sum(sapply(imageSets, function(z) length(z$images))))
    imagesDone <- reactive({
      base <- if(state$currentSetIndex > 1) {
        sum(sapply(imageSets[1:(state$currentSetIndex-1)], function(z) length(z$images)))
      } else {
        0
      }
      return(base + state$currentImageIndex - 1)
    })

    # Check if all milestone ratings are complete
    allComplete <- reactive({
      expectedKeys <- unlist(lapply(seq_along(imageSets), function(i) {
        setName <- names(imageSets)[i]
        imageCount <- length(imageSets[[i]]$images)
        paste0(setName, "_", seq_len(imageCount))
      }))

      # Check if all expected keys have selections
      allSelected <- all(expectedKeys %in% names(state$selections))

      # Check if all required explanations are provided
      if(allSelected) {
        periodVal <- period()
        if(!is.null(periodVal) && periodVal != "" && periodVal != "Interim Review") {
          threshold <- thresholds[[periodVal]]
          if(!is.null(threshold)) {
            needsExplanation <- sapply(names(state$selections), function(key) {
              state$selections[[key]] >= threshold
            })

            if(any(needsExplanation)) {
              keysNeedingExplanation <- names(state$selections)[needsExplanation]
              allExplanations <- all(keysNeedingExplanation %in% names(state$descriptions))
              return(allExplanations)
            }
          }
        }
        return(TRUE)
      }
      return(FALSE)
    })

    uiState <- reactive({
      if (is.null(period()) || period()=="") "none" else "active"
    })

    # progress UI - now with only the progress bar, no text summary
    output$progressSection <- renderUI({
      req(uiState()=="active")
      pct <- round(100 * imagesDone()/totalImages(), 1)
      tagList(
        div(class="d-flex justify-content-between mb-2",
            lapply(seq_along(imageSets), function(i){
              cls <- if (i< state$currentSetIndex) "text-success"
              else if (i==state$currentSetIndex) "text-primary fw-bold"
              else "text-muted"
              span(class=cls, imageSets[[i]]$title)
            })
        ),
        div(class="text-center mb-2", paste0(imagesDone()," of ", totalImages()," (",pct,"%)")),
        div(class="progress mb-3",
            div(class="progress-bar", role="progressbar",
                style=paste0("width:",pct,"%"),
                `aria-valuenow`=pct, `aria-valuemin`=0, `aria-valuemax`=100
            )
        )
      )
    })

    # Conditional submit button UI
    output$submitButtonUI <- renderUI({
      req(uiState() == "active")
      if(allComplete()) {
        div(
          class = "mt-4 text-center",
          actionButton(ns("done"), "Submit Milestones",
                       class = "btn-success btn-lg",
                       style = "width: 50%;")
        )
      } else {
        div(
          class = "mt-4 text-center text-muted",
          "Complete all milestone ratings to enable submission"
        )
      }
    })

    # main image + score buttons + inline explain placeholder
    output$mainContent <- renderUI({
      if (uiState()=="none") return(div("Please select a period to begin."))
      key <- selectionKey()
      sel <- state$selections[[key]]
      tagList(
        div(class="card",
            div(class="card-header",
                paste0(currentSet()$title," – ", currentSetName(),
                       " ", state$currentImageIndex," of ",length(currentSet()$images))
            ),
            div(style="position: relative;",
                imageOutput(ns("mainImage"), height="auto"),
                # buttons 1–9
                div(style="position: relative; height:40px; width:1140px; margin-top:10px;",
                    lapply(1:9, function(i) {
                      left <- 100 + (i-1)*120
                      bg <- if (!is.null(sel) && sel==i) "#4CAF50" else "#f0f0f0"
                      clr <- if (!is.null(sel) && sel==i) "white" else "black"
                      div(style=paste0("position:absolute; left:",left,"px; top:0;"),
                          tags$button(id=ns(paste0("box_",i)),
                                      class="action-button",
                                      style=paste0("width:30px;height:30px;padding:0;
                                                   background:",bg,";color:",clr,";"),
                                      HTML("&nbsp;")
                          )
                      )
                    })
                )
            )
        )
      )
    })

    observe({
      req(uiState()=="active", currentImageFile())
      output$mainImage <- renderImage({
        list(src = system.file("www", currentImageFile(), package="imres"),
             width = "1225px",  # 40% larger than before (800px * 1.4 = 1120px)
             height = "auto",
             alt=currentImageFile())
      }, deleteFile=FALSE)
    })

    # Update explanation text field when navigating
    observe({
      req(uiState() == "active")
      key <- selectionKey()

      # Load existing explanation if available
      if (key %in% names(state$descriptions)) {
        state$currentExplanation <- state$descriptions[[key]]
      } else {
        state$currentExplanation <- NULL
      }

      # Check if explanation is needed
      period <- period()
      sel <- state$selections[[key]]

      if (!is.null(sel) && !is.null(thresholds[[period]]) &&
          sel >= thresholds[[period]] && period != "Interim Review") {
        # Show explanation box with existing text if available
        output$explanationUI <- renderUI({
          textAreaInput(ns("explanation"),
                        "This rating is a bit higher than expected for your level of training (which may be deserving). Please take a moment to justify this rating.",
                        value = state$currentExplanation,
                        rows = 3,
                        width = "100%")
        })
      } else {
        # Clear explanation UI if not needed
        output$explanationUI <- renderUI(NULL)
      }
    })

    # navigation buttons - simplified and modified to handle explanation
    output$navigationButtons <- renderUI({
      req(uiState()=="active")
      key <- selectionKey()
      sel <- state$selections[[key]]
      period <- period()

      # Determine if we need explanation
      needsExplanation <- !is.null(sel) &&
        !is.null(thresholds[[period]]) &&
        sel >= thresholds[[period]] &&
        period != "Interim Review"

      # Determine if Next should be disabled
      disableNext <- is.null(sel) ||
        (needsExplanation && (is.null(input$explanation) ||
                                trimws(input$explanation) == ""))

      div(class="card",
          div(class="card-body",
              fluidRow(
                column(6, actionButton(ns("prev"), "Previous", class="btn-primary", width="100%")),
                column(6, actionButton(ns("next"), "Next",
                                       class=if(disableNext) "btn-primary disabled" else "btn-primary",
                                       width="100%"))
              )
          )
      )
    })

    # reacting to score clicks
    observe({
      req(uiState()=="active")
      for (i in 1:9) {
        local({
          ii <- i
          observeEvent(input[[paste0("box_",ii)]], {
            key    <- selectionKey()
            period <- period()
            overTh <- !is.null(thresholds[[period]]) && ii >= thresholds[[period]] && period != "Interim Review"

            # always save the numeric
            state$selections[[key]] <- ii

            if (!overTh) {
              # clear any leftover explain‐UI
              output$explanationUI <- renderUI(NULL)
              state$descriptions[[key]] <- NULL
              state$pendingSelection <- list(key=NULL, value=NULL)
              state$currentExplanation <- NULL
            } else {
              # require explanation inline
              state$pendingSelection <- list(key=key, value=ii)
              state$currentExplanation <- state$descriptions[[key]] # Existing explanation if any
              output$explanationUI <- renderUI({
                textAreaInput(ns("explanation"),
                              "This rating is a bit higher than expected for your level of training (which may be deserving). Please take a moment to justify this rating.",
                              value = state$currentExplanation,
                              rows = 3,
                              width = "100%")
              })
            }
          }, ignoreInit=TRUE)
        })
      }
    })

    # Watch for explanation changes and save them when they occur
    observe({
      req(input$explanation, state$pendingSelection$key)
      if(!is.null(input$explanation) && nzchar(trimws(input$explanation))) {
        state$descriptions[[state$pendingSelection$key]] <- input$explanation
        state$currentExplanation <- input$explanation
      }
    })

    # prev/next - with fixed "next" keyword issue and explanation handling
    observeEvent(input$prev, {
      # Save current explanation if it exists and is needed
      key <- selectionKey()
      if(!is.null(input$explanation) && nzchar(trimws(input$explanation)) &&
         !is.null(state$pendingSelection$key) && state$pendingSelection$key == key) {
        state$descriptions[[key]] <- input$explanation
      }

      if (state$currentImageIndex > 1) {
        state$currentImageIndex <- state$currentImageIndex - 1
      } else if (state$currentSetIndex > 1) {
        state$currentSetIndex <- state$currentSetIndex - 1
        state$currentImageIndex <- length(imageSets[[state$currentSetIndex]]$images)
      }

      # Clear pending selection when navigating
      state$pendingSelection <- list(key = NULL, value = NULL)
    })

    observeEvent(input[["next"]], {
      key <- selectionKey()

      if (is.null(state$selections[[key]])) {
        showNotification("Pick a score first", type="error")
      } else {
        # Check if explanation is needed
        period <- period()
        sel <- state$selections[[key]]
        needsExplanation <- !is.null(sel) && !is.null(thresholds[[period]]) &&
          sel >= thresholds[[period]] && period != "Interim Review"

        if (needsExplanation && (is.null(input$explanation) || trimws(input$explanation) == "")) {
          showNotification("Please provide an explanation for this rating", type="error")
        } else {
          # Save explanation if needed
          if (needsExplanation && !is.null(input$explanation) && nzchar(trimws(input$explanation))) {
            state$descriptions[[key]] <- input$explanation
          }

          # Navigate to next
          if (state$currentImageIndex < length(currentSet()$images)) {
            state$currentImageIndex <- state$currentImageIndex + 1
          } else if (state$currentSetIndex < length(imageSets)) {
            state$currentSetIndex <- state$currentSetIndex + 1
            state$currentImageIndex <- 1
          }

          # Clear pending selection when navigating
          state$pendingSelection <- list(key = NULL, value = NULL)
        }
      }
    })

    # Handle the done button to prevent "subscript out of bounds" error
    observeEvent(input$done, {
      # Just trigger the event without any subscript calls that might cause issues
    })

    # final return
    list(
      done   = reactive(input$done),
      scores = reactive(state$selections),
      desc   = reactive(state$descriptions)
    )
  })
}
