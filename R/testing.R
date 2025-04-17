

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(httr)

# Load your test data (full dataset)
test_data <- readRDS("test_data.rds")
unique(test_data$access_code)
# test_data should have columns: access_code, name, coach, etc.
#
#
rdm_token <- 'F1F2E0450F3A97F88E031E89432C05B2'
eval_token <- '3955FF9AF4681B2CB55488D11B56B2D5'
url <- "https://redcapsurvey.slu.edu/api/"
rdm_dict <- get_data_dict(rdm_token, url)

all_data <- full_api_pull(rdm_token, url)


ass_dat <- full_api_pull(eval_token, url)
ass_dat <- wrangle_assessment_data(ass_dat)
rdm_dat <- forms_api_pull(rdm_token, url, 'resident_data', 'faculty_evaluation')


# Function to safely pull resident data from REDCap API
get_resident_data <- function() {
  tryCatch({
    ass_dat <- full_api_pull(eval_token, url)
    ass_dat <- wrangle_assessment_data(ass_dat)
    rdm_dat <- forms_api_pull(rdm_token, url, 'resident_data', 'faculty_evaluation')
    return(create_res_data(ass_dat, rdm_dat))
  }, error = function(e) {
    cat("Error in API pull:", e$message, "\n")
    return(NULL)
  })
}

### Process Milestone Data:
# Step 1: Pull all milestone data
miles <- get_all_milestones(rdm_token, url)

# Step 2: Fill in resident data (name, record_id)
miles <- fill_missing_resident_data(miles)

# Step 3: Process program milestones
p_miles <- process_milestones(miles, type = "program")

# Step 4: Process self milestones
s_miles <- process_milestones(miles, type = "self")

# Load resident and milestone data
resident_data <- get_resident_data()

ui <- fluidPage(
  theme = bs_theme(version = 5),
  shinyjs::useShinyjs(),

  div(
    id = "global_access_code",
    textInput("access_code_input", "Enter Access Code:", placeholder = "e.g., ABC123")
  ),

  mod_self_assessment_ui("self_assess")
)

server <- function(input, output, session) {
  # Reactive that returns the verified resident's name
  verified_resident_name <- reactive({
    req(input$access_code_input)
    res <- test_data %>% filter(access_code == input$access_code_input)
    if (nrow(res) == 0) return(NULL)
    res$name[1]
  })

  # Auto-verify when a valid code is entered
  observe({
    if (!is.null(verified_resident_name())) {
      shinyjs::hide("global_access_code")
      showNotification(paste("Access code verified for", verified_resident_name()), type = "message")
    }
  })

  mod_self_assessment_server("self_assess",
                             resident_name = verified_resident_name,
                             full_data = test_data,
                             coach_data = NULL,
                             redcap_uri = url,
                             redcap_token = rdm_token
                             )

}




## Access codes "WdyPEh" "7uG6gS" "NYqoWH" "ujflcv" "i9MmGj" "TuAjIG"
rm(list = ls())
devtools::load_all()
shinyApp(ui, server)
