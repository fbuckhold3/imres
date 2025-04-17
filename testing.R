

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

rdm_dict %>%
  filter(field_name %in% c("hs_mo","college_mo","med_mo")) %>%
  pull(select_choices_or_calculations) %>%
  cat(sep = "\n")

# Function to safely pull resident data from REDCap API
get_resident_data <- function() {
  tryCatch({
    ass_dat <- full_api_pull(eval_token, url)
    ass_dat <- wrangle_assessment_data(ass_dat)
    rdm_dat <- forms_api_pull(rdm_token, url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship')
    return(create_res_data(ass_dat, rdm_dat))
  }, error = function(e) {
    cat("Error in API pull:", e$message, "\n")
    return(NULL)
  })
}

# Load resident and milestone data
resident_data <- get_resident_data()

### Process Milestone Data:
# Step 1: Pull all milestone data
miles <- get_all_milestones(rdm_token, url)

# Step 2: Fill in resident data (name, record_id)
miles <- fill_missing_resident_data(miles)

# Step 3: Process program milestones
p_miles <- process_milestones(miles, type = "program")

# Step 4: Process self milestones
s_miles <- process_milestones(miles, type = "self")



ui <- fluidPage(
  theme = bs_theme(version = 5),
  useShinyjs(),

  # hide any div with class hidden-card by default
  tags$style(".hidden-card { display: none; }") ,

  # access code gate
  div(
    id = "global_access_code",
    textInput("access_code_input", "Enter Access Code:", placeholder = "ABC123")
  ),

  # self-assessment module UI
  mod_self_assessment_ui("self_assess")
)

server <- function(input, output, session) {
  # verify access code
  verified_resident_name <- reactive({
    req(input$access_code_input)
    df <- resident_data %>% filter(access_code == input$access_code_input)
    if (nrow(df)==0) return(NULL)
    df$name[1]
  })

  observe({
    if (!is.null(verified_resident_name())) {
      hide("global_access_code")
      showNotification(
        paste("Access code verified for", verified_resident_name()),
        type = "message"
      )
    }
  })
  # launch module
  mod_self_assessment_server(
    id            = "self_assess",
    resident_name = verified_resident_name,
    resident_data = resident_data,
    s_miles       = s_miles,
    p_miles       = p_miles,
    data_dict     = rdm_dict,
    redcap_uri    = url,
    redcap_token  = rdm_token
  )
}





## Access codes "WdyPEh" "7uG6gS" "NYqoWH" "ujflcv" "i9MmGj" "TuAjIG"
rm(list = ls())
devtools::load_all()

shinyApp(ui, server)
