#' Calculate Resident Level
#'
#' Determines the level of residents (Intern, PGY2, PGY3) based on their type and graduation year.
#'
#' @param coach_data A dataset containing resident data, including `type` and `grad_yr`
#'
#' @return A dataset with an additional `Level` column
#' @export
calculate_resident_level <- function(coach_data) {
  # Debug: Print class and structure of input
  print("Inside calculate_resident_level")
  print(class(coach_data))
  print(str(coach_data))
  
  if (!inherits(coach_data, "data.frame")) {
    stop("Input to calculate_resident_level must be a data frame or tibble.")
  }
  
  # Get today's date and current academic year
  current_date <- Sys.Date()
  current_academic_year <- ifelse(format(current_date, "%m-%d") >= "07-01", 
                                  as.numeric(format(current_date, "%Y")), 
                                  as.numeric(format(current_date, "%Y")) - 1)
  
  coach_data <- coach_data %>%
    mutate(
      grad_yr = as.numeric(grad_yr),  # Ensure grad_yr is numeric
      Level = case_when(
        type == "Preliminary" ~ "Intern",  # Prelim residents are always Interns
        type == "Categorical" & grad_yr == current_academic_year + 3 ~ "Intern",  # PGY1 (Intern) 
        type == "Categorical" & grad_yr == current_academic_year + 2 ~ "PGY2",    # PGY2
        type == "Categorical" & grad_yr == current_academic_year + 1 ~ "PGY3",    # PGY3
        TRUE ~ NA_character_
      )
    )
  
  return(coach_data)
}
