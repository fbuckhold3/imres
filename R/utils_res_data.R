#' Utilities for Wrangling Resident Assessment and Faculty Evaluation Data
#'
#' This file contains utility functions to clean, organize, and merge resident data from multiple sources.
#' The wrangling is essential for ensuring complete and accurate data prior to visualization and analysis.
#' 
#' ## Function Overview:
#' - `wrangle_assessment_data`: Cleans and processes resident assessment data.
#' - `create_res_data`: Creates a merged dataframe from assessment data and resident identifier (rdm) data.
#'
#' These utilities are used to prepare data for Shiny apps and data visualizations.
#'


# -------------------------------------------------------------------------
# Function: wrangle_assessment_data
# Purpose: Clean data from the resident assessment data pull
# -------------------------------------------------------------------------

#' Wrangle Resident Assessment Data
#'
#' Cleans and processes raw resident assessment data by coalescing columns, 
#' creating week-based evaluation bins, and identifying academic years and PGY levels.
#' 
#' **Key Steps:**
#' - Converts and formats assessment dates.
#' - Coalesces multiple redundant "name" and "rotation" columns into single columns.
#' - Determines the academic year and PGY level of each resident at the time of assessment.
#' - Removes unnecessary columns.
#'
#' @param data A dataframe containing the raw resident assessment data.
#'
#' @return A cleaned and wrangled dataframe with consistent 'name' and 'record_id' columns.
#' @export
#'
#' @examples 
#' \dontrun{
#'  raw_data <- read.csv("path/to/assessment_data.csv")
#'  cleaned_data <- wrangle_assessment_data(raw_data)
#'  print(cleaned_data)
#' }
wrangle_assessment_data <- function(data) {
  
  # 1️⃣ Convert dates, create week-based bins for evaluations
  data$ass_date <- lubridate::ymd(data$ass_date)
  data$week <- lubridate::isoweek(data$ass_date)
  data$year <- lubridate::isoyear(data$ass_date)
  data$Date <- format(data$ass_date, "%m/%d/%Y")
  
  # 2️⃣ Coalesce multiple "name", "rotation", and evaluation columns
  t2 <- data %>%
    mutate(
      name = coalesce(rot_2223, rot_2324, rot_2425, rot_2526, rot_2627, rot_2728, rot_2829, 
                      class_23, class_24, class_25, class_26, class_27, class_28, class_29, 
                      class_30, class_31),
      Rotation = coalesce(gen_clin_type, int_rot_sluhip, ip_res_rotation),
      eval_type = coalesce(int_ip_eval_type, res_ip_eval_type, cc_obs_or_sum),
      obs_pe_type2 = coalesce(obs_pe_type2___1, obs_pe_type2___2, obs_pe_type2___3, obs_pe_type2___4, 
                              obs_pe_type2___5, obs_pe_type2___6, obs_pe_type2___8, obs_pe_type2___7, 
                              obs_pe_type2_other),
      obs_pat_type = coalesce(obs_pat_type___1, obs_pat_type___2, obs_pat_type___3, obs_pat_type___4, 
                              obs_pat_type___5, obs_pat_type___6),
      Evaluator = coalesce(cc_csm_att, cc_va_att, slu_gim_att, bronze_att, card_att, ace_att, 
                           pulm_att, endo_att, ho_att, id_att, neph_att),
      x = lubridate::make_date(year),
      weekyr = x + lubridate::weeks(week - 1),
      Evaluator = ifelse(Evaluator %in% c("NA", "(not listed)"), att_name, Evaluator)
    )
  
  # 3️⃣ Add academic year, start year, and PGY level
  ass_dat <- t2 %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    mutate(
      start_year = case_when(
        !is.na(name) & name == class_23 ~ 2020,
        !is.na(name) & name == class_24 ~ 2021,
        !is.na(name) & name == class_25 ~ 2022,
        !is.na(name) & name == class_26 ~ 2023,
        !is.na(name) & name == class_27 ~ 2024,
        !is.na(name) & name == class_28 ~ 2025,
        !is.na(name) & name == class_29 ~ 2026,
        !is.na(name) & name == class_30 ~ 2027,
        !is.na(name) & name == class_31 ~ 2028,
        TRUE ~ NA_integer_
      )
    ) %>%
    mutate(
      academic_year_start = if_else(
        !is.na(Date), 
        if_else(lubridate::month(Date) >= 7, lubridate::year(Date), lubridate::year(Date) - 1), 
        NA_integer_
      )
    ) %>%
    mutate(
      Level = case_when(
        academic_year_start == start_year ~ "Intern",
        academic_year_start == start_year + 1 ~ "PGY2",
        academic_year_start == start_year + 2 ~ "PGY3",
        TRUE ~ NA_character_
      )
    )
  
  # 4️⃣ Remove unnecessary columns
  ass_dat <- 
    ass_dat %>%
    select(-c(rot_2223, rot_2324, rot_2425, rot_2526, rot_2627, rot_2728, rot_2829, 
              class_23, class_24, class_25, class_26, class_27, class_28, class_29, 
              class_30, class_31, gen_clin_type, int_rot_sluhip, ip_res_rotation, 
              int_ip_eval_type, res_ip_eval_type, cc_obs_or_sum, obs_pe_type2___1, 
              obs_pe_type2___2, obs_pe_type2___3, obs_pe_type2___4, obs_pe_type2___5, 
              obs_pe_type2___6, obs_pe_type2___8, obs_pe_type2___7, obs_pe_type2_other, 
              x, obs_pat_type___1, obs_pat_type___2, obs_pat_type___3, obs_pat_type___4, 
              obs_pat_type___5, obs_pat_type___6, record_id, redcap_repeat_instrument, redcap_repeat_instance, redcap_survey_identifier, y1, y2, y3, y4, y5, y6, y7, res_year_2223, res_year_2324, res_year_2425, res_year_2526, res_year_2627, res_year_2728, res_year_2829))
  
  return(ass_dat)
}


# -------------------------------------------------------------------------
# Function: create_res_data
# Purpose: Create a merged dataset from assessment and resident identifier (rdm) data
# -------------------------------------------------------------------------

#' Create Resident Dataframe
#'
#' Combines and processes the assessment data with resident name identifier data 
#' to ensure each row has a complete 'name' and 'record_id'. 
#' Also, fills in 'att' (attending) and 'rot' (rotation) columns.
#'
#' @param assess_data A dataframe of resident assessment data. Must have 'name' instead of 'Resident'.
#' @param rdm_data A dataframe containing name ↔ record_id pairs, with 'name' and 'record_id'.
#' 
#' @return A dataframe with complete 'name' and 'record_id' for every row.
#' @export
#'
#' @examples 
#' \dontrun{
#'  assess_data <- read.csv("path/to/assess_data.csv")
#'  rdm_data <- read.csv("path/to/rdm_data.csv")
#'  cleaned_data <- create_res_data(assess_data, rdm_data)
#'  print(cleaned_data)
#' }
create_res_data <- function(assess_data, rdm_data) {
  
  # 1️⃣ Combine the assessment data with the resident name data
  df <- bind_rows(assess_data, rdm_data)
  
  # 2️⃣ Fill missing 'name' and 'record_id' using fill_missing_resident_data()
  df <- fill_missing_resident_data(df, remove_incomplete = TRUE)
  
  # 3️⃣ Fill 'att' (attending) and 'rot' (rotation) using coalesce and ifelse logic
  df <- df %>%
    mutate(
      att = coalesce(
        cards_att, gim_att, endo_att, gi_att, ho_att, 
        pal_ger_att, id_att, pul_cc_att, neph_att, 
        sluh_gen_floor_att, micu_att, ace_att, cc_att, 
        bronze_att, card_att, spec_att, fellow_name, cons_other_att
      ),
      att = ifelse(att == 'Other / not listed', cons_other_att, att),
      rot = ifelse(rotation == 'Other (Please specify)', other_rot, rotation)
    )
  
  # 4️⃣ Remove archived residents using archive()
  df <- df %>% filter(!name %in% archive(df))
  
  return(df)
}

