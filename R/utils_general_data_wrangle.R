#' General Data Wrangling Utilities
#'
#' This file contains utility functions for data cleaning prior to visualization and analysis.
#'
#' ## Function Overview:
#' - `fill_missing_resident_data`: Ensures each row in a dataset has a 'name' and 'record_id'.
#' - `merge_old_new_data`: Merges old data saved as an RDS file with new REDCap API data.
#' - `archive`: Filters out archived residents from datasets.
#'
#' These utilities are essential for ensuring complete and accurate data for visualization and analysis.

# -------------------------------------------------------------------------
# Function: fill_missing_resident_data
# Purpose: Ensure that each row in a dataframe has a 'name' and 'record_id'.
# -------------------------------------------------------------------------

#' Fill Missing Names and Record IDs
#'
#' Ensures that each row in the provided dataframe has complete 'name' and 'record_id' columns.
#' The function fills missing 'name' and 'record_id' using lookup logic, where information
#' is propagated within the dataframe using `left_join()` and `tidyr::fill()`.
#'
#' @param data A dataframe containing the resident data.
#' @param remove_incomplete Logical. If TRUE, removes any rows that still have missing 'name' or 'record_id'.
#'
#' @return A dataframe with filled 'name' and 'record_id' columns.
#' @export
#'
#' @examples
#' \dontrun{
#'  data <- tibble::tibble(record_id = c(1, 2, 3, NA), name = c(NA, "John Doe", NA, "Jane Smith"))
#'  filled_data <- fill_missing_resident_data(data)
#'  print(filled_data)
#' }
fill_missing_resident_data <- function(data, remove_incomplete = TRUE) {

  name_lookup <- data %>%
    mutate(name = na_if(name, ""), name = na_if(name, "NULL")) %>%
    filter(!is.na(name) & !is.na(record_id)) %>%
    distinct(record_id, name)

  data <- data %>%
    left_join(name_lookup, by = "record_id", suffix = c("", "_names")) %>%
    mutate(name = coalesce(name, name_names)) %>%
    select(-name_names)

  data <- data %>%
    left_join(name_lookup, by = "name", suffix = c("", "_ids")) %>%
    mutate(record_id = coalesce(record_id, record_id_ids)) %>%
    select(-record_id_ids)

  data <- data %>%
    group_by(record_id) %>%
    tidyr::fill(name, .direction = "downup") %>%
    ungroup() %>%
    group_by(name) %>%
    tidyr::fill(record_id, .direction = "downup") %>%
    ungroup()

  if (remove_incomplete) {
    data <- data %>% filter(!is.na(record_id) & !is.na(name))
  }

  return(data)
}

# -------------------------------------------------------------------------
# Function: merge_old_new_data
# Purpose: Merge old RDS data with new data from the REDCap API
# -------------------------------------------------------------------------

#' Merge Old and New Data
#'
#' Merges old data (saved as an `.rds` file) with new data pulled from the REDCap API.
#' It performs a full join on a user-defined key (default is "record_id") and
#' removes any duplicate rows from the combined dataset.
#'
#' @param old_data_path A string. The file path to the `.rds` file containing the old data.
#' @param token A string. The API token for accessing the REDCap project.
#' @param url A string. The API URL for the REDCap project.
#' @param timestamp_columns A character vector. The names of timestamp columns used to check for updates.
#' @param merge_key A string. The key to use for merging old and new data (default is "record_id").
#'
#' @return A data frame containing the merged data, with duplicates removed.
#' @export
#'
#' @examples
#' \dontrun{
#'  merged_data <- merge_old_new_data(
#'    old_data_path = "data/old_data.rds",
#'    token = "your_redcap_token",
#'    url = "https://redcap.yourinstitution.edu/api/",
#'    timestamp_columns = c("event_timestamp", "another_timestamp")
#'  )
#' }
merge_old_new_data <- function(old_data_path, token, url, timestamp_columns, merge_key = "record_id") {

  if (!file.exists(old_data_path)) {
    stop("Old data file not found: ", old_data_path)
  }

  old_data <- readRDS(old_data_path)
  new_data <- fetch_new_redcap_data(token = token, url = url, timestamp_columns = timestamp_columns)

  if (is.null(new_data)) {
    message("No new data found. Returning old data only.")
    return(old_data)
  }

  combined_data <- dplyr::full_join(old_data, new_data, by = merge_key)
  combined_data <- combined_data %>% dplyr::distinct()

  return(combined_data)
}

# -------------------------------------------------------------------------
# Function: archive
# Purpose: Filter out archived residents from datasets
# -------------------------------------------------------------------------

#' Archive Filter for Residents
#'
#' Filters out archived residents from a dataset. It assumes the
#' dataset contains a 'res_archive' column with the value "Yes" for archived residents.
#'
#' @param data A dataframe. The dataset containing the resident information.
#'
#' @return A character vector of archived resident names.
#' @export
#'
#' @examples
#' \dontrun{
#'  data <- tibble::tibble(name = c("John", "Jane", "Jake"), res_archive = c("No", "Yes", "No"))
#'  archived_residents <- archive(data)
#'  print(archived_residents)
#' }
archive <- function(data) {
  archived_residents <- data %>%
    filter(res_archive == "Yes") %>%
    pull(name)

  return(archived_residents)
}

# -------------------------------------------------------------------------
# Function: remove_archived_residents
# Purpose: Remove rows of archived residents from datasets
# -------------------------------------------------------------------------

#' Remove Archived Residents
#'
#' Removes rows of archived residents from a dataset, based on the `archive` function.
#'
#' @param data A dataframe. The dataset containing the resident information.
#'
#' @return A dataframe with archived residents removed.
#' @export
#'
#' @examples
#' \dontrun{
#'  data <- tibble::tibble(name = c("John", "Jane", "Jake"), res_archive = c("No", "Yes", "No"))
#'  data_no_archives <- remove_archived_residents(data)
#'  print(data_no_archives)
#' }
remove_archived_residents <- function(data) {
  # Get list of archived residents
  archived_residents <- archive(data)
  # Remove rows where name is in the list of archived residents
  data <- data %>% filter(!name %in% archived_residents)
  return(data)
}

# -------------------------------------------------------------------------
# Function: get_ccc_session
# Purpose: Get session value based on period
# -------------------------------------------------------------------------

#' Get CCC Session Value
#'
#' Returns the session value (`ccc_session`) for a specific period.
#'
#' @param period_value A character string representing the period (e.g., "Mid Intern", "End Intern").
#'
#' @return An integer corresponding to the `ccc_session` value, or NA if no match is found.
#' @export
#'
#' @examples
#' \dontrun{
#'  session <- get_ccc_session("Mid Intern")
#'  print(session) # Returns 1
#' }
get_ccc_session <- function(period_value) {
  switch(
    period_value,
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "Graduation" = 6,
    "Intern Intro" = 7,
    NA
  )
}
