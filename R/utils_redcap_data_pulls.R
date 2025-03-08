#' @title RedCap API Utilities
#'
#' @description This file contains utility functions for interacting with the RedCap API.
#'
#' @details
#' Function Overview:
#' - `redcap_api_call()`: Handles the generic API call to RedCap.
#' - `full_api_pull()`: Pulls all data from RedCap.
#' - `forms_api_pull()`: Pulls data for specific forms from RedCap.
#' - `fetch_new_redcap_data()`: Incremental data pull from RedCap based on timestamps.
#' - `get_links_api()`: Gets survey links for a specific instrument and record.
#' - `generate_survey_link()`: Generates a new survey link for a specific record and instrument.
#'

# -----------------------------------------------------------------------
#  Function: redcap_api_call
#  Purpose:  Handle a generic API call to RedCap
# -----------------------------------------------------------------------

#' @title Generic API Call to RedCap
#'
#' @description Makes a generic POST request to RedCap API with provided form data.
#'
#' @param token A string. The API token for accessing the RedCap project.
#' @param url A string. The API URL for the RedCap project.
#' @param formData A list. The form data to send in the POST request.
#'
#' @return A dataframe of the response data.
#' @export
redcap_api_call <- function(token, url, formData) {
  if (missing(token) || missing(url)) {
    stop("Both 'token' and 'url' must be provided.")
  }

  # Make sure these parameters are included
  formData$exportSurveyFields <- 'true'
  formData$csvDelimiter <- ''

  httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  response <- httr::POST(url, body = formData, encode = "form")
  if (httr::http_status(response)$category != "Success") {
    stop("Failed to pull data from RedCap. Check your API URL, token, or request parameters.")
  }

  csv_content <- httr::content(response, as = "text", encoding = "UTF-8")

  data <- tryCatch({
    readr::read_csv(csv_content, col_types = readr::cols(.default = "c"))
  }, error = function(e) {
    message("Failed to parse the CSV content from RedCap.")
    return(data.frame())
  })

  # Ensure `record_id` is always character
  if ("record_id" %in% colnames(data)) {
    data <- data %>% dplyr::mutate(record_id = as.character(record_id))
  }

  return(data)
}

# -----------------------------------------------------------------------
#  Function: full_api_pull
#  Purpose:  Pull all data from RedCap
# -----------------------------------------------------------------------

#' @title Full API Pull
#'
#' @description Pulls all data from RedCap using the provided token and API URL.
#'
#' @param token A string. The API token for accessing the RedCap project.
#' @param url A string. The API URL for the RedCap project.
#'
#' @return A dataframe of the full data.
#' @export
full_api_pull <- function(token, url) {
  formData <- list(
    "token" = token,
    "content" = 'record',
    "action" = 'export',
    "format" = 'csv',
    "type" = 'flat',
    "rawOrLabel" = 'label',
    "rawOrLabelHeaders" = 'raw',
    "exportCheckboxLabel" = 'true',
    "exportSurveyFields" = 'true',
    "exportDataAccessGroups" = 'false',
    "returnFormat" = 'csv',
    "csvDelimiter" = ''
  )

  data <- redcap_api_call(token, url, formData)
  return(data)
}

# -----------------------------------------------------------------------
#  Function: forms_api_pull
#  Purpose:  Pull data for a variable number of forms
# -----------------------------------------------------------------------

#' @title Forms API Pull
#'
#' @description Pulls data for specific forms from RedCap.
#'
#' @param token A string. The API token for accessing the RedCap project.
#' @param url A string. The API URL for the RedCap project.
#' @param ... Character names of forms to pull.
#' @param fields A character vector of field names to pull.
#'
#' @return A dataframe of the requested forms.
#' @export
forms_api_pull <- function(token, url, ..., fields = NULL) {
  forms <- list(...)

  # Default formData structure (matches the original formData)
  formData <- list(
    "token" = token,
    "content" = 'record',
    "action" = 'export',
    "format" = 'csv',
    "type" = 'flat',
    "rawOrLabel" = 'label',
    "rawOrLabelHeaders" = 'raw',
    "exportCheckboxLabel" = 'true',
    "exportSurveyFields" = 'true',
    "exportDataAccessGroups" = 'false',
    "returnFormat" = 'csv',
    "csvDelimiter" = ''  # Ensures proper CSV formatting
  )

  # Add specific fields if requested
  if (!is.null(fields)) {
    for (i in seq_along(fields)) {
      formData[[paste0('fields[', i - 1, ']')]] <- fields[[i]]
    }
  }

  # Dynamically add forms to formData
  for (i in seq_along(forms)) {
    formData[[paste0('forms[', i - 1, ']')]] <- forms[[i]]
  }

  response <- httr::POST(url, body = formData, encode = "form")

  if (httr::http_status(response)$category != "Success") {
    stop("Failed to pull data for forms from RedCap. Check the API URL, token, or form names.")
  }

  csv_content <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- tryCatch({
    readr::read_csv(csv_content, col_types = readr::cols(.default = "c"))
  }, error = function(e) {
    message("Failed to parse the CSV content from RedCap.")
    return(data.frame())
  })

  # Standardize column names
  data <- data %>% dplyr::rename_with(~ gsub("[^a-zA-Z0-9_]", "", .x))

  # Coerce record_id to character
  if ("record_id" %in% colnames(data)) {
    data <- data %>% dplyr::mutate(record_id = as.character(record_id))
  }

  return(data)
}

# -----------------------------------------------------------------------
#  Function: fetch_new_redcap_data
#  Purpose:  Pull data updated since the most recent timestamp
# -----------------------------------------------------------------------

#' @title Fetch New RedCap Data
#'
#' @description
#' Pulls and wrangles new data from RedCap based on a timestamp column.
#' This function retrieves new data from the RedCap API for records that have
#' a timestamp greater than or equal to the most recent timestamp in the existing dataset.
#' It then combines the new data with the existing data, applies a wrangling
#' function to the combined dataset, and removes duplicate records.
#'
#' @param token A string. The API token for accessing the RedCap project.
#' @param url A string. The API URL for the RedCap project.
#' @param data A data frame. The existing dataset loaded in the app, containing previous records.
#' @param timestamp_column A string. The name of the timestamp column in the dataset used to filter new records.
#' @param wrangle_function A function. The function to process the combined data.
#' @param api_function A function. The API function to call (e.g., `redcap_api_call`, `forms_api_pull`).
#' @param ... Additional arguments to be passed to the API function.
#'
#' @details
#' The function retrieves the most recent date in the `timestamp_column` of the
#' existing dataset and uses it to filter records in RedCap using a `filterLogic` parameter.
#' Records with timestamps greater than or equal to the most recent date are pulled from
#' RedCap and combined with the existing dataset.
#'
#' **Key Features:**
#' 1. Ensures that **wrangling occurs even if no new data** is returned from the API.
#' 2. **Combines and wrangles** the old and new data together.
#' 3. **Removes duplicate records** using `dplyr::distinct()`.
#'
#' @return
#' A data frame containing the combined and wrangled dataset.
#' This dataset includes new records (if available) along with the previous data.
#'
#' @note
#' - The `timestamp_column` must exist in the **data** parameter; otherwise, the function will stop with an error.
#' - If the **wrangle_function** encounters an error, the function will print debug information and stop execution.
#' - The **dplyr::distinct()** function is applied to the final dataset to ensure there are no duplicate rows.
#'
#' @seealso
#' - [redcap_api_call()]: Used to call the RedCap API.
#' - [dplyr::distinct()]: Used to remove duplicate rows from the combined dataset.
#'
#'
#' @export
fetch_new_redcap_data <- function(token, url, data, timestamp_column, wrangle_function, api_function, ...) {

  # Step 1: Check if the timestamp column exists
  if (!timestamp_column %in% colnames(data)) {
    stop(paste("The timestamp column", timestamp_column, "is not in the provided dataset. Available columns are:", paste(colnames(data), collapse = ", ")))
  }

  # Step 2: Find the most recent timestamp - using POSIXct for precise timestamp comparison
  most_recent_timestamp <- max(as.POSIXct(data[[timestamp_column]], format = "%Y-%m-%d %H:%M:%S"), na.rm = TRUE)
  if (is.na(most_recent_timestamp)) {
    stop("Could not determine the most recent timestamp. Ensure that the timestamp column contains valid datetime values.")
  }

  # Debug message to show what we're working with
  message("Most recent timestamp in existing data: ", format(most_recent_timestamp, "%Y-%m-%d %H:%M:%S"))

  # Format filter logic for API request - using strictly greater than
  filter_logic <- paste0("[", timestamp_column, "] > '", format(most_recent_timestamp, "%Y-%m-%d %H:%M:%S"), "'")

  formData <- list(
    "token" = token,
    "content" = 'record',
    "action" = 'export',
    "format" = 'csv',
    "type" = 'flat',
    "rawOrLabel" = 'label',
    "rawOrLabelHeaders" = 'raw',
    "exportCheckboxLabel" = 'true',
    "exportSurveyFields" = 'true',
    "exportDataAccessGroups" = 'false',
    "returnFormat" = 'csv',
    "filterLogic" = filter_logic
  )

  message("Calling API with filter logic: ", filter_logic)

  # Step 3: Call the API to get new data
  new_data <- api_function(token = token, url = url, formData = formData, ...)

  # Debug message for new data
  if (!is.null(new_data) && nrow(new_data) > 0) {
    message("New data timestamps range: ",
            min(new_data[[timestamp_column]]), " to ",
            max(new_data[[timestamp_column]]))
  }

  if (is.null(new_data) || nrow(new_data) == 0) {
    message("No new records from the API. Returning original data as-is.")
    return(data)
  }

  message("New data pulled from API, number of rows: ", nrow(new_data))

  # Step 4: Combine the existing data with the new data
  combined_data <- dplyr::bind_rows(data, new_data)

  # Step 5: Apply the wrangle function
  wrangled_data <- tryCatch({
    result <- wrangle_function(combined_data)
    if (is.null(result)) {
      message("wrangle_function returned NULL, using combined_data instead")
      combined_data
    } else {
      result
    }
  }, error = function(e) {
    message("Error during wrangle_function execution: ", e$message)
    combined_data
  })

  # Step 6: Remove duplicate rows based on all columns
  updated_data <- dplyr::distinct(wrangled_data)

  message("Final updated data has ", nrow(updated_data), " rows (original had ", nrow(data), " rows)")
  return(updated_data)
}

# -----------------------------------------------------------------------
#  Function: get_links_api
#  Purpose:  Get survey links from RedCap
# -----------------------------------------------------------------------

#' @title Get Links API
#'
#' @description Pulls survey links from RedCap.
#'
#' @param token A string. The API token for accessing the RedCap project.
#' @param url A string. The API URL for the RedCap project.
#' @param record_id A string. The record ID.
#' @param instrument A string. The instrument name.
#'
#' @return A survey link.
#' @export
get_links_api <- function(token, url, record_id, instrument) {
  formData <- list(
    "token" = token,
    "content" = 'surveyLink',
    "record" = record_id,
    "instrument" = instrument
  )

  response <- httr::POST(url, body = formData, encode = "form")
  link <- httr::content(response)
  return(link)
}

# -----------------------------------------------------------------------
#  Function: generate_survey_link
#  Purpose:  Generate a new survey link for a record and instrument
# -----------------------------------------------------------------------

#' @title Generate Survey Link
#'
#' @description Generates a new survey link for a given record and instrument.
#'
#' @param record_id A string. The record ID.
#' @param instrument A string. The instrument name.
#' @param url A string. The API URL for the RedCap project.
#' @param token A string. The API token for accessing the RedCap project.
#'
#' @return A list containing the survey link.
#' @export
generate_survey_link <- function(record_id, instrument, url, token) {
  formData <- list(
    "token" = token,
    "content" = 'surveyLink',
    "record" = record_id,
    "instrument" = instrument
  )

  response <- httr::POST(url, body = formData, encode = "form")
  link <- httr::content(response)
  return(link)
}

#' Extract URL from HTML Response
#'
#' This function extracts a URL from an HTML response, typically from a response
#' body where the URL is wrapped inside HTML tags like `<p>...</p>` or `<body>...</body>`.
#'
#' @param html_content A character string containing the raw HTML response,
#' typically returned from an API request where the URL is embedded inside
#' tags like `<p>...</p>`.
#'
#' @return A character string containing the extracted URL. If no URL is
#' found, the function returns `NA`.
#'
#' @examples
#' # Example 1: Extract URL from a simple HTML response
#' html_content <- "<body><p>https://redcapsurvey.slu.edu/surveys/?s=pmiXnIHKUavNEp8p</p></body>"
#' extract_url(html_content)
#'
#' # Example 2: Extract URL from a more complex HTML response
#' html_content <- "<html><body><p>https://example.com/test?param=123</p></body></html>"
#' extract_url(html_content)
#'
#' # Example 3: No URL in the content
#' html_content <- "<body><p>No URL here</p></body>"
#' extract_url(html_content)  # Returns NA
#'
#' @export
extract_url <- function(html_content) {
  url <- stringr::str_extract(html_content, "https?://[A-Za-z0-9./?&=_-]+")
  return(url)
}


#' Get Data Dictionary
#'
#' @export
get_data_dict <-function(token, url) {
  formData <- list("token"=token,
                   content='metadata',
                   format='csv',
                   returnFormat='json'
  )
  data <- redcap_api_call(token, url, formData)
  return(data)
}



