#' @title REDCap Submission Utility Functions
#'
#' @description Utility functions to interact with REDCap API.
#' These functions support pulling data, calculating instance numbers, and submitting data to REDCap.
#'
#' @details
#' This file includes the following key functions:
#'
#' - **`fetch_record_id()`**: Retrieve the `record_id` for a given resident name.
#' - **`generate_new_instance()`**: Get the next instance for a specific instrument and record.
#' - **`submit_to_redcap()`**: Submit a new record to REDCap.
#' - **`fetch_existing_data()`**: Pull existing data for a specific period, record, and instrument.
#' - **`validate_inputs()`**: Validate that user inputs are within the required range.
#' - **`update_inputs()`**: Update Shiny UI inputs with existing REDCap data.
#'

#' Get CCC Session Value
#'
#' @description Get the `ccc_session` value based on the period.
#'
#' @param period_value A character string representing the period (e.g., "Mid Intern", "Interim Review").
#'
#' @return An integer corresponding to the `ccc_session` value.
#'
#' @export
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




# -----------------------------------------------------------------------
#  Function: fetch_record_id
# -----------------------------------------------------------------------
#' @title Fetch Record ID
#'
#' @description Fetch the record ID for a given resident name.
#'
#' @param resident A character. The resident's name.
#' @param coach_data A dataframe containing resident names and corresponding record_ids.
#' @param redcap_uri A string. The REDCap API URL.
#' @param token A string. The REDCap API token.
#'
#' @return The record ID for the resident, or NULL if not found.
#'
#' @export
fetch_record_id <- function(resident, coach_data, redcap_uri, token) {
  # Check if 'coach_data' exists and contains the relevant information
  if (!is.null(coach_data) && "name" %in% colnames(coach_data) && "record_id" %in% colnames(coach_data)) {
    record_id <- coach_data %>%
      dplyr::filter(name == resident) %>%
      dplyr::pull(record_id) %>%
      unique() # Ensure unique IDs only

    if (length(record_id) > 0) {
      record_id <- record_id[1]  # 🚩 Only take the first one, if multiples exist
      message("Fetched record_id from coach_data: ", record_id)
      return(record_id)
    }
  }

  # If no record_id found in local data, try to pull it from REDCap
  data <- tryCatch({
    REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_uri,
      token = token,
      fields = c("record_id", "name")
    )$data
  }, error = function(e) {
    message("⚠️ Error pulling record_id from REDCap: ", e$message)
    return(NULL)
  })

  # Check if 'name' column exists in pulled data
  if (!"name" %in% colnames(data) || nrow(data) == 0) {
    message(" No name column found in REDCap data, or no data available.")
    return(NULL)
  }

  # Filter and extract the record_id
  record_id <- data %>%
    dplyr::filter(name == resident) %>%
    dplyr::pull(record_id) %>%
    unique() # Ensure unique IDs only

  if (length(record_id) > 0) {
    record_id <- record_id[1]  # 🚩 Only take the first one, if multiples exist
    message("etched record_id from REDCap: ", record_id)
    return(record_id)
  }

  message(" No record_id found for resident: ", resident)
  return(NULL)
}

# -----------------------------------------------------------------------
#  Function: generate_new_instance
# -----------------------------------------------------------------------
#' @title Generate New Instance
#'
#' @description Calculate the next available instance for a specific instrument and record.
#'
#' @param record_id A numeric. The REDCap record ID.
#' @param instrument A string. The REDCap instrument name.
#' @param redcap_uri A string. The REDCap API URL.
#' @param token A string. The REDCap API token.
#'
#' @return The next available instance for the repeating instrument as an integer.
#'
#' @export
generate_new_instance <- function(record_id, instrument_name, coach_data, redcap_uri, token) {
  message("Generating new instance for record_id: ", record_id, " and instrument: ", instrument_name)

  ## ⃣ Check for existing instances in coach_data (local cache)
  if (!is.null(coach_data)) {
    message("Checking coach_data for existing instances...")
    local_data <- coach_data %>%
      dplyr::filter(record_id == !!record_id,
                    tolower(redcap_repeat_instrument) == tolower(instrument_name)) %>%
      dplyr::filter(!is.na(redcap_repeat_instance)) %>%  #  Filter out NA instances!
      dplyr::pull(redcap_repeat_instance) %>%
      as.numeric() %>%  #  Ensure numeric
      na.omit()

    #  Debug print - Local instance numbers
    message("Local instance numbers from coach_data for '", instrument_name, "' for record_id ", record_id, ": ", paste(local_data, collapse = ", "))

    if (length(local_data) > 0) {
      next_instance <- max(local_data) + 1
      message("Next available instance for '", instrument_name, "' from coach_data is ", next_instance)
      return(next_instance)
    } else {
      message(" No instances found in coach_data. Falling back to API call.")
    }
  } else {
    message(" coach_data is NULL. Falling back to API call.")
  }

  ##  Fallback to pull from API if coach_data is not available or empty
  data <- tryCatch({
    REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_uri,
      token = token,
      fields = c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance"),
      export_repeating_instruments = TRUE,
      export_repeating_events     = TRUE
    )$data
  }, error = function(e) {
    message(" Error pulling REDCap data: ", e$message)
    return(NULL)
  })

  # Check if data was returned
  if (is.null(data) || nrow(data) == 0) {
    message("⚠️ No data pulled from REDCap. Returning 1 as the first instance.")
    return(1)
  }

  # Normalize instrument name in the API response to lowercase
  data$redcap_repeat_instrument <- tolower(data$redcap_repeat_instrument)
  instrument_name <- tolower(instrument_name)

  # Debug Print - Full data from API
  message("Full Data for all records from API:")
  print(head(data, 10)) # Print first 10 rows to avoid too much spam

  # Filter to only this instrument's instances for this record_id
  filtered_data <- data %>%
    dplyr::filter(record_id == !!record_id, redcap_repeat_instrument == !!instrument_name) %>%
    dplyr::filter(!is.na(redcap_repeat_instance)) %>% #  Filter out NA instances
    dplyr::pull(redcap_repeat_instance) %>%
    as.numeric() %>%  #  Ensure numeric
    na.omit()

  # Debug Print - Extracted instance numbers
  message("Existing instance numbers for '", instrument_name, "' for record_id ", record_id, ": ", paste(filtered_data, collapse = ", "))

  # Calculate next instance number
  if (length(filtered_data) > 0) {
    next_instance <- max(filtered_data) + 1
  } else {
    next_instance <- 1
  }

  # 🚩 Final debug message
  message("Next available instance for '", instrument_name, "' is ", next_instance)

  return(next_instance)
}


#' @title Submit Data to REDCap
#'
#' @description Submit a data frame to REDCap.
#'
#' @param data A data frame. The data to submit to REDCap.
#' @param record_id A numeric. The record ID.
#' @param redcap_uri A string. The REDCap API URL.
#' @param token A string. The REDCap API token.
#'
#' @return A response object containing the status of the submission.
#'
#' @export
submit_to_redcap <- function(data, record_id, redcap_uri, token) {
  data$record_id <- record_id
  response <- tryCatch(
    REDCapR::redcap_write(ds = data, redcap_uri = redcap_uri, token = token),
    error = function(e) list(success = FALSE, outcome_message = e$message)
  )
  return(response)
}

# -----------------------------------------------------------------------
#  Function: prepare_data_for_submission
# -----------------------------------------------------------------------
#' @title Prepare Data for Submission
#'
#' @description Format the input data for submission to REDCap.
#'
#' @param data A data frame. The input data to be formatted for submission.
#' @param record_id A character or numeric. The REDCap record ID.
#'
#' @return A list with formatted data ready for REDCap submission.
#'
#' @export
prepare_data_for_submission <- function(data, record_id) {
  #  Validate that data is a data frame
  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  #  Validate that record_id is non-null and non-empty
  if (is.null(record_id) || nchar(record_id) == 0) stop("`record_id` must be a non-empty character or numeric value.")

  #  Ensure checkboxes are broken down
  checkbox_fields <- grep("___", colnames(data), value = TRUE)
  if (length(checkbox_fields) > 0) {
    data <- data %>%
      dplyr::mutate(across(all_of(checkbox_fields), ~ ifelse(. == 1, 1, 0)))
  }

  # ️ Ensure the record_id is included in the data
  data <- data %>%
    dplyr::mutate(record_id = as.character(record_id))

  #  Prepare the list to be submitted
  prepared_data <- list(
    content = jsonlite::toJSON(data, auto_unbox = TRUE),
    token = as.character(record_id)
  )

  return(prepared_data)
}


# -----------------------------------------------------------------------
#  Function: fetch_existing_data
# -----------------------------------------------------------------------
fetch_existing_data <- function(record_id, period, redcap_uri, token) {
  data <- tryCatch({
    REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_uri,
      token = token,
      filter_logic = paste0("[record_id] = '", record_id, "'")
    )$data
  }, error = function(e) {
    message(" Error in redcap_read_oneshot: ", e$message)
    return(data.frame())  # Return an empty data frame instead of NULL
  })

  if (!is.data.frame(data)) {
    message(" No data returned. Returning empty dataframe.")
    return(data.frame())
  }

  return(data)
}

# -----------------------------------------------------------------------
#  Function: validate_inputs
# -----------------------------------------------------------------------
#' @title Validate Inputs
#'
#' @description Validate that inputs are within the required range.
#'
#' @param inputs A named list of input values.
#' @param valid_range A vector. The acceptable range of input values.
#'
#' @return A list with `success` (TRUE/FALSE) and invalid inputs.
#'
#' @export
validate_inputs <- function(inputs, valid_range = 0:9) {
  invalid_inputs <- names(inputs)[!sapply(inputs, function(x) x %in% valid_range)]
  if (length(invalid_inputs) > 0) {
    return(list(success = FALSE, invalid_inputs = invalid_inputs))
  }
  return(list(success = TRUE))
}

# -----------------------------------------------------------------------
#  Function: update_inputs
# -----------------------------------------------------------------------
#' @title Update Inputs
#'
#' @description Update Shiny UI inputs with existing REDCap data.
#'
#' @param session The Shiny session object.
#' @param data A data frame. The existing REDCap data.
#' @param input_fields A character vector of input field names.
#'
#' @export
update_inputs <- function(session, data, input_fields) {
  for (field in input_fields) {
    if (field %in% colnames(data)) {
      print(paste("Updating", field, "to", data[[field]][1]))
      shiny::updateNumericInput(session, field, value = data[[field]][1])
    } else {
      print(paste("Field not found:", field, "setting to 0"))
      shiny::updateNumericInput(session, field, value = 0)
    }
  }
}

