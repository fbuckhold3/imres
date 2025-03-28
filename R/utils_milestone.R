# =====================================================
# 📄 **Milestone Data Wrangling Utilities**
#
# This file contains utility functions for cleaning and processing milestone data
# for use in data visualization, analysis, and reporting. The primary goal is to
# prepare "program" and "self" milestone data for radar plots, summary tables,
# and longitudinal analyses.
#
# ## 📚 **Function Overview:**
# - `get_all_milestones`: Pulls milestone data from the REDCap API.
# - `get_milestone_columns`: Extracts milestone columns for "program" or "self" milestone types.
# - `rename_milestone_columns`: Renames milestone columns for consistency and readability.
# - `calculate_and_append_medians`: Calculates median milestone values and appends them to the dataset.
# - `miles_plot`: Generates radar plots for milestone data.
# - `process_milestones`: Processes "program" or "self" milestone data for visualization and analysis.
# =====================================================

# -----------------------------------------------------
# 📚 **Table of Contents**
# -----------------------------------------------------
# 1️⃣ get_all_milestones() - Pulls all milestone data from REDCap
# 2️⃣ get_milestone_columns() - Extracts milestone columns for "program" or "self"
# 3️⃣ rename_milestone_columns() - Renames milestone columns for readability
# 4️⃣ calculate_and_append_medians() - Calculates and appends median milestone rows
# 5️⃣ miles_plot() - Generates radar plots for milestone data
# 6️⃣ process_milestones() - Processes milestone data for "program" or "self"
# -----------------------------------------------------


# --------------------------------------------------------------------------
# 1️⃣ Function: get_all_milestones
# Purpose: Pull milestone data from the API
# --------------------------------------------------------------------------
#' Get All Milestones
#'
#' Pulls data from REDCap for the specified milestone forms.
#'
#' @return A data frame with all milestone data.
#' @export
get_all_milestones <- function(token, url) {
  all_data <- forms_api_pull(
    token = token,
    url = url,
    'resident_data',
    'milestone_entry',
    'milestone_selfevaluation_c33c'
  )
  return(all_data)
}


# --------------------------------------------------------------------------
# 2️⃣ Function: get_milestone_columns
# Purpose: Extract milestone columns for "program" or "self" milestone type
# --------------------------------------------------------------------------
#' Get Milestone Columns
#'
#' Extracts the milestone columns for either "program" or "self" milestone data.
#'
#' @param data The data frame containing milestone data.
#' @param type The type of milestone, either "program" or "self".
#' @return A vector of milestone column names.
#' @export
get_milestone_columns <- function(data, type = "program") {
  if (type == "program") {
    milestone_columns <- colnames(data)[
      stringr::str_detect(colnames(data), "^(rep_pc|rep_mk|rep_sbp|rep_pbl|rep_prof|rep_ics)\\d*$")
    ]
    extra_columns <- c("prog_mile_date", "prog_mile_period", "res_archive")
  } else if (type == "self") {
    milestone_columns <- colnames(data)[
      stringr::str_detect(colnames(data), "^(rep_pc|rep_mk|rep_sbp|rep_pbl|rep_prof|rep_ics)\\d*_self$")
    ]
    extra_columns <- c("prog_mile_date_self", "prog_mile_period_self", "res_archive")
  } else {
    stop("Invalid type. Must be 'program' or 'self'.")
  }

  all_columns <- c(milestone_columns, extra_columns)
  milestone_columns <- unique(all_columns)
  return(milestone_columns)
}


# --------------------------------------------------------------------------
# 3️⃣ Function: rename_milestone_columns
# Purpose: Rename milestone columns according to patterns
# --------------------------------------------------------------------------
#' Rename Milestone Columns
#'
#' Renames milestone columns to clean up names for plotting and analysis.
#'
#' @param df A data frame with milestone columns.
#' @return A data frame with renamed columns.
#' @export
rename_milestone_columns <- function(df) {
  rename_patterns <- c(
    "rep_pc(\\d+)(_self|_desc|_self_desc)?" = "PC\\1",
    "rep_mk(\\d+)(_self|_desc|_self_desc)?" = "MK\\1",
    "rep_sbp(\\d+)(_self|_desc|_self_desc)?" = "SBP\\1",
    "rep_pbl(\\d+)(_self|_desc|_self_desc)?" = "PBL\\1",
    "rep_prof(\\d+)(_self|_desc|_self_desc)?" = "PROF\\1",
    "rep_ics(\\d+)(_self|_desc|_self_desc)?" = "ICS\\1",
    "prog_mile_date" = "mile_date",
    "prog_mile_period" = "period"
  )
  df <- df %>%
    rename_with(~ stringr::str_replace_all(.x, rename_patterns)) %>%
    rename_with(~ stringr::str_remove_all(.x, "_self|_desc"))
  return(df)
}


# --------------------------------------------------------------------------
# 4️⃣ Function: calculate_and_append_medians
# Purpose: Calculate median milestone values for each period
# --------------------------------------------------------------------------
#' Calculate and Append Medians
#'
#' Calculates the median for each milestone column, grouped by period.
#'
#' @param data A data frame containing milestone data.
#' @param period_col The name of the column to group by.
#' @param exclude_cols Columns to exclude from the calculation.
#' @return A data frame with median rows added.
#' @export
calculate_and_append_medians <- function(data, period_col = "period", exclude_cols = c('record_id', 'name', 'mile_date')) {

  # Step 1: Identify numeric columns to calculate medians
  numeric_columns <- setdiff(names(data), c(exclude_cols, period_col))

  # Step 2: Coerce numeric columns to numeric (ignore warnings if coercion fails)
  data <- data %>%
    mutate(across(all_of(numeric_columns), ~ as.numeric(.x)))

  # Step 3: Filter out rows where period is NA
  filtered_data <- data %>%
    filter(!is.na(!!sym(period_col)))

  # Step 4: Calculate the median for each milestone column, grouped by period
  median_data <- filtered_data %>%
    group_by(!!sym(period_col)) %>%
    summarise(across(all_of(numeric_columns), median, na.rm = TRUE))

  # Step 5: Add a "name" column with "Median" for these calculated median rows
  median_data <- median_data %>%
    mutate(name = "Median")

  # Step 6: Reorder columns to match the original data (place 'name' first)
  median_data <- median_data %>%
    relocate(name)

  # Step 7: Bind the median rows to the original dataset
  final_data <- bind_rows(data, median_data)

  return(final_data)
}


# --------------------------------------------------------------------------
# Function: miles_plot
# Purpose: Generate a radar plot for a specific resident and period
# --------------------------------------------------------------------------
#' Milestones Radar Plot
#'
#' This function generates a radar plot using the `ggradar` package for a given
#' resident and period. The input data is filtered internally using `name` and `period`.
#'
#' @param data A data frame containing the milestone data.
#' @param name The resident name for which to generate the radar plot.
#' @param period The milestone period to filter the data.
#' @return A ggplot object representing the radar plot.
#' @export
miles_plot <- function(data, name, period) {

  # 🎉 **Filter the data for the specified resident and period**
  plot_data <- data %>%
    filter((name == !!name & period == !!period) | (name == 'Median' & period == !!period)) %>%  # Filter by resident and period
    relocate(name)  # Move 'name' to the first column (required by ggradar)

  # 🛑 **Check if plot_data is empty**
  if (nrow(plot_data) == 0) {
    message("No data available for the specified resident and period.")
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for this resident and period",
                 color = "red", size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void()
    )
  }

  # 🔍 **Extract the most recent mile_date for the plot title**
  mile_date <- plot_data %>%
    filter(name == !!name) %>%
    pull(mile_date) %>%
    unique() %>%
    na.omit()

  if (length(mile_date) > 1) {
    mile_date <- max(mile_date)  # Use the most recent (latest) mile_date
  }

  # 🔍 **Check for NA values in numeric milestone columns**
  milestone_cols <- setdiff(names(plot_data), c("name", "mile_date", "period", "record_id"))  # Exclude 'name', 'mile_date', 'period', 'record_id', 'res_archive'

  plot_data <- plot_data %>%
    select(-c(record_id, period, mile_date)) %>%  # Remove non-milestone columns (keep only milestone values)
    mutate(across(all_of(milestone_cols), ~ as.numeric(.x)))  # Coerce to numeric

  if (any(is.na(plot_data))) {
    warning("NA values found in milestone data. Replacing NA with 0.")
    plot_data <- plot_data %>%
      mutate(across(all_of(milestone_cols), ~ replace_na(.x, 0)))
  }

  # 🎨 **Dynamic color selection for each group**
  unique_groups <- unique(plot_data$name)
  num_groups <- length(unique_groups)
  available_colours <- c("#FC4E07", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

  if (num_groups > length(available_colours)) {
    warning("Insufficient colors provided for the number of groups. Colors will be recycled.")
    group_colours <- rep(available_colours, length.out = num_groups)
  } else {
    group_colours <- available_colours[1:num_groups]
  }

  # 🚀 **Create the radar plot**
  radar_plot <- ggradar::ggradar(
    plot_data,
    values.radar = c("0", "5", "9"),
    grid.min = 0, grid.mid = 5, grid.max = 9,
    group.line.width = 1.5,
    group.point.size = 3,
    group.colours = group_colours,
    background.circle.colour = "white",
    gridline.min.colour = "#DDDDDD",
    gridline.mid.colour = "#AAAAAA",
    gridline.max.colour = "#333333",
    legend.position = "bottom",
    axis.label.size = 4,
    axis.line.colour = "gray20"
  ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, color = "black"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom"
    ) +
    labs(
      title = paste0(name, " (", period, ") - Entered on ", mile_date)
    )

  return(radar_plot)
}

# --------------------------------------------------------------------------
# 6️⃣ Function: process_milestones
# Purpose: Process milestone data for "program" or "self" milestone type
# --------------------------------------------------------------------------
#' Process Milestone Data
#'
#' Processes "program" or "self" milestone data, appending medians and removing archived residents.
#'
#' @param data The data frame containing milestone data.
#' @param type The type of milestone, "program" or "self".
#' @return A processed milestone dataset.
#' @export
process_milestones <- function(data, type) {

  # ✅ Validate the type input
  if (!type %in% c("program", "self")) {
    stop("Invalid type. Must be 'program' or 'self'.")
  }

  # ✅ 1. Extract the appropriate milestone columns
  milestone_columns <- get_milestone_columns(data, type = type)

  # ✅ 2. Extract and filter the data
  period_col <- if (type == "program") "prog_mile_period" else "prog_mile_period_self"

  milestone_data <- data %>%
    select(record_id, name, all_of(milestone_columns), res_archive, !!sym(period_col)) %>%  # Ensure period and res_archive are retained
    filter(rowSums(!is.na(select(., all_of(milestone_columns)))) >= 10) %>%  # Filter rows with at least 10 non-NA milestone values
    rename_milestone_columns()  # Rename milestone columns

  # ✅ 3. Calculate medians and append to data
  # Use the `period` column after renaming, if rename_milestone_columns changes its name
  if ("period" %in% colnames(milestone_data)) {
    period_col <- "period"
  }

  processed_data <- calculate_and_append_medians(
    milestone_data,
    period_col = period_col,
    exclude_cols = c('record_id', 'name', 'mile_date', 'res_archive')
  )

  # ✅ 4. Ensure res_archive = NA for median rows
  processed_data <- processed_data %>%
    mutate(res_archive = ifelse(name == "Median", NA, res_archive))

  # ✅ 5. Remove archived residents using the function
  archive <- archive(data)

  # ✅ 6. Remove the res_archive column only after archived residents are removed
  final_data <- processed_data%>%
    filter(!name %in% archive) %>%
    select(-res_archive)

  return(final_data)
}

#' @importFrom shiny NS selectInput moduleServer

#' @export
# Module for period selection
mod_miles_select_ui <- function(id) {
  ns <- NS(id)
  selectInput(ns("period"), "Select Period", choices = c("Entering Residency", "Mid Intern", "End Intern", "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating", "Interim Review"))
}

#' @export
mod_miles_select_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$period)
  })
}
