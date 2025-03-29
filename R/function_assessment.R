#' Assessment Visualization
#' The following contains functions used in various applications for visualizing data regarding assessment of residents. Includes general plus delta data, continuity clinic, inpatient, and observation assessments, with more to be added as needed.
#'
#'
#'
#' Count Resident Assessments
#'
#' @description Summarizes the number of assessments for a specified resident,
#'   grouping by resident level, evaluation type, and rotation.
#'
#' @param data A data frame containing at least:
#'   \describe{
#'     \item{name}{Resident name.}
#'     \item{Level}{Resident level (e.g., "PGY1", "PGY2").}
#'     \item{eval_type}{Type of assessment (e.g., "Performance during the rotation").}
#'     \item{Rotation}{Rotation name or identifier.}
#'   }
#' @param resident_name A character string, the name of the resident of interest.
#'
#' @return A data frame (tibble) with columns:
#'   \describe{
#'     \item{Level}{The resident level.}
#'     \item{eval_type}{Renamed evaluation type (Summative, Continuity Clinic, or Observation).}
#'     \item{Rotation}{Rotation name, with parentheses content removed.}
#'     \item{Count}{How many assessments of this type/rotation/level.}
#'   }
#'   Returns \code{NULL} if no rows match the resident.
#'
#' @examples
#' \dontrun{
#'   df_counts <- count_res_assessments(resident_data, "John Doe")
#' }
#'
#' @export
count_res_assessments <- function(data, resident_name) {
  eval_labels <- c("Performance during the rotation" = "Summative",
                   "Quarterly" = "Continuity Clinic",
                   "An observation of an activity" = "Observation")
  plot_data <- data %>%
    dplyr::filter(name == resident_name) %>%
    dplyr::group_by(Level, eval_type, Rotation) %>%
    dplyr::summarize(Count = dplyr::n(), .groups = 'drop') %>%
    stats::na.omit() %>%
    dplyr::mutate(eval_type = dplyr::recode(eval_type, !!!eval_labels),
                  Rotation  = stringr::str_replace(Rotation, "\\(.*\\)", ""))

  if (nrow(plot_data) == 0) {
    return(NULL)
  } else {
    return(plot_data)
  }
}

#' Count Faculty Evaluations
#'
#' @description Counts how many faculty evaluations (where \code{redcap_repeat_instrument}
#'   is "Faculty Evaluation") a specified resident has completed, grouped by rotation.
#'
#' @param data A data frame containing at least:
#'   \describe{
#'     \item{name}{Resident name.}
#'     \item{redcap_repeat_instrument}{The instrument name (e.g., "Faculty Evaluation").}
#'     \item{rot}{Rotation identifier.}
#'   }
#' @param resident_name A character string, the name of the resident of interest.
#'
#' @return A data frame (tibble) with columns:
#'   \describe{
#'     \item{rot}{Rotation identifier.}
#'     \item{Count}{Number of faculty evaluations for that rotation.}
#'   }
#'   Returns \code{NULL} if no matching rows are found.
#'
#' @examples
#' \dontrun{
#'   df_fac_eval <- count_fac_eval(resident_data, "John Doe")
#' }
#'
#' @export
count_fac_eval <- function(data, resident_name) {
  plot_data <- data %>%
    dplyr::filter(name == resident_name,
                  redcap_repeat_instrument == "Faculty Evaluation") %>%
    dplyr::group_by(rot) %>%
    dplyr::summarize(Count = dplyr::n(), .groups = 'drop') %>%
    stats::na.omit()

  if (nrow(plot_data) == 0) {
    return(NULL)
  } else {
    return(plot_data)
  }
}

#' Compute Evaluation Progress
#'
#' @description Given a data set and a \code{count_function} (which should be either
#'   \code{count_res_assessments} or another function returning a data frame with
#'   columns \code{Level} and \code{Count}), this function calculates
#'   cumulative progress toward a target of 25 evaluations.
#'
#' @param data A data frame of resident evaluations.
#' @param resident_name A character string, the name of the resident of interest.
#' @param count_function A function (e.g., \code{count_res_assessments}) that returns
#'   summarized counts of evaluations by \code{Level}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{Level}{Resident level.}
#'     \item{Total}{Total number of evaluations for this level.}
#'     \item{Progress}{A percentage from 0--100.}
#'   }
#'   Returns \code{NULL} if the \code{count_function} returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   progress_df <- compute_eval_progress(resident_data, "Jane Doe", count_res_assessments)
#' }
#'
#' @export
compute_eval_progress <- function(data, resident_name, count_function) {
  plot_data <- count_function(data, resident_name)  # e.g. count_res_assessments()
  if (is.null(plot_data)) {
    return(NULL)
  }

  plot_data %>%
    dplyr::group_by(Level) %>%
    dplyr::summarize(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Progress = pmin((.data$Total / 25) * 100, 100))
}

#' Generate Total Evaluation Plot
#'
#' @description Creates a stacked bar chart visualizing how many evaluations a resident has,
#'   by evaluation type and rotation, faceted by level (if available).
#'
#' @param data A data frame, typically containing columns \code{name}, \code{Level},
#'   \code{eval_type}, and \code{Rotation}.
#' @param resident_name A character string, the name of the resident of interest.
#'
#' @details If the resident has no data or the \code{Level} column does not exist,
#'   a default placeholder plot is returned.
#'
#' @return A \pkg{ggplot2} object (either a stacked bar chart, or a placeholder plot
#'   if data is unavailable).
#'
#' @examples
#' \dontrun{
#'   p <- generate_tot_eval_plot(resident_data, "Jane Doe")
#'   print(p)
#' }
#'
#' @export
generate_tot_eval_plot <- function(data, resident_name) {
  plot_data <- count_res_assessments(data, resident_name)

  if (is.null(plot_data) || !"Level" %in% colnames(plot_data) || all(is.na(plot_data$Level))) {
    message("No valid `Level` data found! Displaying default plot.")

    # Return a default placeholder plot
    return(
      ggplot2::ggplot(
        data.frame(eval_type = c("Summative", "Observation", "Continuity Clinic"),
                   Count = c(0, 0, 0)),
        ggplot2::aes(x = .data$eval_type, y = .data$Count, fill = .data$eval_type)
      ) +
        ggplot2::geom_bar(stat = "identity", width = 0.6) +
        ggplot2::scale_fill_manual(values = c("Summative" = "#E69F00",
                                              "Observation" = "#56B4E9",
                                              "Continuity Clinic" = "#009E73")) +
        ggplot2::labs(title = "No Evaluations Yet", x = "Evaluation Type", y = "Number of Evaluations") +
        ggplot2::theme_minimal()
    )
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$eval_type, y = .data$Count, fill = .data$Rotation)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.6) +
    ggplot2::scale_fill_brewer(palette = "Set2", limits = unique(plot_data$Rotation)[1:8]) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Evaluations Completed by Level",
                  x = "Evaluation Type",
                  y = "Number of Evaluations",
                  fill = "Rotation") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~Level, ncol = 1)
}

#' Display Progress Bars in a Shiny UI
#'
#' @description Creates a list of Bootstrap progress bars for each resident level,
#'   given a data frame with columns \code{Level}, \code{Total}, and \code{Progress}.
#'
#' @param progress_data A data frame with columns:
#'   \describe{
#'     \item{Level}{Resident level (character).}
#'     \item{Total}{Numeric total count of evaluations.}
#'     \item{Progress}{Numeric percent (0--100) of completion.}
#'   }
#'
#' @return A \code{\link[shiny]{tagList}} of \code{div}s suitable for rendering
#'   in a Shiny UI.
#'
#' @examples
#' \dontrun{
#'   ui <- fluidPage(
#'     uiOutput("progress_bars")
#'   )
#'
#'   server <- function(input, output, session) {
#'     progress_df <- data.frame(Level = c("PGY1","PGY2"), Total = c(10, 15), Progress = c(40, 60))
#'     output$progress_bars <- renderUI({
#'       display_progress(progress_df)
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @export
display_progress <- function(progress_data) {
  if (is.null(progress_data) || nrow(progress_data) == 0) {
    return(
      htmltools::div(
        "No evaluations completed yet.",
        style = "text-align: center; font-size: 16px;"
      )
    )
  }

  htmltools::tagList(
    lapply(seq_len(nrow(progress_data)), function(i) {
      level <- progress_data$Level[i]
      total <- progress_data$Total[i]
      prog  <- progress_data$Progress[i]

      htmltools::div(
        style = "margin-bottom: 15px;",
        htmltools::h5(level, style = "margin-bottom: 5px;"),
        htmltools::div(
          class = "progress",
          htmltools::div(
            class = "progress-bar",
            role = "progressbar",
            style = paste0("width: ", prog, "%;"),
            `aria-valuenow` = prog,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100",
            paste0(prog, "%")
          )
        ),
        htmltools::p(
          paste("Evaluations Completed:", total),
          style = "margin-top: 5px; font-size: 90%;"
        )
      )
    })
  )
}

#' Create a Stylized DataTable
#'
#' @description Returns a \code{\link[DT]{datatable}} with styling and hover effects,
#'   optionally displaying a message if the data is empty.
#'
#' @param data A data frame or tibble to display.
#' @param caption An optional string to use as the table caption.
#'
#' @return A \code{\link[DT]{datatable}} object styled with a specific look-and-feel.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(A = 1:3, B = c("x","y","z"))
#'   create_styled_dt(df, caption = "Example Table")
#' }
#'
#' @export
create_styled_dt <- function(data, caption = NULL) {
  if (nrow(data) == 0) {
    return(
      DT::datatable(
        data.frame(Message = paste0("No ", tolower(caption), " data available")),
        options = list(dom = 't'),
        caption = caption,
        rownames = FALSE
      )
    )
  }

  DT::datatable(
    data,
    options = list(
      pageLength = 5,
      dom = 'ftp',
      scrollX = TRUE,
      columnDefs = list(list(
        targets = "_all",
        render = DT::JS(
          "function(data, type, row) {
            if (data === null || data === '') {
              return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
            }
            return data;
          }"
        )
      ))
    ),
    caption = caption,
    rownames = FALSE,
    class = 'cell-border stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(data),
      backgroundColor = '#f8f9fa',
      borderColor = '#dfe2e5'
    )
}

#' Parse Labels for ip_obs_type
#'
#' @description Extracts only the textual labels for all possible choices
#'   from the REDCap dictionary's \code{select_choices_or_calculations}
#'   for a given field (default \code{"ip_obs_type"}).
#'
#' @param dict A data frame or tibble of your REDCap dictionary, must have
#'   \code{field_name} and \code{select_choices_or_calculations}.
#' @param ip_obs_field Character name of the field (default: \code{"ip_obs_type"}).
#'
#' @return A character vector of labels (e.g., "Written H&P", "Verbal presentation",
#'   "Physical Exam").
#'
#' @examples
#' \dontrun{
#'   labels <- parse_ip_obs_labels(my_dict, "ip_obs_type")
#'   print(labels)
#' }
#'
#' @export
parse_ip_obs_labels <- function(dict, ip_obs_field = "ip_obs_type") {
  dict_row <- dict %>%
    dplyr::filter(.data$field_name == ip_obs_field)

  dict_string <- dict_row %>%
    dplyr::pull(.data$select_choices_or_calculations) %>%
    .[1]

  # Split on '|'
  choice_list <- stringr::str_split(dict_string, "\\|")[[1]]

  # Trim each piece, discard empty lines
  choice_list <- choice_list %>%
    purrr::map_chr(stringr::str_trim) %>%
    purrr::discard(~ .x == "")

  # Parse out the label portion
  labels_only <- purrr::map_chr(choice_list, ~ {
    match <- stringr::str_match(.x, "^(\\d+)\\s*,\\s*(.*)$")
    match[, 3]
  })

  labels_only <- labels_only[!is.na(labels_only)]
  labels_only
}

#' Summarize Observations
#'
#' @description Given a data frame of observations (with a text column for \code{ip_obs_type}),
#'   this function filters for one resident, counts how many times each type appears,
#'   and ensures zero counts are shown for any labels that didn't appear.
#'
#' @param data A data frame with columns:
#'   \describe{
#'     \item{name}{Resident name.}
#'     \item{ip_obs_type}{The observation type (e.g., numeric code or label).}
#'   }
#' @param resident The resident's name (character).
#' @param labels A character vector of all possible labels (e.g. from \code{parse_ip_obs_labels}).
#' @param ip_obs_field The column name in \code{data} storing the text label or code
#'   (default \code{"ip_obs_type"}).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{ip_obs_type_label}{The observation label.}
#'     \item{Count}{How many times it appeared for that resident.}
#'   }
#'
#' @examples
#' \dontrun{
#'   labels <- parse_ip_obs_labels(my_dict)
#'   obs_summary <- summarize_observations(ass_dat, "John Doe", labels)
#' }
#'
#' @export
summarize_observations <- function(data,
                                   resident,
                                   labels,
                                   ip_obs_field = "ip_obs_type") {
  plot_data <- data %>%
    dplyr::filter(.data$name == resident,
                  !is.na(.data[[ip_obs_field]])) %>%
    dplyr::mutate(temp_label = stringr::str_trim(.data[[ip_obs_field]])) %>%
    dplyr::group_by(temp_label) %>%
    dplyr::summarize(Count = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(ip_obs_type_label = temp_label)

  all_labels <- tibble::tibble(ip_obs_type_label = labels)

  final_df <- dplyr::left_join(all_labels, plot_data, by = "ip_obs_type_label") %>%
    dplyr::mutate(Count = tidyr::replace_na(.data$Count, 0)) %>%
    dplyr::arrange(factor(.data$ip_obs_type_label, levels = labels))

  final_df
}

#' Compute Observation Progress
#'
#' @description Given a data frame of summarized observations, this function
#'   computes a \code{Progress} column (as a percentage of a specified target).
#'
#' @param summarized_df A data frame with columns \code{ip_obs_type_label}
#'   and \code{Count} (e.g., from \code{summarize_observations}).
#' @param target Numeric, the number of observations at which progress should
#'   be considered 100\%. Default is 5.
#'
#' @return The same data frame with an additional \code{Progress} column (integer 0--100).
#'
#' @examples
#' \dontrun{
#'   obs_summary <- summarize_observations(ass_dat, "Jane Doe", c("Written H&P", "Physical Exam"))
#'   obs_progress <- compute_obs_progress(obs_summary, target = 5)
#' }
#'
#' @export
compute_obs_progress <- function(summarized_df, target = 5) {
  if (nrow(summarized_df) == 0) {
    return(summarized_df)
  }

  summarized_df %>%
    dplyr::mutate(Progress = pmin(round((.data$Count / target) * 100), 100))
}

#' Prepare Progress Data for Display
#'
#' @description A convenience wrapper around \code{\link{compute_obs_progress}} that
#'   renames columns to \code{Level} and \code{Total} for consistency with other
#'   progress displays.
#'
#' @param summarized_df A data frame with columns \code{ip_obs_type_label} and \code{Count}.
#' @param target Numeric, the number of observations for 100\%. Default is 5.
#'
#' @return A data frame with columns: \code{Level}, \code{Total}, and \code{Progress}.
#'
#' @examples
#' \dontrun{
#'   obs_summary <- summarize_observations(ass_dat, "John Doe", c("Written H&P","Physical Exam"))
#'   final_progress <- prepare_progress_data(obs_summary, target = 5)
#' }
#'
#' @export
prepare_progress_data <- function(summarized_df, target = 5) {
  summarized_df %>%
    compute_obs_progress(target = target) %>%
    dplyr::rename(Level = "ip_obs_type_label", Total = "Count")
}

#' Prepare and Display Observation Table
#'
#' @description A helper function that filters data for a specified resident and
#'   observation type label, drops empty/unwanted columns, renames columns based on
#'   the data dictionary, then returns a styled \code{\link[DT]{datatable}}.
#'
#' @param data A data frame of observations containing columns such as \code{name},
#'   \code{ip_obs_type}, \code{Date}, etc.
#' @param dict A data frame data dictionary with columns \code{field_name} and
#'   \code{field_label}.
#' @param resident A character string for the resident name.
#' @param selected_label The chosen observation label to filter on. Must match the
#'   strings in \code{ip_obs_type}.
#' @param caption An optional caption for the output table. Defaults to
#'   \code{"Details for: {selected_label}"} if \code{NULL}.
#'
#' @return A \code{\link[DT]{datatable}} object, styled by \code{\link{create_styled_dt}}.
#'
#' @examples
#' \dontrun{
#'   # If 'my_dict' is your REDCap dictionary and 'obs_data' is your main data:
#'   output$obs_table <- DT::renderDT({
#'     prep_obs_table(obs_data, my_dict, "John Doe", "Physical Exam")
#'   })
#' }
#'
#' @export
prep_obs_table <- function(data,
                           dict,
                           resident,
                           selected_label,
                           caption = NULL) {
  filtered_data <- data %>%
    dplyr::filter(name == resident, ip_obs_type == selected_label)

  if (nrow(filtered_data) == 0) {
    return(create_styled_dt(
      data.frame(),
      caption = paste("No data for:", selected_label)
    ))
  }

  always_include_cols <- c("Date", "ip_obs_type", "Level", "Evaluator")
  non_empty <- colSums(!is.na(filtered_data)) > 0
  non_empty[intersect(always_include_cols, names(filtered_data))] <- TRUE
  final_data <- filtered_data[, non_empty, drop = FALSE]

  unwanted_cols <- c(
    "assess_a_resident_timestamp", "ass_date", "clin_context",
    "slu_gim_att", "att_sign", "assess_a_resident_complete",
    "week", "year", "name", "eval_type", "weekyr",
    "start_year", "academic_year_start", "record_id", "ip_obs_type"
  )
  final_data <- final_data[, setdiff(names(final_data), unwanted_cols), drop = FALSE]

  rename_map <- stats::setNames(dict$field_label, dict$field_name)
  final_data <- dplyr::rename_with(
    final_data,
    .cols = dplyr::everything(),
    .fn = ~ ifelse(.x %in% names(rename_map), rename_map[.x], .x)
  )

  front_cols <- c("Date", "Rotation", "Level")
  front_cols <- intersect(front_cols, names(final_data))
  other_cols <- setdiff(names(final_data), front_cols)
  final_data <- final_data[, c(front_cols, other_cols), drop = FALSE]

  create_styled_dt(
    final_data,
    caption = if (is.null(caption)) {
      paste("Details for:", selected_label)
    } else {
      caption
    }
  )
}



#' Create a Styled DataTable for  Data
#'
#' Generates a DT::datatable object with custom styling and an optional caption.
#'
#' @param data A data frame of processed evaluation data.
#' @param caption An optional character string for the table caption/title.
#'
#' @return A DT::datatable object with applied styling.
#'
#' @export
create_styled_dt <- function(data, caption = NULL) {
  if (nrow(data) == 0) {
    return(DT::datatable(
      data.frame(Message = paste0("No ", tolower(caption), " data available")),
      options = list(dom = 't'),
      caption = caption,
      rownames = FALSE
    ))
  }

  DT::datatable(
    data,
    options = list(
      pageLength = 5,
      dom = 'ftp',
      scrollX = TRUE,
      columnDefs = list(list(
        targets = "_all",
        render = DT::JS(
          "function(data, type, row) {
            if (data === null || data === '') {
              return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
            }
            return data;
          }"
        )
      ))
    ),
    caption = caption,
    rownames = FALSE,
    class = 'cell-border stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(data),
      backgroundColor = '#f8f9fa',
      borderColor = '#dfe2e5'
    )
}



#' Generate Plus-Delta Data
#'
#' Filters and processes the input data to extract plus-delta evaluations for a specific resident.
#'
#' @param data A data frame containing evaluation data.
#' @param resident A character string specifying the resident's name.
#'
#' @return A data frame containing the Date, Rotation, Level, Plus (renamed from cc_res_does_well),
#' Delta (renamed from res_to_improve), Feedback (renamed from min_giv_feedback), and Evaluator for the resident.
#'
#' @export
generate_p_d <- function(data, resident) {
  data %>%
    filter(name == resident) %>%
    select(Date, Rotation, Level, cc_res_does_well, res_to_improve, min_giv_feedback, Evaluator) %>%
    dplyr::rename(Plus = cc_res_does_well,
                  Delta = res_to_improve,
                  Feedback = min_giv_feedback) %>%
    filter(!(is.na(Rotation) & is.na(Plus) & is.na(Delta)))
}

#' Create Continuity Clinic (CC) Evaluation Completion Table
#'
#' Creates a reactable table displaying the completion status of evaluations organized by level
#' and evaluation type, filtered by a specific name. Always includes the four standard evaluation types across all quarters.
#'
#' @param data A dataframe containing columns: name, Level, cc_eval_type, and Evaluator.
#' @param name A character string specifying the name to filter the data by.
#'
#' @return A reactable table object.
#'
#' @export
#'
#' @importFrom reactable reactable colDef colGroup reactableTheme
#' @importFrom htmltools div
#' @importFrom dplyr filter arrange
create_cc_table <- function(data, name) {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("Package 'reactable' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  standard_eval_types <- c(
    "1st quarter - inbasket coverage",
    "2nd quarter - summative evaluation",
    "3rd quarter - documentation",
    "4th quarter - summative evaluation"
  )

  display_names <- c(
    "Q1 - Inbasket",
    "Q2 - Summative",
    "Q3 - Documentation",
    "Q4 - Summative"
  )

  filtered_data <- data %>%
    dplyr::filter(name == !!name, !is.na(cc_eval_type)) %>%
    dplyr::arrange(Level)

  # Always include all three levels
  result_df <- data.frame(Level = c("Intern", "PGY2", "PGY3"), stringsAsFactors = FALSE)


  # Loop over each standard evaluation type to populate the table

  for (i in 1:length(standard_eval_types)) {
    et <- standard_eval_types[i]
    display_name <- display_names[i]

    result_df[[paste0(display_name, " Status")]] <- FALSE
    result_df[[paste0(display_name, " Evaluator")]] <- NA

    for (j in 1:nrow(result_df)) {
      level_val <- result_df$Level[j]

      if (level_val == "No data available") next

      matching_rows <- filtered_data %>%
        dplyr::filter(Level == level_val, cc_eval_type == et)

      if (nrow(matching_rows) > 0) {
        result_df[[paste0(display_name, " Status")]][j] <- TRUE
        result_df[[paste0(display_name, " Evaluator")]][j] <- matching_rows$Evaluator[1]
      }
    }
  }

  custom_theme <- reactable::reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5ff",
    cellPadding = "10px 12px",
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
      fontSize = "14px"
    ),
    headerStyle = list(
      backgroundColor = "#f1f3f5",
      color = "#212529",
      fontWeight = 600,
      borderBottom = "2px solid #dee2e6"
    )
  )

  cols <- list(
    Level = reactable::colDef(
      name = "Level",
      minWidth = 120,
      cell = function(value) {
        htmltools::div(
          style = "font-weight: 600; color: #1a73e8;",
          value
        )
      }
    )
  )

  column_groups <- list()
  for (i in 1:length(display_names)) {
    display_name <- display_names[i]

    status_col <- paste0(display_name, " Status")
    eval_col <- paste0(display_name, " Evaluator")

    cols[[status_col]] <- reactable::colDef(
      name = display_name,
      cell = function(value) {
        if (isTRUE(value)) {
          htmltools::div(
            style = "display: flex; justify-content: center; align-items: center;",
            htmltools::div(
              style = "background-color: #e6f4ea; color: #137333; border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-size: 18px;",
              "✓"
            )
          )
        } else {
          htmltools::div(
            style = "display: flex; justify-content: center; align-items: center;",
            htmltools::div(
              style = "background-color: #fce8e6; color: #c5221f; border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-weight: bold; font-size: 18px;",
              "O"
            )
          )
        }
      },
      width = 120,
      align = "center"
    )

    cols[[eval_col]] <- reactable::colDef(
      name = "Evaluator",
      cell = function(value) {
        if (is.na(value)) {
          htmltools::div(style = "color: #999; font-style: italic;", "Not assigned")
        } else {
          htmltools::div(style = "font-weight: 500;", value)
        }
      },
      width = 130
    )

    column_groups[[length(column_groups) + 1]] <- reactable::colGroup(
      name = display_name,
      columns = c(status_col, eval_col)
    )
  }

  reactable::reactable(
    result_df,
    columns = cols,
    bordered = TRUE,
    highlight = TRUE,
    striped = TRUE,
    theme = custom_theme,
    columnGroups = column_groups
  )
}

#' Create a Styled DataTable for  Data
#'
#' Generates a DT::datatable object with custom styling and an optional caption.
#'
#' @param data A data frame of processed evaluation data.
#' @param caption An optional character string for the table caption/title.
#'
#' @return A DT::datatable object with applied styling.
#'
#' @export
create_styled_dt <- function(data, caption = NULL) {
  if (nrow(data) == 0) {
    return(DT::datatable(
      data.frame(Message = paste0("No ", tolower(caption), " data available")),
      options = list(dom = 't'),
      caption = caption,
      rownames = FALSE
    ))
  }

  DT::datatable(
    data,
    options = list(
      pageLength = 5,
      dom = 'ftp',
      scrollX = TRUE,
      columnDefs = list(list(
        targets = "_all",
        render = DT::JS(
          "function(data, type, row) {
            if (data === null || data === '') {
              return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
            }
            return data;
          }"
        )
      ))
    ),
    caption = caption,
    rownames = FALSE,
    class = 'cell-border stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(data),
      backgroundColor = '#f8f9fa',
      borderColor = '#dfe2e5'
    )
}

#' Process Summative Evaluation Data
#'
#' Processes summative evaluation data for a specified resident and level. The function filters,
#' renames, and selects relevant columns based on level-specific criteria for summative evaluations.
#'
#' @param data A data frame containing evaluation data.
#' @param resident_name A character string specifying the resident's name.
#' @param level A character string indicating the level ("Intern", "PGY2", or "PGY3").
#'
#' @return A data frame with processed summative evaluation data if available; otherwise,
#' a data frame containing a message indicating no data or describing any error encountered.
#'
#' @export
process_summative_data <- function(data, resident_name, level) {
  cat("Function called with resident:", resident_name, "and level:", level, "\n")

  cc_names <- c(
    `Intern Presentation` = 'cc_intern_pc1_1',
    Differential = 'cc_intern_pc3_1',
    `Health Promotion` = 'cc_intern_pc5_1',
    `Chronic Management` = 'cc_intern_pc5_2',
    `Minimize unfamiliar terms (I)` = 'cc_intern_ics1_2',
    `Shared Decision-Making (I)` = 'cc_intern_ics1_1',
    Respect = 'cc_intern_prof1',
    `Takes feedback (I)` = 'cc_intern_pbl2_1',
    `Acknowledge errors` = 'cc_intern_pbl2_2',
    `Presentation PGY2` = 'cc_pgy2_pc1_1',
    Documentation = 'cc_pgy2_ics3_1',
    `Reflection on practice` = 'cc_pgy2_pbl2_2',
    `Care coordination (2)` = 'cc_pgy2_sbp2_1',
    `Use Evidence (2)` = 'cc_pgy2_pbl1',
    `Shared Decision-Making (2)` = 'cc_pgy2_ics1_1',
    `Teamwork (2)` = 'cc_pgy2_ics2_2',
    `Takes feedback (2)` = 'cc_pgy_pbl2_1',
    `Minimize unfamiliar terms (2)` = 'cc_pgy2_ics_1_2',
    `Presentation PGY3` = 'cc_pgy3_pc1_1',
    `Teamwork (3)` = 'cc_pgy3_ics2_2',
    `Shared Decision-Making (3)` = 'cc_pgy3_ics1_1',
    `Minimize unfamiliar terms (3)` = 'cc_pgy3_ics1_2',
    `Care coordination (3)` = 'cc_pgy3_sbp2_1',
    `Use Evidence (3)` = 'cc_pgy3_pbl1',
    `Takes feedback (3)` = 'cc_pgy3_pbl2_1',
    `Acknowledge errors (3)` = 'cc_pgy3_pbl2_2'
  )

  tryCatch({
    summative_evals <- data %>%
      filter(name == resident_name,
             !is.na(cc_eval_type),
             cc_eval_type %in% c("2nd quarter - summative evaluation", "4th quarter - summative evaluation"))

    cat("Found", nrow(summative_evals), "summative evaluations\n")

    if (level == "Intern") {
      filtered_data <- summative_evals %>%
        filter(!is.na(cc_intern_pc1_1) | !is.na(cc_intern_pc3_1) | !is.na(cc_intern_pc5_1) |
                 !is.na(cc_intern_ics1_2) | !is.na(cc_intern_ics1_1) | !is.na(cc_intern_prof1))
    } else if (level == "PGY2") {
      filtered_data <- summative_evals %>%
        filter(!is.na(cc_pgy2_pc1_1) | !is.na(cc_pgy2_ics3_1) | !is.na(cc_pgy2_pbl2_2) |
                 !is.na(cc_pgy2_ics1_1) | !is.na(cc_pgy2_ics2_2))
    } else if (level == "PGY3") {
      filtered_data <- summative_evals %>%
        filter(!is.na(cc_pgy3_pc1_1) | !is.na(cc_pgy3_ics2_2) | !is.na(cc_pgy3_ics1_1) |
                 !is.na(cc_pgy3_pbl2_1) | !is.na(cc_pgy3_pbl2_2))
    } else {
      filtered_data <- summative_evals
    }

    cat("After level-specific variable filtering, row count:", nrow(filtered_data), "\n")

    if (nrow(filtered_data) > 0) {
      filtered_data <- filtered_data %>%
        rename(any_of(cc_names)) %>%
        select(-any_of(c("Resident", "ID", "assess_a_resident_timestamp",
                         "clin_context", "res_to_improve", "cc_res_does_well",
                         "min_giv_feedback", "assess_a_resident_complete",
                         "Rotation", "eval_type", "Year", "Level", "cc_csm_att",
                         "att_sign", "week", "year", "name", "start_year",
                         "academic_year_start", "record_id"))) %>%
        select_if(~ any(!is.na(.)) & any(. != ""))

      cat("Final filtered data row count:", nrow(filtered_data), "\n")
      return(filtered_data)
    } else {
      return(data.frame(Message = paste("No summative data available for", level)))
    }
  }, error = function(e) {
    cat("Error caught:", e$message, "\n")
    return(data.frame(Message = paste("Error processing summative data:", e$message)))
  })
}

#' Process Continuity Clinic Plus-Delta Data
#'
#' Filters and processes continuity clinic plus-delta evaluation data for a given resident.
#'
#' @param data A data frame containing evaluation data.
#' @param resident_name A character string specifying the resident's name.
#'
#' @return A data frame with plus-delta data including Date, Level, Quarter (renamed from cc_eval_type),
#' Plus (renamed from cc_res_does_well), Delta (renamed from res_to_improve), and Feedback (renamed from min_giv_feedback).
#'
#' @export
process_cc_pd_data <- function(data, resident_name) {
  data %>%
    filter(name == resident_name, (Rotation == "VA Continuity clinic") | (Rotation == "CSM Continuity clinic (including metabolism clinic)")) %>%
    select(Date, Level, cc_eval_type, cc_res_does_well, res_to_improve, min_giv_feedback, Evaluator) %>%
    rename(Quarter = cc_eval_type,
           Plus = cc_res_does_well,
           Delta = res_to_improve,
           Feedback = min_giv_feedback)
}

#' Process Continuity Clinic Inbasket Data
#'
#' Filters and processes inbasket evaluation data for the continuity clinic for a given resident.
#'
#' @param data A data frame containing evaluation data.
#' @param resident_name A character string specifying the resident's name.
#'
#' @return A data frame with inbasket data including Level, Responsible (renamed from cc_inb_resp),
#' Result Response (renamed from cc_inb_resu), MyChart (renamed from cc_inb_mych), Paperwork (renamed from cc_inb_comm),
#' and Comms with preceptor (renamed from cc_inb_comp).
#'
#' @export
process_cc_inbasket_data <- function(data, resident_name) {
  bask <- c("cc_inb_resp", "cc_inb_resu", "cc_inb_mych", "cc_inb_comm", "cc_inb_comp", "Level", "Evaluator")
  data %>%
    filter(name == resident_name, cc_eval_type == "1st quarter - inbasket coverage") %>%
    select(starts_with('cc_'), Level, Evaluator) %>%
    select(any_of(bask)) %>%
    relocate('Level') %>%
    rename(Responsible = cc_inb_resp,
           `Result Response` = cc_inb_resu,
           MyChart = cc_inb_mych,
           Paperwork = cc_inb_comm,
           `Comms with preceptor` = cc_inb_comp)
}

#' Process Continuity Clinic Documentation Data
#'
#' Filters and processes documentation evaluation data for the continuity clinic for a given resident.
#'
#' @param data A data frame containing evaluation data.
#' @param resident_name A character string specifying the resident's name.
#'
#' @return A data frame with documentation data including Level, Update EMR (renamed from cc_doc_update),
#' Review EMR (renamed from cc_doc_review), Update meds (renamed from cc_doc_medall), Does reminders (renamed from cc_doc_remind),
#' Accurate Diagnoses (renamed from cc_doc_diag), Notes clear (renamed from cc_doc_notes), Notes complete (renamed from cc_doc_comp),
#' and Evaluator.
#'
#' @export
process_cc_document_data <- function(data, resident_name) {
  data %>%
    filter(name == resident_name, cc_eval_type == "3rd quarter - documentation") %>%
    select(Level, cc_doc_update, cc_doc_review, cc_doc_medall, cc_doc_remind, cc_doc_diag, cc_doc_notes, cc_doc_comp, Evaluator) %>%
    rename(`Update EMR` = cc_doc_update,
           `Review EMR` = cc_doc_review,
           `Update meds` = cc_doc_medall,
           `Does reminders` = cc_doc_remind,
           `Accurate Diagnoses` = cc_doc_diag,
           `Notes clear` = cc_doc_notes,
           `Notes complete` = cc_doc_comp)
}


#' @export
#' @title Pull & Split Inpatient Evaluations (Intern vs. Resident)
#' @description
#'   1. Filter data to a particular resident.
#'   2. Keep only core columns + any columns that start with "int_ip_" or "res_ip_".
#'   3. Split into two data frames:
#'      - intern_df: rows that have at least one non-NA in "int_ip_" columns
#'      - resident_df: rows that have at least one non-NA in "res_ip_" columns
#'   4. Drop columns that are entirely NA in each subset.
#'   5. Rename columns using your REDCap dictionary.
#'   6. Return a list with both data frames.
#'
#' @param data A data frame with columns:
#'   - `name` (resident name)
#'   - `Date`, `Level`, `Evaluator`, `Rotation`
#'   - zero or more columns starting with `int_ip_` or `res_ip_`
#' @param dict A data frame (dictionary) with `field_name` and `field_label`.
#' @param resident Character name for the resident (e.g., "Adam Streicher").
#'
#' @return A list with two data frames: `$intern_df` and `$resident_df`.
pull_inpatient_eval_split <- function(data, dict, resident) {

  # 1) Filter for the given resident
  filtered <- data %>%
    filter(name == resident)

  # 2) Identify "core" columns
  keep_core <- c("Date", "Level", "Evaluator", "Rotation")

  # 3) Find columns starting with "int_ip_" or "res_ip_"
  ip_cols <- grep("^(int_ip_|res_ip_)", names(filtered), value = TRUE)

  # If you expect at least one, proceed. If ip_cols is empty, you'll get no results
  # but it's safe to continue.

  # 4) Subset to those columns + core columns
  out <- filtered %>%
    select(any_of(keep_core), any_of(ip_cols)) %>%
    # drop columns that are ALL NA
    select(where(~ any(!is.na(.))))

  # 5) Separate the columns into "int_ip_" subset vs "res_ip_" subset
  int_cols <- grep("^int_ip_", names(out), value = TRUE)
  res_cols <- grep("^res_ip_", names(out), value = TRUE)

  # We still keep the core columns in each subset
  # So each subset = core columns + its own "ip" columns

  # 5a) Rows for intern subset: any row that has at least one non-NA in the int_ip_ columns
  intern_rows <- rep(FALSE, nrow(out))
  if (length(int_cols) > 0) {
    intern_rows <- rowSums(!is.na(out[, int_cols, drop=FALSE])) > 0
  }
  intern_df <- out[intern_rows, c(keep_core, int_cols), drop=FALSE]

  # drop columns that are entirely NA
  intern_df <- intern_df %>%
    select(where(~ any(!is.na(.))))

  # 5b) Rows for resident subset: any row that has at least one non-NA in the res_ip_ columns
  resident_rows <- rep(FALSE, nrow(out))
  if (length(res_cols) > 0) {
    resident_rows <- rowSums(!is.na(out[, res_cols, drop=FALSE])) > 0
  }
  resident_df <- out[resident_rows, c(keep_core, res_cols), drop=FALSE]

  resident_df <- resident_df %>%
    select(where(~ any(!is.na(.))))

  # 6) Rename columns using your dictionary in each subset
  setnames(
    intern_df,
    old = dict$field_name,
    new = dict$field_label,
    skip_absent = TRUE
  )
  setnames(
    resident_df,
    old = dict$field_name,
    new = dict$field_label,
    skip_absent = TRUE
  )

  # 7) Return them both as a list
  list(
    intern_df   = intern_df,
    resident_df = resident_df
  )
}
