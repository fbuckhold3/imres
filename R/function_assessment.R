#' Assessment Visualization
#' The following contains functions used in various applications for visualizing data regarding assessment of residents. Includes general plus delta data, continuity clinic, inpatient, and observation assessments, with more to be added as needed.
#'

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

  result_df <- data.frame(Level = unique(filtered_data$Level), stringsAsFactors = FALSE)

  if (nrow(result_df) == 0) {
    result_df <- data.frame(Level = "No data available", stringsAsFactors = FALSE)
  }

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

#' Create a Styled DataTable for CC Evaluation Data
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
