% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_assessment.R
\name{create_cc_table}
\alias{create_cc_table}
\title{Create Continuity Clinic (CC) Evaluation Completion Table}
\usage{
create_cc_table(data, name)
}
\arguments{
\item{data}{A dataframe containing columns: name, Level, cc_eval_type, and Evaluator.}

\item{name}{A character string specifying the name to filter the data by.}
}
\value{
A reactable table object.
}
\description{
Creates a reactable table displaying the completion status of evaluations organized by level
and evaluation type, filtered by a specific name. Always includes the four standard evaluation types across all quarters.
}
