% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_redcap_submit.R
\name{fetch_record_id}
\alias{fetch_record_id}
\title{Fetch Record ID}
\usage{
fetch_record_id(resident, coach_data, redcap_uri, token)
}
\arguments{
\item{resident}{A character. The resident's name.}

\item{coach_data}{A dataframe containing resident names and corresponding record_ids.}

\item{redcap_uri}{A string. The REDCap API URL.}

\item{token}{A string. The REDCap API token.}
}
\value{
The record ID for the resident, or NULL if not found.
}
\description{
Fetch the record ID for a given resident name.
}
