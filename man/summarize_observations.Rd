% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_assessment.R
\name{summarize_observations}
\alias{summarize_observations}
\title{Summarize Observations}
\usage{
summarize_observations(data, resident, labels, ip_obs_field = "ip_obs_type")
}
\arguments{
\item{data}{A data frame with columns:
\describe{
  \item{name}{Resident name.}
  \item{ip_obs_type}{The observation type (e.g., numeric code or label).}
}}

\item{resident}{The resident's name (character).}

\item{labels}{A character vector of all possible labels (e.g. from \code{parse_ip_obs_labels}).}

\item{ip_obs_field}{The column name in \code{data} storing the text label or code
(default \code{"ip_obs_type"}).}
}
\value{
A tibble with columns:
  \describe{
    \item{ip_obs_type_label}{The observation label.}
    \item{Count}{How many times it appeared for that resident.}
  }
}
\description{
Given a data frame of observations (with a text column for \code{ip_obs_type}),
  this function filters for one resident, counts how many times each type appears,
  and ensures zero counts are shown for any labels that didn't appear.
}
\examples{
\dontrun{
  labels <- parse_ip_obs_labels(my_dict)
  obs_summary <- summarize_observations(ass_dat, "John Doe", labels)
}

}
