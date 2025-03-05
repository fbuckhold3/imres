#' @importFrom shiny tags HTML
NULL

#' Apply Standard imres Theme
#'
#' This function applies the standard imres styling to a Shiny application.
#'
#' @return A tags$style object with HTML CSS
#' @export
imres_theme <- function() {
  tags$style(HTML(paste0("
    body { background-color: ", imres_colors$background, "; color: ", imres_colors$text, "; }
    h1, h2, h3 { color: ", imres_colors$primary, "; }
    .btn-primary { background-color: ", imres_colors$primary, "; border-color: ", imres_colors$primary, "; }
    .btn-secondary { background-color: ", imres_colors$secondary, "; border-color: ", imres_colors$secondary, "; }
    .btn-success { background-color: ", imres_colors$success, "; border-color: ", imres_colors$success, "; }
    .btn-warning { background-color: ", imres_colors$warning, "; border-color: ", imres_colors$warning, "; }
    .btn-danger { background-color: ", imres_colors$danger, "; border-color: ", imres_colors$danger, "; }
  ")))
}

#' Load Custom imres CSS
#'
#' This function loads the custom CSS file for imres applications.
#'
#' @return A tags$link object pointing to the CSS file
#' @export
imres_css <- function() {
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
}

#' Get Path to imres Logo
#'
#' @return A string containing the file path to the logo
#' @export
imres_logo <- function() {
  # For Shiny apps, we just need the filename, not the full system path
  return("ssm_slucare.png")
}

