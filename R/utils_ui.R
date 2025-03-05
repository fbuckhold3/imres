# imres/R/theme.R
#' Apply Standard imres Theme
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


# imres/R/css_loader.R
#' Load Custom imres CSS
#' @export
imres_css <- function() {
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
}
