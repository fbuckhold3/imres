# imres: Data Visualization Support for IM Residency Programs

## Overview

`imres` is a package designed to support interpretable machine learning research with standardized UI components, styling, and utility functions for building consistent Shiny applications.

## Installation

You can install the development version of `imres` from GitHub:

```         
# install.packages("remotes")
remotes::install_github("fbuckhold3/imres")
```

## Features

-   **Consistent Styling**: Apply uniform styling across all Shiny applications with `imres_theme()`

-   **Custom CSS**: Easily incorporate custom CSS with `imres_css()`

-   **Branding**: Add the standard logo to your applications with `imres_logo()`

-   **Color Palette**: Access the standard color palette with `imres_colors`

## Usage

```         
library(shiny)
library(imres)

ui <- fluidPage(
  # Apply imres styling
  imres_theme(),
  imres_css(),
  
  # Add the logo
  tags$img(src = imres_logo(), height = "50px"),
  
  # Your app UI components
  titlePanel("My Interpretable ML App"),
  
  sidebarLayout(
    sidebarPanel(
      # Inputs here
    ),
    mainPanel(
      # Outputs here
    )
  )
)

server <- function(input, output, session) {
  # Server logic
}

shinyApp(ui = ui, server = server)
```

## Requirements

-   R \>= 3.5.0

-   shiny

## Contact

For questions, issues, or feature requests, please [open an issue](https://github.com/fbuckhold3/imres/issues) on GitHub.

## License

This project is licensed under the MIT License - see the LICENSE file for details
