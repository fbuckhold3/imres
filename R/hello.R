# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
install.packages(c("usethis", "devtools", "roxygen2", "testthat", "pkgdown"))
library(usethis)
library(devtools)
library(roxygen2)
library(testthat)
library(pkgdown)

hello <- function() {
  print("Hello, world!")
}
