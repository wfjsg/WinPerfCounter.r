# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(lubridate)
library(foreach)
library(stringr)


#' @importFrom lubridate
#' @param filename you want to read.
#' @return readed dataframe.
#' @export
WinPerfCounter.readcsv <- function(filename) {
  a <- read.csv(filename,
                check.names = FALSE,
                header = TRUE,
                fileEncoding = "UTF-8-BOM")

}

