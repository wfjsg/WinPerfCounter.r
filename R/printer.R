library(lubridate)
library(foreach)
library(stringr)
library(dplyr)
library(formattable)
library(tibble)

#' @export
WinPerfCounter.summary <- function(df) {
  statistics <- df %>% dplyr::group_by(metric) %>%
    dplyr::summarise_at(dplyr::vars(value), dplyr::funs(
      min(.,na.rm=T),
      mean(.,na.rm=T),
      median(.,na.rm=T),
      max(.,na.rm=T)))
  return(statistics);
}

#' @export
WinPerfCounter.summaryInGB <- function(df) {
  statistics <- df %>% dplyr::group_by(metric) %>%
    dplyr::summarise_at(dplyr::vars(valueInGB), dplyr::funs(
      min(.,na.rm=T),
      mean(.,na.rm=T),
      median(.,na.rm=T),
      max(.,na.rm=T)))
  return(statistics);
}

#' @export
WinPerfCounter.formattable <- function(df) {
  formattable::formattable(df, list(
    min = color_bar("aquamarine"),
    mean = color_bar("chartreuse"),
    median = color_bar("steelblue"),
    max = color_bar("chocolate")
  ))
}

