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
      quantile25 = quantile(., na.rm=T, probs = 0.25),
      mean(.,na.rm=T),
      median(.,na.rm=T),
      quantile75 = quantile(., na.rm=T, probs = 0.75),
      max(.,na.rm=T)))
  return(statistics);
}

#' @export
WinPerfCounter.summaryInGB <- function(df) {
  statistics <- df %>% dplyr::group_by(metric) %>%
    dplyr::summarise_at(dplyr::vars(valueInGB), dplyr::funs(
      min(.,na.rm=T),
      quantile25 = quantile(., na.rm=T, probs = 0.25),
      mean(.,na.rm=T),
      median(.,na.rm=T),
      quantile75 = quantile(., na.rm=T, probs = 0.75),
      max(.,na.rm=T)))
  return(statistics);
}

#' @export
WinPerfCounter.formattable <- function(df) {
  formattable::formattable(df, list(
    min = color_bar("aquamarine"),
    quantile25 = color_bar("chartreuse"),
    mean = color_bar("chartreuse"),
    median = color_bar("steelblue"),
    quantile75 = color_bar("chartreuse"),
    max = color_bar("chocolate")
  ))
}

