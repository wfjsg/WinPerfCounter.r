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
library(dplyr)

#' @export
WinPerfCounter.readcsv <- function(filename) {
  data <- read.csv(filename,
                check.names = FALSE,
                header = TRUE,
                as.is = TRUE)
                # fileEncoding = "UTF-8-BOM")
  options(digits.secs=3)
  colnames(data)[1] <-"DateTime"
  # as.POSIXct try fromat isonly ... https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
  remainCol <- ncol(data) -1
  data1 <- cbind("Timestamp" = as.POSIXct(data$DateTime, format="%m/%d/%Y %H:%M:%S"),
                 data[,c(1:remainCol)])
  colNames <- colnames(data1)
  for(ii in 3:length(colNames)){
    splitted <- strsplit(colNames[ii], '\\\\')
    nname <- paste(splitted[[1]][4], splitted[[1]][5], sep = "|")
    colnames(data1)[ii] <- nname
  }
  return(data1);
}

#' @description align:"15 minutes" https://www.rdocumentation.org/packages/lubridate/versions/1.5.6/topics/round_date
#' @export
WinPerfCounter.alignTime <- function(data, align){
  r <- c(
      floor_date(min(data$Timestamp), align),
      ceiling_date(max(data$Timestamp), align)
    )
  return (r)
}

#' @export
WinPerfCounter.miniSummary <- function(data){
  # duration <- max(data$Timestamp) - min(data$Timestamp)
  # recordCount <-length(data)
  # ret <- list(duration = duration, recordCount = recordCount)

  sm <- summary(data)
  df <- t(data.frame(unclass(sm), check.names = FALSE, stringAsFactors = FALSE))
  return (df)
}

# memo: https://technet.microsoft.com/ja-jp/library/cc748731.aspx
# memo: http://jehupc.exblog.jp/14257174/


#' @export
WinPerfCounter.Metric.CPU <- function(data){
  col <- c("Timestamp", paste("Processor(_Total)|", c("% Processor Time", "Interrupts/sec"), sep = ""))
  ret <- data %>% dplyr::select(col)
  return(ret)
}

#' @export
WinPerfCounter.Metric.Memory <- function(data){
  keys <- c("Available Bytes", "Committed Bytes", "Commit Limit")
  col <- c("Timestamp", paste("Memory|", keys, sep = ""))
  outkeys <- c("Timestamp", gsub(" ", "_", keys))
  # ret <- data %>% dplyr::select(col) %>% tidyr::gather(key = metric, value = value, -Timestamp)
#  ret <- data %>% dplyr::select(col) %>%
    # dplyr::rename_all(WinPerfCounter.makeKeyOnly()) %>%
 #   dplyr::rename_at(.vars = dplyr::vars(), dplyr::funs(WinPerfCounter.makeKeyOnly(.)))
    # tidyr::gather(key = metric, value = value, -Timestamp)
  tmp <- data %>% dplyr::select(col)
  colnames(tmp) <- outkeys
  # ret <- tmp %>% tidyr::gather(key = metric, value = value, -Timestamp)
  ret <- tmp %>% tidyr::gather(key = metric, value = value, -Timestamp) %>% dplyr::mutate(valueInGB = value/(1024*1024*1024))
  return(ret)
}

WinPerfCounter.Process.Metric.Memory <- function(data, process){
  keys <- c("Virtual Bytes", "Working Set", "Private Bytes")
  col <- c("Timestamp", paste("Process(", process, ")|", keys, sep = ""))
  outkeys <- c("Timestamp", gsub(" ", "_", keys))

    tmp <- data %>% dplyr::select(col)
  colnames(tmp) <- outkeys
  # ret <- tmp %>% tidyr::gather(key = metric, value = value, -Timestamp)
  ret <- tmp %>%
    tidyr::gather(key = metric, value = value, -Timestamp) %>%
    dplyr::mutate(valueInGB = value/(1024*1024*1024))
  return(ret)
}
