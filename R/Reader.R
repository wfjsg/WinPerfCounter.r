library(lubridate)
library(foreach)
library(stringr)
library(tidyverse)
library(readr)
library(tidyverse)

#' @export
WinPerfCounter.readcsv <- function(filename) {
  data <- read_csv(filename, locale = locale(encoding = "cp932"), col_types = cols())
  #data <- read.csv(filename,
  #              check.names = FALSE,
  #              header = TRUE,
  #              as.is = TRUE)
  #              # fileEncoding = "UTF-8-BOM")
  options(digits.secs=3)
  colnames(data)[1] <-"DateTime"
  # as.POSIXct try fromat isonly ... https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
  remainCol <- ncol(data) -1
  data1 <- cbind("Timestamp" =
                   as.POSIXct(data$DateTime, format="%m/%d/%Y %H:%M:%S"),
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
  sm <- summary(data)
  df <- t(data.frame(unclass(sm), check.names = FALSE, stringAsFactors = FALSE))
  return (df)
}

# memo: https://technet.microsoft.com/ja-jp/library/cc748731.aspx
# memo: http://jehupc.exblog.jp/14257174/

makeTidyMetric <- function(data, category, metrics){
  col <- c("Timestamp", paste(category, "|", metrics, sep = ""))
  outkeys <- c("Timestamp", gsub(" ", "_", metrics))
  tmp <- data %>% dplyr::select(col)
  colnames(tmp) <- outkeys
  ret <- tmp %>% tidyr::gather(key = metric, value = value, -Timestamp, factor_key = TRUE)
  return(ret)
}

#' @export
WinPerfCounter.Metric.CPU <- function(data){
  keys <- c("% Processor Time", "% Privileged Time", "% User Time")
  ret <- makeTidyMetric(data, "Processor(_Total)", keys)
  return(ret)
}


#' @export
WinPerfCounter.Metric.Memory <- function(data){
  keys <- c("Available Bytes", "Committed Bytes", "Commit Limit")
  ret <- makeTidyMetric(data, "Memory", keys)
  ret <- ret %>% dplyr::mutate(valueInGB = value/(1024*1024*1024))
  return(ret)
}


#' @export
WinPerfCounter.Process.Metric.CPU <- function(data, process){
  keys <- c("% Processor Time", "% Privileged Time", "% User Time")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  return(ret)
}


#' @export
WinPerfCounter.Process.Metric.Memory <- function(data, process){
  keys <- c("Virtual Bytes", "Private Bytes", "Working Set")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  ret <- ret %>% dplyr::mutate(valueInGB = value/(1024*1024*1024))
}

#' @export
WinPerfCounter.Process.Metric.IOPS <- function(data, process){
  keys <- c("IO Write Operations/sec", "IO Read Operations/sec", "IO Other Operations/sec")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  return(ret)
}

#' @export
WinPerfCounter.Process.Metric.IOBytes <- function(data, process){
  keys <- c("IO Write Bytes/sec", "IO Read Bytes/sec", "IO Other Bytes/sec")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  ret <- ret %>% dplyr::mutate(valueInGB = value/(1024*1024*1024))
  return(ret)
}


# IO Write Operations/sec

#' @export
VMMap.read.header <- function(filename){
  colName <- c("Type",
              "Size","Committed","Private","Total_WS","Private_WS","Shareable_WS",
              "Shared_WS","Locked_WS","Blocks","Largest")
  read_col_type = paste(rep('c', length(colName)), collapse = '', sep = "")
  stringColumns <- c(1)
  numberCol = colName[stringColumns * -1]
  stringCol = colName[stringColumns]
  data <- read_csv(filename, skip =8, n_max = 11, col_names = colName, col_types=read_col_type) %>%
    dplyr::mutate_at(numberCol, ~ gsub(",", "", .)) %>%
    dplyr::mutate_at(numberCol, ~ dplyr::if_else(is.na(.), "0", .)) %>%
    dplyr::mutate_at(numberCol, ~ as.integer(.)) %>%
    dplyr::mutate(Type = str_replace_all(Type, " ", "_"))
  return(data)
}

#' @export
VMMap.read.body <- function(filename){
  colName <- c("Address", "Type",
               "Size","Committed","Private","Total_WS","Private_WS","Shareable_WS","Shared_WS",
               "Locked_WS","Blocks","Protection", "Details")
  read_col_type = paste(rep('c', length(colName)), collapse = '', sep = "")
  stringColumns = c(2, 12, 13)
  numberCol = colName[c(1, stringColumns) * -1] # Addressカラムは別扱いにする
  stringCol = colName[stringColumns]
  data <- read_csv(filename, skip = 33, trim_ws = FALSE,  col_names = colName, col_types=read_col_type) %>%
    dplyr::mutate_at(numberCol, ~ gsub(",", "", .)) %>%
    dplyr::mutate_at(numberCol, ~ dplyr::if_else(is.na(.), "0", .)) %>%
    dplyr::mutate_at(numberCol, ~ as.integer(.)) %>%
    dplyr::mutate_at(stringCol, ~ dplyr::if_else(is.na(.), "", .)) %>%
    dplyr::mutate(ParentOrChild = dplyr::if_else(str_detect(Address, "^  "), "Child", "Parent")) %>%
    dplyr::mutate(Address = str_replace(Address, "^  ", "")) %>%
    dplyr::mutate(AddressInt = strtoi(Address, base=16)) %>%
    dplyr::mutate(Type = str_replace_all(Type, " ", "_"))
  return(data)
}

