library(lubridate)
library(foreach)
library(stringr)
library(tidyverse)
library(readr)
library(tidyverse)

#' @export
WinPerfCounter.readcsv <- function(filename) {
  preRead <- read_csv(filename, n_max = 100,
                      locale = locale(encoding = "cp932"),
                      col_types = cols())
  # colnames(preRead)-2, -2 is DateTime and last comments.
  colTypes <- paste("c", paste(rep('d', length(colnames(preRead))-2), collapse=""), "c", sep = "")

  data <- read_csv(filename,
                   locale = locale(encoding = "cp932"),
                   col_names = TRUE,
                   col_types = colTypes)
  #data <- read.csv(filename,
  #              check.names = FALSE,
  #              header = TRUE,
  #              as.is = TRUE)
  #              # fileEncoding = "UTF-8-BOM")
  # options(digits.secs=3)
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
  ret <- tmp %>%
    tidyr::gather(key = metric, value = value, -Timestamp, factor_key = TRUE)
  return(ret)
}


smartResample <- function(data, interval){
  ret <- data %>%
    dplyr::mutate(classTimestamp = round_date(Timestamp, interval)) %>%
    dplyr::group_by(classTimestamp, metric) %>%
    dplyr::summarise_at(dplyr::vars(value), dplyr::funs(max)) %>%
    dplyr::mutate(Timestamp = classTimestamp)
  return(ret)
}


#' @export
smartResampleMB <- function(data, interval){
  ret <- data %>%
    dplyr::mutate(classTimestamp = round_date(Timestamp, interval)) %>%
    dplyr::group_by(classTimestamp, metric) %>%
    dplyr::summarise_at(dplyr::vars(valueInMB), dplyr::funs(max)) %>%
    dplyr::mutate(Timestamp = classTimestamp)
  return(ret)
}


#' @export
smartResampleGB <- function(data, interval){
  ret <- data %>%
    dplyr::mutate(classTimestamp = round_date(Timestamp, interval)) %>%
    dplyr::group_by(classTimestamp, metric) %>%
    dplyr::summarise_at(dplyr::vars(valueInGB), dplyr::funs(max)) %>%
    dplyr::mutate(Timestamp = classTimestamp)
  return(ret)
}

#' @export
WinPerfCounter.Metric.CPU <- function(data, resample = FALSE){
  keys <- c("% Processor Time", "% Privileged Time", "% User Time")
  ret <- makeTidyMetric(data, "Processor(_Total)", keys)
  if( resample != FALSE ){
    return( smartResample(ret, resample) )
  }else{
    return(ret)
  }
}

#' @export
WinPerfCounter.Metric.GPU.UtilizationPercentage <- function(data, resample = FALSE, addr){
  submetric <- "Utilization Percentage"

  # TODO code clear
  df <- data_frame(original_col_name = colnames(data)) %>%
    dplyr::filter(str_detect(original_col_name, "GPU Engine")) %>%
    tidyr::separate(original_col_name, into = c('raw_category', 'metric'),
                    sep = '\\|', remove = FALSE) %>%
    dplyr::mutate(category = str_replace(raw_category, "^GPU Engine\\(", "")) %>%
    dplyr::mutate(category = str_replace(category, "\\)$", "")) %>%
    tidyr::separate(category, into = c('str_pid', 'pid', 'str_luid', 'uid',
                                       'gpu_addr', 'str_phys', 'phys_number',
                                       'str_engtype', 'engtype'),
                    sep ='_', extra='merge', remove = FALSE)

  category_name <- df %>%
    dplyr::filter(gpu_addr == addr) %>%
    dplyr::pull(original_col_name) %>%
    c('Timestamp', levels(.))

  df2 <- df %>%
    dplyr::select(original_col_name, gpu_addr, engtype, metric) %>%
    dplyr::mutate(original_col_name = as_factor(original_col_name))

  ret <- data %>%
    dplyr::select(dplyr::one_of(category_name)) %>%
    tidyr::gather(key = original_col_name, value = value, -Timestamp, factor_key = TRUE)

  ret <- ret %>%
    dplyr::mutate(original_col_name = as_factor(original_col_name)) %>%
    dplyr::left_join(df2, by=c('original_col_name' = 'original_col_name'))
    dplyr::filter(metric == submetric)

  # TODO: call Resampling!!

  return(ret)
  # df3 <- data %>%
  #   dplyr::select(one_of(df2))
}

#' @export
WinPerfCounter.Metric.GPU_Memory <- function(data, resample = FALSE, addr){
  keys <- c("Shared Usage", "Dedicated Usage", "Total Committed")
  category_name <- paste("GPU Adapter Memory", "(luid_0x00000000_", addr, "_phys_0)", sep = "")
  ret <- makeTidyMetric(data, category_name, keys) %>%
    dplyr::mutate(valueInMB = value/(1024*1024))
  if( resample != FALSE ){
    return( smartResample(ret, resample) )
  }else{
    return(ret)
  }
}


#' @export
WinPerfCounter.Metric.Memory <- function(data, resample = FALSE){
  keys <- c("Available Bytes", "Committed Bytes", "Commit Limit")
  ret <- makeTidyMetric(data, "Memory", keys)
  ret <- ret %>% dplyr::mutate(valueInGB = value/(1024*1024*1024))
  if( resample != FALSE ){
    return( smartResampleGB(ret, resample) )
  }else{
    return(ret)
  }
}

#' @export
WinPerfCounter.Process.Metric.CPU <- function(data, process, resample = FALSE){
  keys <- c("% Processor Time", "% Privileged Time", "% User Time")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  if( resample != FALSE ){
    return( smartResample(ret, resample) )
  }else{
    return(ret)
  }
}


#' @export
WinPerfCounter.Process.Metric.Memory <- function(data, process, resample = FALSE){
  keys <- c("Virtual Bytes", "Private Bytes", "Working Set")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  ret <- ret %>% dplyr::mutate(valueInGB = value/(1024*1024*1024))
  if( resample != FALSE ){
    return( smartResampleGB(ret, resample) )
  }else{
    return(ret)
  }
}

#' @export
WinPerfCounter.Process.Metric.IOPS <- function(data, process, resample = FALSE){
  keys <- c("IO Write Operations/sec", "IO Read Operations/sec", "IO Other Operations/sec")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  if( resample != FALSE ){
    return( smartResample(ret, resample) )
  }else{
    return(ret)
  }
}

#' @export
WinPerfCounter.Process.Metric.IOBytes <- function(data, process, resample = FALSE){
  keys <- c("IO Write Bytes/sec", "IO Read Bytes/sec", "IO Other Bytes/sec")
  ret <- makeTidyMetric(data, paste("Process(", process, ")", sep = ""), keys)
  ret <- ret %>% dplyr::mutate(valueInMB = value/(1024*1024))
  if( resample != FALSE ){
    return( smartResampleMB(ret, resample) )
  }else{
    return(ret)
  }
}


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
  data <- read_csv(filename, skip = 15, trim_ws = FALSE,  col_names = colName, col_types=read_col_type) %>%
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

#' @export
listup_gpu_engine <- function(data){
  candidates <- colnames(data)
  e2 <- candidates %>%
    purrr::map(~ str_split_fixed(., '\\|', 2)) %>%
    purrr::map_dfr(~ data_frame(v = .[1], e = .[2])) %>%
    dplyr::filter(str_detect(v, pattern = "GPU Process Memory")) %>%
    dplyr::select(v) %>%
    dplyr::distinct(v) %>%
    dplyr::mutate(f = str_match(v, "luid_0x[0-9A-Fa-f]+_(0x[0-9A-Fa-f]+)")[,2]) %>%
    dplyr::distinct(f) %>%
    dplyr::pull(f)
  return(e2)
}
