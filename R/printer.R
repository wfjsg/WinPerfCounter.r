library(lubridate)
library(foreach)
library(stringr)
library(dplyr)
library(formattable)
library(ggplot2)
library(ggthemes)
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
WinPerfCounter.summaryInMB <- function(df) {
  statistics <- df %>% dplyr::group_by(metric) %>%
    dplyr::summarise_at(dplyr::vars(valueInMB), dplyr::funs(
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

#' @export
WinPerfCounter.Process.PlotCPU <- function(cpu, plotConfig, processName){
  chart.conf <- plotConfig$process[[processName]]
  chart <- ggplot(cpu, aes(x = Timestamp, y=value))
  chart <- chart + ggtitle(paste("Process(", processName, ") CPU", sep = ""))
  chart <- chart + geom_point(aes(colour = metric), alpha = 0.7)
  chart <- chart + xlim(c(plotConfig$viewStart, plotConfig$viewEnd))
  chart <- chart + theme_gray()
  if( chart.conf$cpu.scale.auto ){
    ymax <-100 * plotConfig$cpucore
  }else{
    ymax <- chart.conf$cpu.max
  }
  chart <- chart + ylim(chart.conf$cpu.min, ymax)
  chart <- WinPerfCounter.DecorateForEvent(chart, plotConfig)
  return(chart)
}

#' @export
WinPerfCounter.Process.PlotMemory <- function(memory, plotConfig, processName){
  chart.conf <- plotConfig$process[[processName]]
  chart <- ggplot(memory, aes(x = Timestamp, y=valueInGB))
  chart <- chart + ggtitle(paste("Process(", processName, ") Memory", sep = ""))
  chart <- chart + geom_hline(yintercept = plotConfig$memory, linetype="dashed", colour="b lue")
  chart <- chart + annotate("text", label=paste(plotConfig$memory, "GB Installed"),
                                          x=viewStart, y=plotConfig$memory, hjust = 0.0, vjust = -0.5)
  chart <- chart + geom_point(aes(colour = metric), alpha = 0.7)
  chart <- chart + xlim(c(plotConfig$viewStart, plotConfig$viewEnd))
  if( chart.conf$memory.scale.auto == FALSE ){
    chart <- chart + ylim(chart.conf$memory.min, chart.conf$memory.max)
  }
  chart <- chart + theme_gray()
  chart <- WinPerfCounter.DecorateForEvent(chart, plotConfig)
  return(chart)
}

#' @export
WinPerfCounter.Process.PlotIOPS <- function(iops, plotConfig, processName){
  chart.conf <- plotConfig$process[[processName]]
  chart <- ggplot(iops, aes(x = Timestamp, y=value))
  chart <- chart + ggtitle(paste("Process(", processName, ") IOPS", sep = ""))
  chart <- chart + geom_point(aes(colour = metric), alpha = 0.7)
  chart <- chart + xlim(c(plotConfig$viewStart, plotConfig$viewEnd))
  if( chart.conf$iops.scale.auto == FALSE ){
    chart <- chart + ylim(chart.conf$iops.min, chart.conf$iops.max)
  }
  chart <- chart + theme_gray()
  chart <- WinPerfCounter.DecorateForEvent(chart, plotConfig)
  return(chart)
}

#' @export
WinPerfCounter.Process.PlotIOBytes <- function(iobytes, plotConfig, processName){
  chart.conf <- plotConfig$process[[processName]]
  chart <- ggplot(iobytes, aes(x = Timestamp, y=valueInMB))
  chart <- chart + ggtitle(paste("Process(", processName, ") IO Mega Byte/s", sep = ""))
  chart <- chart + geom_point(aes(colour = metric), alpha = 0.7)
  chart <- chart + xlim(c(plotConfig$viewStart, plotConfig$viewEnd))
  if( chart.conf$iobytes.scale.auto == FALSE ){
    chart <- chart + ylim(chart.conf$iobytes.min, chart.conf$iobytes.max)
  }
  chart <- chart + theme_gray()
  chart <- WinPerfCounter.DecorateForEvent(chart, plotConfig)
  return(chart)
}

#' @export
WinPerfCounter.DecorateForEvent <- function(plot, plotConfig){
  eventRangeStart <- plotConfig$eventRange[c(TRUE, FALSE)]
  eventRangeEnd <- plotConfig$eventRange[c(FALSE, TRUE)]
  if( length(plotConfig$eventVerticalLines) > 0 ){
    plot <- plot + geom_vline(xintercept = plotConfig$eventVerticalLines, alpha = 0.4, color = "coral2")
  }
  if( length(eventRangeStart) > 0 ){
    plot <- plot + annotate("rect", xmin = eventRangeStart, xmax = eventRangeEnd,
                            ymin = -Inf, ymax = Inf,
                            fill = "burlywood", alpha = 0.3)
  }
  return(plot);
}

