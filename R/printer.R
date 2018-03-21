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
WinPerfCounter.Process.PlotCPU <- function(cpu, processes, cpu.core, processName){
  chart.conf <- processes[[processName]]
  cpu_chart <- ggplot(cpu, aes(x = Timestamp, y=value))
  cpu_chart <- cpu_chart + ggtitle(paste("Process(", processName, ") CPU", sep = ""))
  cpu_chart <- cpu_chart + geom_point(aes(colour = metric))
  if( chart.conf$cpu.scale.auto ){
    cpu_chart <- cpu_chart + ylim(0, 100 * sysenv.cpucore)
  }else{
    cpu_chart <- cpu_chart + ylim(chart.conf$cpu.min, chart.conf$cpu.max)
  }
  cpu_chart <- cpu_chart + theme_gray()
  return(cpu_chart)
}

#' @export
WinPerfCounter.Process.PlotMemory <- function(memory, processes, cpu.core, processName){
  chart.conf <- processes[[processName]]
  memory_chart <- ggplot(memory, aes(x = Timestamp, y=valueInGB))
  memory_chart <- memory_chart + ggtitle(paste("Process(", processName, ") Memory", sep = ""))
  memory_chart <- memory_chart + geom_hline(yintercept = sysenv.memory, linetype="dashed", colour="b lue")
  memory_chart <- memory_chart + annotate("text", label=paste(sysenv.memory, "GB Installed"),
                                          x=viewStart, y=sysenv.memory, hjust = 0.0, vjust = -0.5)
  memory_chart <- memory_chart + geom_point(aes(colour = metric))
  if( chart.conf$memory.scale.auto == FALSE ){
    memory_chart <- memory_chart + ylim(chart.conf$memory.min, chart.conf$memory.max)
  }
  memory_chart <- memory_chart + theme_gray()
  return(memory_chart)
}

#' @export
WinPerfCounter.Process.PlotIOPS <- function(iops, processes, cpu.core, processName){
  chart.conf <- processes[[processName]]
  chart <- ggplot(iops, aes(x = Timestamp, y=value))
  chart <- chart + ggtitle(paste("Process(", processName, ") IOPS", sep = ""))
  chart <- chart + geom_point(aes(colour = metric))
  if( chart.conf$iops.scale.auto == FALSE ){
    chart <- chart + ylim(chart.conf$iops.min, chart.conf$iops.max)
  }
  chart <- chart + theme_gray()
  return(chart)
}



