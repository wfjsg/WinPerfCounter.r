#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(WinPerfCounter)
library(ggplot2)
library(shiny)

# load data
csvfile <- "../tests/testthat/with_gpu.csv"
# csvfile <- "tests/testthat/with_gpu.csv"
dataRaw <- WinPerfCounter.readcsv(csvfile)
column_df <- purrr::map(colnames(dataRaw), ~ str_split_fixed(., '\\|', 2)) %>%
  purrr::map_dfr(~ data_frame(v = .[1], e = .[2]))
category <- column_df %>%
  dplyr::filter(e != "") %>%
  dplyr::select(v) %>%
  dplyr::distinct() %>%
  dplyr::pull(v)
data <- dataRaw
#data <- dataRaw %>%
#  tidyr::gather(key = metric, value = value, -Timestamp, -DateTime, factor_key = TRUE)


get_secondary_keys <- function(first_key){
  e <- column_df %>%
    dplyr::filter(v == first_key) %>%
    dplyr::pull(e)
  e
}
# cur_column <- c('Memory|Page Faults/sec', 'Memory|Committed Bytes')
cur_column <- get_secondary_keys(category[1])

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("WinPerfCounter Quick Viewer"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput("category", label = "category",
                    choices = category),
        checkboxGroupInput("draw_column",
                           label = "selected_draw",
                           choices = cur_column)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)


internalRenderPlot <- function(x, y){
  if( length(y) == 0){
    return(ggplot())
  }

  full_column_names <- paste(x, y, sep = '|') %>% c('Timestamp')
  my_df <- data %>%
    dplyr::select(dplyr::one_of(full_column_names)) %>%
    tidyr::gather(key = metric, value = value, -Timestamp, factor_key = TRUE)
  g <- ggplot(my_df, aes(x = Timestamp, y = value))
  g <- g + geom_point(aes(colour = metric), alpha = 0.7)
  return(g)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   output$distPlot <- renderPlot({
     p <- internalRenderPlot(input$category, input$draw_column)
     print(p)
   })
   observeEvent(c(input$category),{
     new_choise <- column_df %>%
       dplyr::filter(v == input$category) %>%
       dplyr::pull(e)
     updateCheckboxGroupInput(session, "draw_column", choices = new_choise)
   })
}


# Run the application
shinyApp(ui = ui, server = server)

