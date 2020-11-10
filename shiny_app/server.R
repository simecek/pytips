# Server logic
library(shiny)
library(tidyverse)

DATA_PATH = "../twitter_dump/df4.csv"
df <- read_csv(DATA_PATH) 

server <- function(input, output, session) {
  
  rv <- reactiveValues(n=1,
                       results=data.frame(id=numeric(), topic=character(), eval=numeric(), stringsAsFactors = FALSE)) 
  
  observeEvent(input$run, {
    if (rv$n < nrow(df)) {
      rv$n <- rv$n + 1;
      rv$results[nrow(rv$results) + 1,] = c(df$id[rv$n], input$topic, input$eval);
      print(rv$results);
      updateSliderInput(session, "n", value = rv$n);
    }  
  })
  observeEvent(input$run2, {
    if (rv$n > 1) {
      rv$n <- rv$n - 1;
      rv$results <- head(rv$results, -1);
      print(rv$results);
      updateSliderInput(session, "n", value = rv$n);
    } 
  })
  
  # using https://community.rstudio.com/t/embed-twitter-tweet-in-shiny-app/51364
  output$tweet <- renderUI({
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = df$url[rv$n])),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
  })
  
}