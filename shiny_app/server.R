# Server logic
library(shiny)
library(tidyverse)

DATA_PATH = "../twitter_dump/df4.csv"
df <- read_csv(DATA_PATH) 

server <- function(input, output, session) {
  
  rv <- reactiveValues(n=1,
                       results=data.frame(id=numeric(), text=character(), topic=character(), 
                                          eval=numeric(), duplicated=logical(), stringsAsFactors = FALSE)) 
  
  observeEvent(input$goNext, {
    if (rv$n < nrow(df)) {
      rv$n <- rv$n + 1;
      rv$results[nrow(rv$results) + 1,] = c(df$id[rv$n], df$full_text[rv$n], input$topic, input$eval, input$duplicated);
      print(rv$results);
      updateSliderInput(session, "n", value = rv$n);
      updateCheckboxInput(session, "duplicated", value = FALSE)
    }  
  })
  
  observeEvent(input$goPrevious, {
    if (rv$n > 1) {
      rv$n <- rv$n - 1;
      rv$results <- head(rv$results, -1);
      print(rv$results);
      updateSliderInput(session, "n", value = rv$n);
      updateCheckboxInput(session, "duplicated", value = FALSE)
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
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste0(input$name, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      rv$results[nrow(rv$results) + 1,] = c(df$id[rv$n], df$full_text[rv$n], input$topic, input$eval);
      write_csv(rv$results, file)
    }
  )
  
}