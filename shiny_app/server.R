# Server logic
library(tidyverse)

DATA_PATH = "../twitter_dump/df4.csv"
df <- read_csv(DATA_PATH) 

server <- function(input, output) {
  
  # using https://community.rstudio.com/t/embed-twitter-tweet-in-shiny-app/51364
  output$tweet <- renderUI({
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = df$url[input$n])),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
  })
  
}