library(shiny)
library(tidyverse)

TOPICS_PATH = "data/topics.txt"
topics <- readLines(TOPICS_PATH)
DATA_PATH = "../twitter_dump/df3.csv"
df <- read_csv(DATA_PATH)

ui <- fluidPage(
  
  tags$head(
    tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
  ),
  
  # Application title
  titlePanel("Pytips Evaluator"),
  
  # Sidebar with a slider input
  sliderInput("n",
              "Tweet nb:",
              min = 1,
              max = nrow(df),
              value = 1,
              width = "60%"),

  column(3,
  
  selectInput("topic", "Choose topic:", topics, selectize = FALSE, size = length(topics)),
  radioButtons("eval", "Quality of the tweet:", selected = "4", inline = TRUE,
               choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7")),
  checkboxInput("duplicated", "Duplicated (already seen)"), 
  actionButton("goPrevious", "Previous"),
  actionButton("goNext", "Next")
  ),
    
  column(9,
  # Show a plot of the generated distribution
  uiOutput("tweet")),
  
  conditionalPanel(condition = "input.n == 515",
      textInput("name", "To be saved under a name:", "")),
  
  conditionalPanel(condition = "input.n == 515",
                   downloadButton("downloadResults", "Save"))
)
