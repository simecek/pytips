library(shiny)
library(tidyverse)

TOPICS_PATH = "data/topics.txt"
topics <- readLines(TOPICS_PATH)
DATA_PATH = "../twitter_dump/df4.csv"
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
              width = "100%"),

  selectInput("topic", "Choose topic:", topics),
  sliderInput("eval", "Quality of tweet:", min = 1, max = 5, value = 3),
  actionButton("run2", "Previous"),
  actionButton("run", "Next"),
  
    
  # Show a plot of the generated distribution
  uiOutput("tweet")
  

)
