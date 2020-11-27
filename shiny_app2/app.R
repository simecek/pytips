library(shiny)
library(tidyverse)

DATA_PATH = "df5.csv"  # path to current d.f. with tweets
all_tweets <- read_csv(DATA_PATH)
NTWEETS <- 2  # how many tweets should be evaluated in each sesseion
CAMPAINS <- c('pyladies', 'twitter', 'linkedin')

thankyou_messages = list()
thankyou_messages[['pyladies']] = "<br/><h3> Děkujeme! </h3> 
  <p>A přejeme 
  úspěšné dokončení datového kurzu PyLadies. Když nám vyplníš jméno a email, pošleme ti sbírku
  nejlepších pythonových tipů, až ji teda budeme mít hotovou.</p>
  
  <p>Nezapomeň, prosím, kliknout na tlačítko Save, jinak se tvé odpovědi neuloží.</p>
  <br/>"

thankyou_messages[['twitter']] = thankyou_messages[['linkedin']] = "<br/><h3> Thank you! </h3> 
  <p>We wish you merry python holidays! If you share your name and email with us, we will 
  send you a 75% discount code once the book of the tips is ready.</p>
  
  <p>Please, do not forget to hit the \"Save\" button. Otherwise, your answers will not be recorded.</p>
  <br/>"

thankyou_messages[['default']] = "<br/><h3> Thank you! </h3> 
  <p>We wish you merry python holidays! If you share your name and email with us, we will 
  send you a 50% discount code once the book of the tips is ready.</p>
  
  <p>Please, do not forget to hit the \"Save\" button. Otherwise, your answers will not be recorded.</p>
  <br/>"

server <- function(input, output, session) {
  
  campaign <-"default"
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['campaign']])) {
      campaign <- query[['campaign']]
      if (!(campaign %in% CAMPAINS)) campaign <-"default"
    } 
  })
  
  df <- all_tweets[sample(nrow(all_tweets), NTWEETS), ]
  starttime <- Sys.time()
  
  rv <- reactiveValues(n = 1,
                       done = FALSE,
                       results = data.frame(id=numeric(), text=character(), 
                                          eval=numeric(), duplicated=logical(), stringsAsFactors = FALSE)) 
  
  observeEvent(input$goNext, {
    if (rv$n < nrow(df)) {
      rv$results[nrow(rv$results) + 1,] = c(df$id[rv$n], df$full_text[rv$n], input$eval, input$duplicated);
      rv$n <- rv$n + 1;
      updateSliderInput(session, "n", value = rv$n);
      updateCheckboxInput(session, "duplicated", value = FALSE)
    }  
  })
  
  observeEvent(input$goPrevious, {
    if (rv$n > 1) {
      rv$n <- rv$n - 1;
      rv$results <- head(rv$results, -1);
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
  
  output$thanks <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['campaign']])) {
      campaign <- query[['campaign']]
      if (!(campaign %in% CAMPAINS)) campaign <-"default"
    } else {
      campaign <-"default"
    }
    
    HTML(thankyou_messages[[campaign]])
  })
  
  output$finished = renderText({
    if (rv$done) {
      return('Your answers have been saved.')
    } else {
      return('')
    }
  })
  
  observeEvent(input$saveResults, {
    if (!rv$done) {
      
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['campaign']])) {
        campaign <- query[['campaign']]
        if (!(campaign %in% CAMPAINS)) campaign <-"default"
      } else {
        campaign <-"default"
      }
      
      timestamp = as.numeric(Sys.time())
      
      filename = paste0("answers/", input$name, "-", timestamp, ".csv")
      content = rv$results
      content[nrow(rv$results) + 1,] = c(df$id[rv$n], df$full_text[rv$n], 
                                              input$eval, input$duplicated)
      write_csv(content, filename)
      
      filename2 = paste0("responders/", input$name, "-", timestamp, ".txt")
      duration = round(as.numeric(difftime(Sys.time(), starttime, units="secs")))
      content2 = paste(input$name, timestamp, input$email, input$pylevel, campaign, duration, sep="\t")
      writeLines(content2, filename2)
      
      rv$done = TRUE
    }  
  })
}  





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
              max = NTWEETS,
              value = 1,
              width = "60%"),
  
  column(3,
         
         radioButtons("eval", "Quality of the tweet (7=best, 1=worst):", selected = "4", inline = TRUE,
                      choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7")),
         checkboxInput("duplicated", "I do not understand this tweet"), 
         actionButton("goPrevious", "Previous"),
         actionButton("goNext", "Next"),
         
         conditionalPanel(condition = paste("input.n ==", NTWEETS),
                          htmlOutput("thanks")),
         
         conditionalPanel(condition = paste("input.n ==", NTWEETS),
                          textInput("name", "Your name:", "")),
         
         conditionalPanel(condition = paste("input.n ==", NTWEETS),
                          textInput("name", "Your email:", "")),
         
         conditionalPanel(condition = paste("input.n ==", NTWEETS),
                          radioButtons("pylevel", "Regarding Python, I am", selected = "Intermediate", inline = TRUE,
                                       choices = c("Beginner" = "Beginner", "Intermediate" = "Intermediate", "Advanced" = "Advanced"))),
         
         conditionalPanel(condition = paste("input.n ==", NTWEETS),
                          actionButton("saveResults", "Save", icon = icon("download"))),
         conditionalPanel(condition = paste("input.n ==", NTWEETS),
                          textOutput('finished'))
  ),
  
  column(9,
         # Show a plot of the generated distribution
         uiOutput("tweet")),
)

# Run the application 
shinyApp(ui = ui, server = server)