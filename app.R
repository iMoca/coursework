#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


rsconnect::setAccountInfo(name='ioanamoca',
                          token='3748CDA7FAA5225AAA372202390F2550',
                         secret='2uJC0vNnkizrOMnQB3+TzPnDYOndoL+jjl2nuEhD')
library(rsconnect)
library(wordcloud)
library(shiny)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(NLP)
library(tidyverse)
library(stringr)
library(tidytext)
library(textstem)
library(dplyr)
library(lexicon)
library(tidyr)

deployApp()



# Define server logic to read selected file ----
server <- function(input, output) {
  
  #Reading the Input File
  input_file <- reactive({
    if (is.null(input$file)) {
      return("Please load a file!")
    }
    # Read the text in the uploaded file
    readLines(input$file$datapath)
  })
  
  getTermMatrix <- function(input, output, session) {
    docs <- Corpus(VectorSource(input_file()))
    Spaceifier <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, Spaceifier, "/")
    docs <- tm_map(docs, Spaceifier, "@")
    docs <- tm_map(docs, Spaceifier, "\\|")
    #docs <- tm_map(docs, Spaceifier, """)
    #docs <- tm_map(docs, Spaceifier, """)
    #docs <- tm_map(docs, Spaceifier, "'")
    #docs <- tm_map(docs, Spaceifier, "-")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, lemmatize_strings)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    sort(rowSums(m),decreasing=TRUE)
  }
  
 
  
  #Generate sentiment corpus
  sentimentInput <- reactive({
    switch(input$sentiment,
           "Anger" = "anger", 
           "Anticipation" = "anticipation",
           "Disgust" = "disgust", 
           "Fear" = "fear", 
           "Joy" = "joy",
           "Sadness" = "sadness",
           "Surprise" = "surprise", 
           "Trust" = "trust")
  })
  
  
   
    #Output the first few lines of the uploaded text
    output$contents <- renderText({
        req(input$file$datapath)
        readLines(paste(input$file$datapath), 10)
        
    })
    
    #Defining an expression for our term document matrix
    terms <- reactive ({getTermMatrix(input_file())})
    wordcloud_rep <- repeatable(wordcloud)
    

    
    #Generating the wordcloud
    output$plot <- renderPlot({
      v <- terms()
      d <- data.frame(word = names(v),freq=v)
      nrcsentiment <- get_sentiments("nrc") %>%   
        filter(sentiment == input$sentiment)
      nrcsentimentjoin <- d %>%
          inner_join(nrcsentiment, by = "word")
      wordcloud_rep(nrcsentimentjoin$word, nrcsentimentjoin$freq,
                    min.freq=input$slider1,
                    max.words=input$slider2,
                    random.order= FALSE,
                    rot.per=input$slider3,
                    colors=brewer.pal(8, "Dark2"))
      
      #Generate Story Progression plot
      
      #Defining a reactive expression for the tidy dtm
      tidyup <- function(input, output, session) {
        docs <- Corpus(VectorSource(input_file()))
        docs <- tm_map(docs, content_transformer(tolower))
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, removeWords, stopwords("english"))
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, stripWhitespace)
        docs <- tm_map(docs, lemmatize_strings)
        dtm <- TermDocumentMatrix(docs)
        dtm_tidy <- tidy(dtm)
        id <- as.numeric(rownames(dtm_tidy))
        dtm_tidy <- cbind(id=id, dtm_tidy)
        dtm_tidy %>%
          dplyr::mutate(word=term)
        
      }
      
      
      tidyterms <- reactive ({tidyup(input_file())})
      
      
      output$plot2 <- renderPlot({
        Progression <- tidyterms() %>%
          inner_join(get_sentiments("nrc"), by = 'word') %>%   
                       filter(sentiment == input$sentiment) %>%
          dplyr::group_by(index = id %/% 500) %>%  
          dplyr::summarise(sentiment = n()) %>%   
          dplyr::mutate(method = "NRC")
        Progression %>%                     
          ggplot(aes(index, sentiment)) +
          geom_col(show.legend = FALSE,
                   fill = "purple") + 
          xlab("Book Progression") + 
          ylab("Sentiment") + 
          labs(title = "Progression of Chosen Sentiment in The Text") +
          theme_minimal()
      })
      
      
      
      })
   
}


ui <- fluidPage(
  
  # App title ----
  titlePanel("Story Miner and Sentiment Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      
      strong("Settings:"),
      
      # Text file uploader
      fileInput("file", "Choose your text file", accept=c("text/plain", ".txt")),
    
      # Slider input for frequency change
     sliderInput("slider1", "Minimum Frequency:",
                min = 1, max = 50, value = 5),
    
      # Slider input for rotation change
      sliderInput("slider3", "Rotation:",
                min = 0.0, max = 1.0, value = 0.35),
    
      # Slider input for number of words change
      sliderInput("slider2", "Max words:",
                min = 10, max = 1000, value = 100),
    
    # Emotion for Sentiment Analysis
      selectInput("sentiment", "Choose a sentiment:", 
                  choices = c("anger", "anticipation", "disgust", "fear", "joy","sadness","surprise", "trust"), selected = "joy", multiple = FALSE)),
      
    
    
      # Main panel for displaying outputs
      mainPanel(
      
        # Output: 
        textOutput("contents"),
        plotOutput("plot"),
        plotOutput("plot2")
        )
    )
)



# Run the app ----
shinyApp(ui = ui, server = server)
