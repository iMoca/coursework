
#rsconnect::setAccountInfo(name='gabrielvesco',
                         # token='56570F36ACE2786A32B3FEB774F91244',
                         # secret='<SECRET>')
library(rsconnect)
## ui.R and server.R
library(shiny)
library(shinydashboard)
library(proxy)
library(recommenderlab)
library(reshape2)
library(plyr)
library(dplyr)
library(DT)
library(RCurl)

source('https://raw.githubusercontent.com/ChicagoBoothML/MachineLearning_Fall2015/master/Programming%20Scripts/Book%20Recommendation/R/ParseData.R')
suppressMessages(data <- parse_book_crossing_data())
Books <- data$books
Users <- data$users
Ratings <- data$ratings
Ratings[ , `:=`(user_id = factor(user_id), isbn = factor(isbn))]
ratings_per_user <- dcast(Ratings, user_id ~ ., value.var='book_rating')
ratings_per_book <- dcast(Ratings, isbn ~ ., value.var='book_rating')
userswithsufficientratings <- ratings_per_user[. >= 2, user_id]
bookswithsufficientratings <- ratings_per_book[. >=2, isbn]
Ratings <- Ratings[user_id %in% userswithsufficientratings, ]
Ratings <- Ratings[isbn %in% bookswithsufficientratings, ]
set.seed(1234)
Ratingssubset <- Ratings[sample(nrow(Ratings), 700, replace = FALSE, prob = NULL),]


Books <- Books %>% filter_all(all_vars(!grepl('http',.)))
Books <- Books[with(Books, order(book_title)), ]




server <- function(input, output) {
  
  # Text for the 3 boxes showing average scores
  formulaText1 <- reactive({
    paste(input$select)
  })
  formulaText2 <- reactive({
    paste(input$select2)
  })
  formulaText3 <- reactive({
    paste(input$select3)
  })
  
  output$book1 <- renderText({
    formulaText1()
  })
  output$book2 <- renderText({
    formulaText2()
  })
  output$book3 <- renderText({
    formulaText3()
  })
  
  
  # Table containing recommendations
  output$table <- renderTable({
    
    #cat1 <- subset(Books, title==input$select)
    #cat2 <- subset(Books, title==input$select2)
    #cat3 <- subset(Books, title==input$select3)
  
    
    book_recommendation <- function(input,input2,input3){
      row_num <- which(Books[,3] == input)
      row_num2 <- which(Books[,3] == input2)
      row_num3 <- which(Books[,3] == input3)
      userSelect <- matrix(NA,length(unique(Ratingssubset$isbn)))
      userSelect[row_num] <- 5 #hard code first selection to rating 5
      userSelect[row_num2] <- 4 #hard code second selection to rating 4
      userSelect[row_num3] <- 4 #hard code third selection to rating 4
      userSelect <- t(userSelect)
      
      ratingmat <- reshape2::dcast(Ratingssubset, user_id~isbn, value.var = "book_rating", na.rm=FALSE)
      ratingmat <- ratingmat[,-1]
      colnames(userSelect) <- colnames(ratingmat)
      ratingmat2 <- rbind(userSelect,ratingmat)
      ratingmat2 <- as.matrix(ratingmat2)
      
      #Convert rating matrix into a sparse matrix
      ratingmat2 <- as(ratingmat2, "realRatingMatrix")
      
      #Create Recommender Model
      recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=10))
      recom <- predict(recommender_model, ratingmat2[1], n=10)
      recom_list <- as(recom, "list")
      recom_result <- data.frame(matrix(NA,10))
      recom_result[1:10,1] <- Books[as.integer(recom_list[[1]][1:10]),3]
      recom_result <- data.frame(na.omit(recom_result[order(order(recom_result)),]))
      recom_result <- data.frame(recom_result[1:5,])
      colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
      return(recom_result)
    }
    
    book_recommendation(input$select, input$select2, input$select3)
    
  })
  
  book.ratings <- merge(Ratingssubset,Books, by='isbn')
  
  output$tableRatingssubset1 <- renderValueBox({
    book.avg1 <- summarise(subset(book.ratings, book_title==input$select),
                           Average_Rating1 = mean(book_rating, na.rm = TRUE))
    valueBox(
      value = format(book.avg1, digits = 3),
      subtitle = input$select,
      icon = if (book.avg1 >= 6) icon("thumbs-up") else icon("thumbs-down"),
      color = if (book.avg1 >= 6) "aqua" else "red"
    )
    
  })
  
  output$tableRatings2 <- renderValueBox({
    book.avg2 <- summarise(subset(book.ratings, book_title==input$select2),
                           Average_Rating = mean(book_rating, na.rm = TRUE))
    valueBox(
      value = format(book.avg2, digits = 3),
      subtitle = input$select2,
      icon = if (book.avg2 >= 6) icon("thumbs-up") else icon("thumbs-down"),
      color = if (book.avg2 >= 6) "aqua" else "red"
    )
  })
  
  output$tableRatings3 <- renderValueBox({
    book.avg3 <- summarise(subset(book.ratings, book_title==input$select3),
                           Average_Rating = mean(book_rating, na.rm = TRUE))
    valueBox(
      value = format(book.avg3, digits = 3),
      subtitle = input$select3,
      icon = if (book.avg3 >= 6) icon("thumbs-up") else icon("thumbs-down"),
      color = if (book.avg3 >= 6) "aqua" else "red"
    )
  })
  
  
  # Generate a table summarizing each players stats
  output$myTable <- renderDataTable({
    Books[c("book_title")]
  })
  
}


ui <- dashboardPage(skin="blue",
                     dashboardHeader(title = "Book Recommenders"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Books", tabName = "Books", icon = icon("star-o")),
                         menuItem(
                           list(
                             
                             selectInput("select", label = h5("Select 3 Books That You Like"),
                                         choices =as.character(Books$book_title[1:length(unique(Books$isbn))]),
                                         selectize = FALSE,
                                         selected = "The Lovely Bones"),
                             selectInput("select2", label = h5("Book #2"),
                                         choices =as.character(Books$book_title[1:length(unique(Books$isbn))]),
                                         selectize = FALSE,
                                         selected = "Wild Animus"),
                             selectInput("select3", label = h5("Book #3"),
                                         choices =as.character(Books$book_title[1:length(unique(Books$isbn))]),
                                         selectize = FALSE,
                                         selected = "The Da Vinci Code"),
                             submitButton("Submit")
                           )
                         )
                       )
                     ),
                     
                     
                     dashboardBody(
                       tags$head(
                         tags$style(type="text/css", "select { max-width: 360px; }"),
                         tags$style(type="text/css", ".span4 { max-width: 360px; }"),
                         tags$style(type="text/css",  ".well { max-width: 360px; }")
                       ),
                       
                        tabItems(
                          tabItem(tabName = "Books",
                               fluidRow(
                                 box(
                                   width = 6, status = "info", solidHead = TRUE,
                                   title = "Other Books You Might Like",
                                   tableOutput("table")),
                                 valueBoxOutput("tableRatings1"),
                                 valueBoxOutput("tableRatings2"),
                                 valueBoxOutput("tableRatings3"),
                                 HTML('<br/>'),
                                 box(DT::dataTableOutput("myTable"), title = "Table of All Books", width=12, collapsible = TRUE)
                               )
                       )
                     ))
)


shinyApp(ui = ui, server = server)



