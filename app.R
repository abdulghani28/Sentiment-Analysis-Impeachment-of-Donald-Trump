library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(tm)
library(wordcloud2)
library(caret)
library(RTextTools)
library(e1071)


#load dataset
trump_tweets = read.csv("~/Sentiment_Analysis/dataset/trumpResult1.csv", sep = ";")

#klasifikasi ke dalam matriks
t.tweets<-as.matrix(trump_tweets[trump_tweets$klasifikasi
                                 %in% c("Positif","Negatif","Netral")
                                 ,])

#Naive Bayes
indexes <- createDataPartition(t.tweets[,2], p=0.6, list = FALSE)
train.data <- t.tweets[indexes,]
test.data <- t.tweets[-indexes,]

classifier = naiveBayes(train.data, as.factor(train.data[,2]) )

#Tes Validitas Naive Bayes
predicted = predict(classifier, test.data[,1]); predicted
table(test.data[, 2], predicted)
nb_classifier = naiveBayes(train.data, as.factor(train.data[,2]) )
summary(nb_classifier)
result = confusionMatrix(as.factor(test.data[, 2]), predicted)


#Build a term-document matrix
komen <- trump_tweets$text
komenc <- Corpus(VectorSource(komen))
{
  dtm <- TermDocumentMatrix(komenc)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  a <- data.frame(word = names(v),freq=v)
}
head(a,n=100)

#wordcloud

#Barplot word

#Barplot Sentimen Analisis


# Proses Shiny

ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "Pilihan"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Naive Bayes", tabName = "tablee", icon = icon("car")),
                         menuItem("term document matrix", tabName = "resultt", icon = icon("car")),
                         menuItem("Wordcloud", tabName = "Wordcloud", icon = icon("car")),
                         menuItem("Frequent Word", tabName = "FrequentWord", icon = icon("car")),
                         menuItem("Sentimen Analisis", tabName = "SentimenAnalisis", icon = icon("tree"))
                         
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem("tablee",
                                 fluidPage(
                                   h1("Naive Bayes")
                                 ),
                                 tableOutput("tableee")
                         ),
                         
                         tabItem("resultt",
                                 fluidPage(
                                   h1("term document matrix")
                                 ),
                                 verbatimTextOutput("resulttt")
                         ),
                         
                         tabItem("SentimenAnalisis",
                                 fluidPage(
                                   h1("Analisis Sentiment")
                                 ),
                                 plotOutput("correlation")
                         ),
                         
                         tabItem("Wordcloud",
                                 fluidPage(
                                   h1("WordCloud")
                                 ), 
                               wordcloud2Output("wcplot")
                                 
                         ),
                         tabItem("FrequentWord",
                                 fluidPage(
                                   h1("Frequent Word"),
                                   box(plotOutput("FrequentWord"), width = 500)
                                 )       
                         )
                         
                       )
                     )
)

server <- function(input,output) {
  #Tes Validitas Naive Bayes
  output$tableee <- renderTable({
    predicted = predict(classifier, test.data[,1]); predicted
    table(test.data[, 2], predicted)
  })
  
  #result 
  #Build a term-document matrix
  output$resulttt <- renderPrint({
    nb_classifier = naiveBayes(train.data, as.factor(train.data[,2]) )
    summary(nb_classifier)
    result = confusionMatrix(as.factor(test.data[, 2]), predicted)
    result
  })
  
  #Barplot Sentimen Analisis
  output$correlation <- renderPlot({
    barplot(klas$jumlah, main = "Analisis Sentimen", xlab = "Jenis Sentimen",
            ylab = "Jumlah Sentimen", names.arg = klas$klasifikasi, col = c("red", "lightblue", "lightgreen"))
  })
  
  #wordcloud
  output$wcplot <- renderWordcloud2({
    wordcloud2(a,shape = "star",
               backgroundColor = "white",
               color = 'random-light' ,size = 0.3)
  })
  
  #Barplot word Analisis
  klas <- read.csv("~/Sentiment_Analysis/dataset/klasifikasi.csv", sep = ";", header = TRUE)
  output$FrequentWord <- renderPlot({
    barplot(a[1:10,]$freq, las = 2, names.arg = a[1:10,]$word,
            col ="lightgreen", main ="Most frequent words",
            ylab = "Word frequencies")
  })
  
  
}

shinyApp(ui, server)

