library(shiny)
library(knitr)
library(stringr)
library(stringi)
library(tm)
library(wordcloud)
library(ggplot2)
library(RWeka)
description <- function(text,filename){
        des <- data.frame(stri_stats_general(text))
        NumWords <-sum(stri_count_words(text))
        des <- rbind(des,NumWords)
        rownames(des)[5] <- "NumWords"
        colnames(des) <- filename
        des
}
setwd('F:/UserData/My Documents/dataProduct/final/en_US')
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
set.seed(1)
blogs <- sample(blogs, round(length(blogs)/100,0))
news <- sample(news, round(length(news)/100,0))
twitter <- sample(twitter, round(length(twitter)/100,0))
lang <- c(blogs, news, twitter)

Encoding(lang) <- "latin1"
lang <-iconv(lang, "latin1", "ASCII", sub="")
lang <- qdap::sent_detect(lang, language = "en", model = NULL) 
corpus = Corpus(VectorSource(lang))
corpus <- tm_map(corpus, content_transformer(function(x)
        iconv(x, to = "UTF-8", sub = "byte")))
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords("english"))
grams <- function(corpus, num, bar = TRUE, cloud=FALSE) {
        gram <- NGramTokenizer(corpus, Weka_control(min = num, max = num, delimiters = " \\r\\n\\t.,;:\"()?!"))
        gram <- data.frame(table(gram))
        colnames(gram) <- c("Text", "NumOccur")
        gram <- dplyr::arrange(gram, desc(NumOccur))
        
}
gram1 <- grams(corpus, 1)
gram2 <- grams(corpus, 2)
gram3 <- grams(corpus, 3)
gram4 <- grams(corpus, 4)



predictword <- function(input){
        strword <- strsplit(input,' ')[[1]]
        len <- length(strword)
        input <- removePunctuation(input)
        if (len==0){gramo(input)}
        if (len==1){gramt(input)}
        if (len==2){gramth(input)}
        if (len>=3){gramf(input)}
}


gramo <- function(input){
        i=0
        while (i < 5){
                i=i+1
                strword <- strsplit(as.String(gram1$Text[i])," ")
                numstr <- length(strword[[1]])
                print(strword[[1]][numstr])
        }
}


gramt <- function(input){
        input <- tolower(input)
        count <- length(strsplit(input," ")[[1]])
        if (count > 1) {
                input <- strsplit(input,' ')
                numinput <- length(input[[1]])
                input <- paste(input[[1]][numinput])
        }
        result <- grep(paste('^',input,sep=''),gram2$Text)
        if (sum(result)==0){
                gramo(input)
        }
        else{
                num <- length(result)
                i=0
                while (i < num & i <= 5){
                        i=i+1
                        index <- result[i]
                        strword <- strsplit(as.String(gram2$Text[index])," ")
                        numstr <- length(strword[[1]])
                        print(strword[[1]][numstr])
                }
        }
        
}


gramth <- function(input){
        input <- tolower(input)
        count <- length(strsplit(input," ")[[1]])
        if (count > 2) {
                input <- strsplit(input,' ')
                numinput <- length(input[[1]])
                input <- paste(input[[1]][numinput-1],input[[1]][numinput])
        }
        result <- grep(paste('^',input,sep=''),gram3$Text)
        if (sum(result)==0){
                input()gramt(input)
        }
        else{
                num <- length(result)
                i=0
                while (i < num & i <= 5){
                        i=i+1
                        index <- result[i]
                        strword <- strsplit(as.String(gram3$Text[index])," ")
                        numstr <- length(strword[[1]])
                        print(strword[[1]][numstr])
                }
        }
}


gramf <- function(input){
        input <- tolower(input)
        count <- length(strsplit(input," ")[[1]])
        if (count > 3) {
                input <- strsplit(input,' ')
                numinput <- length(input[[1]])
                input <- paste(input[[1]][numinput-2],input[[1]][numinput-1],input[[1]][numinput])
        }
        result <- grep(paste('^',input,sep=''),gram4$Text)
        if (sum(result)==0){
                gramth(input)
        }
        else{
                num <- length(result)
                i=0
                while (i < num & i <= 5){
                        i=i+1
                        index <- result[i]
                        strword <- strsplit(as.String(gram4$Text[index])," ")
                        numstr <- length(strword[[1]])
                        print(strword[[1]][numstr])
                }
        }
}

shinyServer(
        function(input,output){
                output$oresult <- renderPrint({predictword(input)})
        }
)



