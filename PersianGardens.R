library(NLP)
library(tm)
library(xlsx)
library(RColorBrewer)
library(wordcloud)

rm(list=ls())

#functions
clean.corpus <- function(txt){
  text_corpus <- tm_map(text_corpus, stemDocument)
  text_corpus <- tm_map(text_corpus, removeWords, c(stopwords("english"), stopwords("SMART"))) 
  text_corpus <- tm_map(text_corpus, removePunctuation)
  text_corpus <- tm_map(text_corpus, removeNumbers)
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  assign('text_corpus',text_corpus,envir=.GlobalEnv)
}

word.frequency <- function (TDM){
  temp <- inspect(TDM)
  word.frequency <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
  row.names(word.frequency) <- NULL
  word.frequency <- word.frequency[order(-word.frequency$Freq),]
  assign('word.frequency',word.frequency,envir=.GlobalEnv)
}

data.dir<-"GitHub/Persian-Gardens"


#read the text file
setwd(data.dir)
fileName = "Donald Wilber Report.txt"
conn <- file(fileName,open="r")
text <-readLines(conn)
close(conn)

#creat a cropus
setwd("./Persian-Gardens")
text_corpus <- Corpus(VectorSource(text), readerControl = list(language = "en"))

#clean up the corpus
clean.corpus(text_corpus)

#(TDM) which reflects the number of times each word in the corpus is found in each of the documents
TDM <- TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,stopwords = TRUE))
inspect(TDM)
word.frequency(TDM)
write.xlsx(word.frequency, "./WordFrequency.xlsx")

#Creat the wordckoud
wordcloud(word.frequency$ST, word.frequency$Freq,max.words=1000)


