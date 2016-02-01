#install the packages you need to mine text
Needed <- c("NLP", "tm", "RColorBrewer", "wordcloud", "SnowballC", "xlsx", "cluster")   
install.packages(Needed, dependencies=TRUE)

library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(xlsx)
library(cluster)   

rm(list=ls())

#functions
clean.corpus <- function(txt){
  text_corpus <- tm_map(text_corpus, stemDocument)
  text_corpus <- tm_map(text_corpus, removeWords, c(stopwords("english"), stopwords("SMART"))) 
  text_corpus <- tm_map(text_corpus, removePunctuation)
  #text_corpus <- tm_map(text_corpus, tolower)
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  text_corpus <- tm_map(text_corpus, PlainTextDocument) 
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

#Combining words that should stay together
for (j in seq(text)) {
  text[[j]] <- gsub("Allen W. Dulles", "Allen_W_Dulles", text[[j]])
  text[[j]] <- gsub("British secret intelligence service ", "SIS", text[[j]])
  text[[j]] <- gsub("British Foreign Office", "BFO", text[[j]])
  text[[j]] <- gsub("Department of State", "DepOfState", text[[j]])
  text[[j]] <- gsub("Director of CIA", "DoCIA", text[[j]])
  text[[j]] <- gsub("Foreign Office", "Foreign_Office", text[[j]])
  text[[j]] <- gsub("General Fazllolah Zahedi", "Gen_Zahedi", text[[j]])
  text[[j]] <- gsub("Kermit Roosevelt", "Kermit_Roosevelt", text[[j]])
  text[[j]] <- gsub("General Fazllolah Zahedi", "Gen_Zahedi", text[[j]])
  text[[j]] <- gsub("Loy Wesley Henderson", "Loy_Henderson", text[[j]])
  text[[j]] <- gsub("military secretariat", "military_secretariat", text[[j]])
  text[[j]] <- gsub("Norman Schwarzkopf", "Norman_Schwarzkopf", text[[j]])
  text[[j]] <- gsub("President of the United States", "President_US", text[[j]])
  text[[j]] <- gsub("Princess Ashraf Pahlavi", "Ashraf_Pahlavi", text[[j]])
  text[[j]] <- gsub("Roger Goiran", "Roger_Goiran", text[[j]])
  text[[j]] <- gsub("Secretary of State", "Secretary_of_State", text[[j]])
  text[[j]] <- gsub("Tehran military", "Tehran_military", text[[j]])
}

#creat a cropus
text_corpus <- Corpus(VectorSource(text), readerControl = list(language = "en"))

#clean up the corpus
clean.corpus(text_corpus)

#(TDM) which reflects the number of times each word in the corpus is found in each of the documents
TDM <- TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,stopwords = TRUE))
inspect(TDM)
word.frequency(TDM)
write.xlsx(word.frequency, "./WordFrequency.xlsx")

#Creat the wordckoud
wordcloud(word.frequency$ST, word.frequency$Freq,max.words=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

"Clustering 
https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#term-correlations"

