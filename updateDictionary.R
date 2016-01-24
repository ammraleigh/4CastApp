#setwd("~/Data Science/Coursera John Hopkins/10 Capstone/Project")

library(data.table)
options( java.parameters = "-Xmx50g" )
library(tm)
#library(qdap)
library(openNLP)
library(RWeka)
library("quanteda")  #Use sparse matrix document-feature matrix (dfm) to phase out dense matrix objects
library(dplyr)
library("slam")
library("methods")

source("C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/shiny/prepareData.R")

RDataPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/data/"
enDataPath <- file.path ( "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/Coursera-SwiftKey/final" , "en_US" )

load(paste(RDataPath, "enDictionary.RData", sep=""))

enCorpus <- Corpus(DirSource(enDataPath, encoding="UTF-8"), readerControl=list(language="eng", reader=readPlain))

enCorpus[[1]]$content <- enCorpus[[1]]$content[1:ceiling(.1*length(enCorpus[[1]]$content))]

enTrain <- c(enCorpus[[1]]$content)

rm(enCorpus); gc()

corpus <- VCorpus(VectorSource(enTrain));
rm(enTrain); gc()

corpus <- prepareData(corpus)
corpus <- tm_map(corpus, removeWords, letters)

unigram <- data.table()
n <- 5
k <- ceiling(length(corpus)/n)
for(i in 1:n){
  start <- (i-1)*k+1
  end <- i*k
  if(i==n) {
    end <- length(corpus)
  }
  tempTokens <- dfm(corpus[start:end], ngrams = 1)
  tempTokens <- as.DocumentTermMatrix(tempTokens, weighting = weightTf )
  tempTokens <- col_sums(tempTokens > 0)
  tempTable <- data.table(wUnigram=unlist(attributes(tempTokens)), ufreq=tempTokens)

  unigram <- rbind(unigram, tempTable)
  setkey(unigram, wUnigram)
  unigram <- unigram[, sum(ufreq), by=wUnigram]
  setnames(unigram, names(unigram), c("wUnigram","ufreq"))
}

unigram <- subset(unigram,!unigram$wUnigram=="") 
unigram <- subset(unigram,!is.na(unigram$wUnigram)) 

test <- anti_join(unigram, enDictionary, by = "wUnigram")
write.csv(test, file = paste(RDataPath, "wordsNotEnDictionary.csv", sep = ""))

rm(unigram); gc()
