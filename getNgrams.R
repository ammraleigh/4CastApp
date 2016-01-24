#setwd("~/Data Science/Coursera John Hopkins/10 Capstone/Project")

options( java.parameters = "-Xmx50g" )

library(data.table)
library(tm)
library(openNLP)
library(RWeka)
library("quanteda")  #Use sparse matrix document-feature matrix (dfm) to phase out dense matrix objects
library(dplyr)
library("slam")
library("methods")

source("~/Data Science/Coursera John Hopkins/10 Capstone/Project/shiny/prepareData.R")

#Windows
RDataPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/data/"
enDataPath <- file.path ( "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/Coursera-SwiftKey/final" , "en_US" )

#Linux
#RDataPath <- "/home/ec2-user/data/"
#enDataPath <- file.path ( "/home/ec2-user/Coursera-SwiftKey/final" , "en_US" )

load(paste(RDataPath, "enDictionary.RData", sep=""))

#read in all 3 english files: blogs, news and twitter
enCorpus <- Corpus(DirSource(enDataPath, encoding="UTF-8"), readerControl=list(language="eng", reader=readPlain))
lengths <- sapply(1:3,function(i) length(enCorpus[[i]]$content));lengths

#partition into training 80%) /test (20%) sets

enTest1 <- enCorpus[[1]]$content[ceiling(.8*length(enCorpus[[1]]$content))+1:length(enCorpus[[1]]$content)]
enTest2 <- enCorpus[[2]]$content[ceiling(.8*length(enCorpus[[2]]$content))+1:length(enCorpus[[2]]$content)]
enTest3 <- enCorpus[[3]]$content[ceiling(.8*length(enCorpus[[3]]$content))+1:length(enCorpus[[3]]$content)]
lengths <- c(length(enTest1), length(enTest2), length(enTest3));lengths
enTestAll <- c(enTest1, enTest2, enTest3)
save(enTestAll, file= paste(RDataPath, "enTest.RData", sep = "" ))
write.csv(enTestAll, file = paste(RDataPath, "enTest.txt", sep = ""))
rm(enTestAll); gc()

enTrain <- enCorpus
enTrain[[1]]$content <- enCorpus[[1]]$content[1:ceiling(.8*length(enCorpus[[1]]$content))]
enTrain[[2]]$content <- enCorpus[[2]]$content[1:ceiling(.8*length(enCorpus[[2]]$content))]
enTrain[[3]]$content <- enCorpus[[3]]$content[1:ceiling(.8*length(enCorpus[[3]]$content))]
lengths <- sapply(1:3,function(i) length(enTrain[[i]]$content));lengths
enTrainAll <- c(enTrain[[1]]$content, enTrain[[2]]$content, enTrain[[3]]$content)
save(enTrainAll, file= paste(RDataPath, "enTrain.RData", sep = "" ))
rm(enTrain); gc()

rm(enCorpus); gc()

corpus <- VCorpus(VectorSource(enTrainAll));
rm(enTrainAll); gc()

corpus <- prepareData(corpus)

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
unigram <- inner_join(unigram, enDictionary, by = "wUnigram")
save(unigram, file= paste(RDataPath, "unigram.RData", sep = "" ))
rm(unigram); gc()

bigram <- data.table()
n <- 10
k <- ceiling(length(corpus)/n)
for(i in 1:n){
  start <- (i-1)*k+1
  end <- i*k
  if(i==n){
    end <- length(corpus)
  }
  tempTokens <- dfm(corpus[start:end], ngrams = 2, concatenator = " ")
  tempTokens <- as.DocumentTermMatrix(tempTokens, weighting = weightTf )
  tempTokens <- col_sums(tempTokens > 0)
  tempTable <- data.table(wBigram=unlist(attributes(tempTokens)), bfreq=tempTokens)
  
  bigram <- rbind(bigram, tempTable)
  setkey(bigram, wBigram)
  bigram <- bigram[, sum(bfreq), by=wBigram]
  setnames(bigram, names(bigram), c("wBigram","bfreq"))
}
bigram <- subset(bigram,!bigram$wBigram == "") 
bigram <- subset(bigram,!is.na(bigram$wBigram))

bigram$wUnigramN <- sapply(bigram$wBigram, function(x) sub(x, pattern = "^[[:alpha:]]* ", replacement = ""))
setkey(bigram, wUnigramN); setkey(enDictionary, wUnigram)
bigram <- merge(bigram, enDictionary, by.x = "wUnigramN", by.y = "wUnigram")

bigram$wUnigramNminus1 <- sapply(bigram$wBigram, function(x) sub(x, pattern = " [[:alpha:]]*$", replacement = ""))
setkey(bigram, wUnigramNminus1); setkey(enDictionary, wUnigram)
bigram <- merge(bigram, enDictionary, by.x = "wUnigramNminus1", by.y = "wUnigram")

bigram$wUnigramN <- NULL
bigram$wUnigramNminus1 <- NULL

save(bigram, file= paste(RDataPath, "bigram.RData", sep=""))
rm(bigram); gc()

trigram <- data.table()
n <- 20
k <- ceiling(length(corpus)/n)
for(i in 1:n){
  start <- (i-1)*k+1
  end <- i*k
  if(i==n){
  }
  tempTokens <- dfm(corpus[start:end], ngrams = 3, concatenator = " ")
  tempTokens <- as.DocumentTermMatrix(tempTokens, weighting = weightTf )
  tempTokens <- col_sums(tempTokens > 0)
  tempTable <- data.table(wTrigram=unlist(attributes(tempTokens)), tfreq=tempTokens)
  
  trigram <- rbind(trigram, tempTable)
  setkey(trigram, wTrigram)
  trigram <- trigram[, sum(tfreq), by=wTrigram]
  setnames(trigram, names(trigram), c("wTrigram","tfreq"))
}
trigram <- subset(trigram,!trigram$wTrigram == "") 
trigram <- subset(trigram,!is.na(trigram$wTrigram)) 

trigram$wBigram <- sapply(trigram$wTrigram, function(x) sub(x, pattern = "^[[:alpha:]]* ", replacement = ""))
trigram$wUnigram <- sapply(trigram$wBigram, function(x) sub(x, pattern = " [[:alpha:]]*$", replacement = ""))
setkey(trigram, wUnigram); setkey(enDictionary, wUnigram)
trigram <- merge(trigram, enDictionary, by.x = "wUnigram", by.y = "wUnigram")

trigram$wUnigram <- NULL
trigram$wUnigram <- sapply(trigram$wBigram, function(x) sub(x, pattern = "^[[:alpha:]]* ", replacement = ""))
setkey(trigram, wUnigram); setkey(enDictionary, wUnigram)
trigram <- merge(trigram, enDictionary, by.x = "wUnigram", by.y = "wUnigram")

trigram$wUnigram <- NULL
trigram$wBigram <- sapply(trigram$wTrigram, function(x) sub(x, pattern = " [[:alpha:]]*$", replacement = ""))
trigram$wUnigram <- sapply(trigram$wBigram, function(x) sub(x, pattern = " [[:alpha:]]*$", replacement = ""))
setkey(trigram, wUnigram); setkey(enDictionary, wUnigram)
trigram <- merge(trigram, enDictionary, by.x = "wUnigram", by.y = "wUnigram")
trigram$wUnigram <- NULL
trigram$wBigram <- NULL

save(trigram, file= paste(RDataPath, "trigram.RData", sep=""))
rm(trigram); gc()

# quadgram <- data.table()
# n <- 25
# k <- ceiling(length(corpus)/n)
# for(i in 1:n){
#   start <- (i-1)*k+1
#   end <- i*k
#   if(i==n){
#     end <- length(corpus)
#   }
#   tempTokens <- dfm(corpus[start:end], ngrams = 4, concatenator = " ")
#   tempTokens <- as.DocumentTermMatrix(tempTokens, weighting = weightTf )
#   tempTokens <- col_sums(tempTokens > 0)
#   tempTable <- data.table(wQuadgram=unlist(attributes(tempTokens)), qfreq=tempTokens)
#   
#   quadgram <- rbind(quadgram, tempTable)
#   setkey(quadgram, wQuadgram)
#   quadgram <- quadgram[, sum(qfreq), by=wQuadgram]
#   setnames(quadgram, names(quadgram), c("wQuadgram","qfreq"))
# }
# quadgram <- subset(quadgram,!quadgram$wQuadgram == "") 
# quadgram <- subset(quadgram,!is.na(quadgram$wQuadgram)) 
# save(quadgram, file= paste(RDataPath, "quadgram.RData", sep = ""))
# rm(quadgram); gc()

rm(tempTokens, tempTable, corpus); gc()
