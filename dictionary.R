
#setwd("~/Data Science/Coursera John Hopkins/10 Capstone/Project")

library(data.table)
options( java.parameters = "-Xmx50g" )
library(tm)
library(openNLP)
library(RWeka)
library("quanteda")  #Use sparse matrix document-feature matrix (dfm) to phase out dense matrix objects
library("slam")
library("methods")

# Start with hunspell dictionary to validate all words

if (.Platform$OS.type == "windows"){
  dictionary_path <- file.path ( "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/dictionary", "en_US" )
  RDataPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/data/"
}

if (.Platform$OS.type == "unix") {
  dictionary_path <- file.path ( "/home/ec2-user/dictionary" , "en_US" )
}

corpus <- Corpus(DirSource(dictionary_path, encoding="UTF-8"), readerControl=list(language="eng", reader=readPlain))
lengths <- sapply(1:3,function(i) length(corpus[[i]]$content));lengths

corpus <- data.table(wUnigram=c(c(corpus[[1]]$content, corpus[[2]]$content, corpus[[3]]$content)))
corpus$wUnigram <- sapply(corpus, function(x) sub(x, pattern = "\\/[[:alnum:]]*$", replacement = ""))

corpus <- VCorpus(VectorSource(corpus));
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='')))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, letters)
corpus <- tm_map(corpus, removePunctuation)

enDictionary <- data.table(wUnigram=c(corpus[[1]]$content))
enDictionary <- enDictionary[-which(duplicated(enDictionary))]
enDictionary <- subset(enDictionary, !enDictionary$wUnigram=="")
enDictionary <- enDictionary <- subset(enDictionary,!is.na(enDictionary$wUnigram)) 

enDictionary$nchar <- nchar(enDictionary$wUnigram)


save(enDictionary, file= paste(RDataPath, "enDictionary.RData", sep=""))
write.csv(enDictionary, file = paste(RDataPath, "enDictionary.csv", sep = ""))

rm(corpus); gc()
