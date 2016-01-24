#setwd("~/Data Science/Coursera John Hopkins/10 Capstone/Project")

trainNLPModel <- function () {
  
  library("data.table")
  
  if (.Platform$OS.type == "windows"){
    RDataPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/data/"
    shinyPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/shiny/data"
  }
  
  if (.Platform$OS.type == "unix") {
    RDataPath <- "/home/ec2-user/data/"
    shinyPath <- "/home/ec2-user/shiny/"
  }

  load(paste(RDataPath,"unigram.RData", sep=""))
  unigram$uMLE <- unigram$ufreq/sum(unigram$ufreq)
  unigram[unigram$wUnigram=="i"]$wUnigram <- "I"
  setkey(unigram, wUnigram)
  
  load(paste(RDataPath,"bigram.RData", sep=""))
  bigram$wUnigram <- sapply(bigram$wBigram, function(x) sub(x, pattern = " [[:alpha:]]*$", replacement = ""))
  setkey(bigram, wBigram, wUnigram)
  bigram <- merge(bigram, unigram, all=TRUE)
  setkey(bigram, wBigram, wUnigram)
  
  unigram <- unigram[order(-uMLE)]
  unigram <- subset(unigram, ufreq>1)
  unigram$ufreq <- NULL
  # to preserve some memory only use the first 100 unigrams with the app
  if (nrow(unigram) > 100) {unigram <- unigram[1:100]}
  save(list=c("unigram"), file=paste(shinyPath,"shinyUnigram.RData", sep = ""))
  rm(unigram); gc()
  
  bigram <- subset(bigram, !is.na(ufreq)); bigram <- subset(bigram, !is.na(bfreq)); bigram$uMLE <- NULL
  bigram$bMLE <- bigram$bfreq/bigram$ufreq
  bigram$ufreq <- NULL
  
  load(paste(RDataPath,"trigram.RData", sep=""))
  trigram$wBigram <- sapply(trigram$wTrigram, function(x) sub(x, pattern = " [[:alpha:]]*$", replacement = ""))
  setkey(trigram, wTrigram, wBigram)
  trigram <- merge(trigram, bigram, all=TRUE)
  
  #bigram <-  subset(bigram,  bfreq>1)   #this is wrong, we want to keep the unique recommendations!
  #bigram <- sapply(bigram, function(x){head(bigram[grep(paste0("^", bigram$wUnigram, "$"), wUnigram),][order(wUnigram,-bMLE)],10)})
  bigram <- bigram[order(wUnigram, -bMLE) , .SD[bMLE %in% head(bMLE, 5)], by=wUnigram] #problem with ties showing up
  bigram$bfreq <- NULL
  bigram$wUnigram <- NULL
  save(list=c("bigram"), file=paste(shinyPath,"shinyBigram.RData", sep = ""))
  rm(bigram); gc()

  trigram$wUnigram <- NULL; trigram$bMLE <- NULL
  trigram <- subset(trigram, !is.na(bfreq)); trigram <- subset(trigram, !is.na(tfreq))
  trigram$tMLE <- trigram$tfreq/trigram$bfreq
  
  trigram$bfreq <- NULL
  trigram$tfreq <- NULL
  #trigram <- subset(trigram, tfreq>1)
  trigram <- trigram[order(wBigram, -tMLE) , .SD[tMLE %in% head(tMLE, 5)], by=wBigram] #problem with ties showing up
  trigram$wBigram <- NULL
  save(list=c("trigram"), file=paste(shinyPath,"shinyTrigram.RData", sep = ""))
  rm(trigram); gc()
  
}

trainNLPModel()