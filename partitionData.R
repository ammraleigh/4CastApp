#setwd("~/Data Science/Coursera John Hopkins/10 Capstone/Project")

partitionData <- function(ngrams) {
  
  for (i in letters) {
    theLetter <- toupper(i)
    if (!exists("ngramName")) {ngramName <- deparse(substitute(ngrams))}
    theName <- paste0(ngramName,theLetter)
    theSearch <- paste0("^",i)
    theFile <- paste0(theName, ".RData")
    assign(theName, ngrams[grep(theSearch, ngrams[[colnames(ngrams)[1], exact = FALSE]]),])
    save(list=c(theName), file=paste0(shinyPath,theFile))
    rm(theName)
  }
  
}

RDataPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/data/"
shinyPath <- "C:/Users/ann/Documents/Data Science/Coursera John Hopkins/10 Capstone/Project/shiny/"

load(paste0(RDataPath,"shinyBigram.RData"))
load(paste0(RDataPath,"shinyTrigram.RData"))

partitionData(bigram)
partitionData(trigram)


#shinyPath <- "~/data/"
#theFiles <- list.files(shinyPath)

# theFiles <- list.files(shinyPath)
# for (i in 1:length(theFiles)) {
#   load(paste0(shinyPath,theFiles[i]))
# }
