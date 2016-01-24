
getPredictions <- function(input, numWord) {
  
  if (input == " ") {return()}                                       #still typing
  if (!substr(input, nchar(input), nchar(input)) == " ") {return()}  #still typing
  
  input <- prepareData(input); if (input =="") {return()}
  spltOutput <- strsplit(input, split = " ")
  n = length(spltOutput[[1]]); if (n <= 0) {return()}
  
  predictedWord3 <- vector(); predictedWord2 <- vector()
  
  if (n+1 >= 3) {
    dbName <- paste0("trigram", toupper(substr(spltOutput[[1]][n-1], 1, 1)))
    trigramDB <- get(dbName)
    if (!exists(dbName)) {return()}
    indexTrigrams <- grepl(paste0("^", paste(spltOutput[[1]][n-1],spltOutput[[1]][n]), " "), trigramDB$wTrigram)
    if (sum(indexTrigrams) > 0) {
      predictedWord3 <- trigramDB[indexTrigrams,][order(-tMLE)][1:numWord,c(wTrigram)]     
    }
  }
  else if (n+1 == 2) {
    dbName <- paste0("bigram", toupper(substr(spltOutput[[1]][n], 1, 1)))
    bigramDB <- get(dbName)
    if (!exists(dbName)) {return()}
    indexBigrams <- grepl(paste0("^", spltOutput[[1]][n], " "), bigramDB$wBigram)
    if (sum(indexBigrams)) {
      predictedWord2 <- bigramDB[indexBigrams,][order(-bMLE)][1:numWord,c(wBigram)]
    }
  }

  # stupid (greedy) backoff model for the prediction algorithm
  if (length(predictedWord3) <= 0) {
    if (length(predictedWord2) <= 0) {
      return (unlist(strsplit(unigram[1:numWord,c(wUnigram)], split = " "))[c(1,2,3,4)])
    }
    else {
      return (unlist(strsplit(predictedWord2[1:numWord], split = " "))[c(2,4,6,8)])
    }
  }
  else {
    return (unlist(strsplit(predictedWord3[1:numWord], split = " "))[c(3,6,9,12)])
  }
}
