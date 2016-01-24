
library("tm")

prepareData <- function (rawData) {
  
    corpus <- Corpus(VectorSource(rawData))
    corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte"))) 
    corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='')))
    #profanity is removed by running the unigrams agains a standardized dictionary (hunspell) which does not include profanity
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, content_transformer(function(x, pattern) gsub(pattern, " ", x)), "/|@|\\|~|_|\\*|#|%|\\^|&|<|>")
    corpus <- tm_map(corpus, removePunctuation)   #preserve_intra_word_dashes = TRUE
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, PlainTextDocument)
    corpus <- as.vector(unlist(sapply(corpus, '[',"content")))
    corpus
  
  }
