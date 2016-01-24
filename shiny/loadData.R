
loadData <- function () {
  theFiles <- list.files(pattern=".RData")
  fileCount <- length(theFiles)
  for (i in 1:fileCount) {
    load(theFiles[i], envir = parent.frame())
  }
  return (TRUE)
}