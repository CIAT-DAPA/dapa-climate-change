# Extract AUC and threshold values

library(stringr)

ll        <- unlist(sapply(list.files(pattern="part.*",full=T), list.files, full=T))

  ll.v      <-ldply(ll,get.values)

get.values <- function(path) {

  r       <- list()
  f       <- readLines(str_c(path, "/info.txt"))
  r$id    <- as.numeric(str_split(path, "/")[[1]][3])
  r$part  <- ifelse(any(grepl("^Training.AUC:",f)),as.numeric(str_split(str_split(path, "/")[[1]][2],"\\.")[[1]][2]),NA)
  r$auc   <- ifelse(any(grepl("^Training.AUC:",f)),as.numeric(str_split(f[grep("^Training.AUC:",f)],":")[[1]][2]),NA)
  r$ts    <- ifelse(any(grepl("X.Training.samples:",f)),as.numeric(str_split(f[grep("X.Training.samples",f)],":")[[1]][2]),NA)
  r$tp    <- ifelse(any(grepl("X10.percentile.training.presence.logistic.threshold:",f)),as.numeric(str_split(f[grep("X10.percentile.training.presence.logistic.threshold:",f)],":")[[1]][2]),NA)
  r$ss    <- ifelse(any(grepl("Equal.training.sensitivity.and.specificity.logistic.threshold:",f)),as.numeric(str_split(f[grep("Equal.training.sensitivity.and.specificity.logistic.threshold:",f)],":")[[1]][2]),NA)
  r$ee    <- ifelse(any(grepl("Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold:",f)),as.numeric(str_split(f[grep("Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold:",f)],":")[[1]][2]),NA)

  return(unlist(r))
}




