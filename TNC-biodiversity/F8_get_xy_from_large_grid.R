get.values.lg <- function(ascii, where) {
  con <- file(ascii, "rt")

  count <- 1
  where$e.value <- NA

  repeat {
    line    <- scan(con, what=character(0), nlines=1)
    if (!length(line)) break
    if (count %in% keys) where[where$row==count,'e.value'] <- line[l[[count]]] 
    count <- count +1
  }
  write.csv(where, paste("values", ascii, ".csv", sep=""), row.names=F, quote=F)
  close(con)
}
