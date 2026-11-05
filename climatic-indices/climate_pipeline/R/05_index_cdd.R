# CDD: máxima racha seca dentro de cada mes. NA corta una racha.
index_cdd <- function(x, c) {
  run <- best <- numeric(nrow(x))
  for (j in seq_len(ncol(x))) {
    dry <- !is.na(x[, j]) & x[, j] < c$wet_mm
    run <- ifelse(dry, run + 1, 0); best <- pmax(best, run)
  }
  best[rowSums(!is.na(x)) < ceiling(ncol(x) * c$min_valid_fraction)] <- NA_real_
  best
}
