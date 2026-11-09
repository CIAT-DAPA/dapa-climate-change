# DRD: total de días secos, usando el número REAL de días del mes.
index_drd <- function(x, c) {
  out <- rowSums(x < c$wet_mm, na.rm = TRUE)
  out[rowSums(!is.na(x)) < ceiling(ncol(x) * c$min_valid_fraction)] <- NA_real_
  out
}
