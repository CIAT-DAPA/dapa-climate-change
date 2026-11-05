# P95: % de DÍAS HÚMEDOS por encima del P95 mensual histórico.
# No confundir con R95pTOT (fracción de lluvia acumulada).
index_p95 <- function(x, threshold, c) {
  wet <- x >= c$wet_mm
  nwet <- rowSums(wet, na.rm = TRUE)
  nexceed <- rowSums(wet & x > threshold, na.rm = TRUE)
  out <- 100 * nexceed / nwet
  out[nwet == 0L | is.na(threshold) |
      rowSums(!is.na(x)) < ceiling(ncol(x) * c$min_valid_fraction)] <- NA_real_
  data.frame(p95_wet_days_pct = out, wet_days = as.integer(nwet))
}
