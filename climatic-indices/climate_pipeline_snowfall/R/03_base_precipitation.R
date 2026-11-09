# Bases mensuales: suma de precipitación diaria, mm/mes.
# Conserva nvalid incluso cuando el mes no supera el control de cobertura.
base_precipitation <- function(x, c) {
  nvalid <- rowSums(!is.na(x)); total <- rowSums(x, na.rm = TRUE)
  total[nvalid < ceiling(ncol(x) * c$min_valid_fraction)] <- NA_real_
  data.frame(prec_total_mm = total, prec_nvalid = as.integer(nvalid))
}
