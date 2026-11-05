# FRD: número de días con Tmin < 0 °C; NO se convierte a Kelvin.
index_frd <- function(tn, c) {
  out <- rowSums(tn < c$frost_c, na.rm = TRUE)
  out[rowSums(!is.na(tn)) < ceiling(ncol(tn) * c$min_valid_fraction)] <- NA_real_
  out
}
