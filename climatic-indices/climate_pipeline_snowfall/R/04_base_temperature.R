# Temperaturas mensuales en °C. Tmean_est NO es media de 24 horas:
# es la media mensual de (Tmax_diaria + Tmin_diaria) / 2.
base_temperature <- function(tx, tn, c) {
  valid_tx <- rowSums(!is.na(tx)); valid_tn <- rowSums(!is.na(tn))
  pair <- !is.na(tx) & !is.na(tn)
  tm <- (tx + tn) / 2
  mx <- rowMeans(tx, na.rm = TRUE); mn <- rowMeans(tn, na.rm = TRUE)
  av <- rowMeans(tm, na.rm = TRUE)
  required <- ceiling(ncol(tx) * c$min_valid_fraction)
  mx[valid_tx < required] <- NA_real_; mn[valid_tn < required] <- NA_real_
  av[rowSums(pair) < required] <- NA_real_
  data.frame(tmax_mean_c = mx, tmin_mean_c = mn, tmean_est_c = av,
             tmax_nvalid = as.integer(valid_tx), tmin_nvalid = as.integer(valid_tn),
             tpair_nvalid = as.integer(rowSums(pair)))
}
