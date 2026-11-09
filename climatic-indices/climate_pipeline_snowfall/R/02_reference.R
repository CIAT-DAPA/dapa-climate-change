# HITO 02a: P95 de días húmedos POR CELDA Y MES (percentil tipo 7).
# Cambio explícito respecto al código antiguo: referencia mensual, no móvil por día.
reference_precipitation <- function(c, inv, tile, grid) {
  n <- tile$nrows * tile$ncols
  p95 <- matrix(NA_real_, n, 12L); nwet <- nvalid <- matrix(0L, n, 12L)
  for (m in 1:12) {
    files <- inv$path[inv$year %in% c$reference_years & inv$month == m]
    # Abrir como máximo un mes (~31 GeoTIFF) por lectura, no 1 271
    # archivos simultáneos en una referencia de 41 años.
    x <- do.call(cbind, lapply(c$reference_years, function(y)
      read_tile(source_month(inv, y, m), tile, grid, 'prec')))
    nvalid[, m] <- rowSums(!is.na(x))
    x[x < c$wet_mm] <- NA_real_
    nwet[, m] <- rowSums(!is.na(x))
    q <- row_q(x, 0.95)[, 1]
    q[nwet[, m] < c$min_wet_reference_days |
      nvalid[, m] < ceiling(length(files) * c$min_reference_fraction)] <- NA_real_
    p95[, m] <- q
  }
  list(cell_id = cell_ids(grid, tile), p95 = p95, nwet = nwet, nvalid = nvalid)
}
# HITO 02b: climatología diaria Tmax, 366 posiciones de calendario.
# Cada día medio se estima entre años; luego ventana circular de 5 días.
reference_temperature <- function(c, inv, tile, grid) {
  n <- tile$nrows * tile$ncols
  sums <- matrix(0, n, 366L); counts <- matrix(0L, n, 366L)
  expected <- integer(366L)
  for (y in c$reference_years) for (m in 1:12) {
    paths <- source_month(inv, y, m); days <- calendar_day(month_dates(y, m))
    x <- read_tile(paths, tile, grid, 'tmax'); ok <- !is.na(x)
    x[!ok] <- 0
    sums[, days] <- sums[, days, drop = FALSE] + x
    counts[, days] <- counts[, days, drop = FALSE] + ok
    expected[days] <- expected[days] + 1L
  }
  daily <- sums / counts
  daily[counts < rep(ceiling(expected * c$min_reference_fraction), each = n)] <- NA_real_
  daily[!is.finite(daily)] <- NA_real_
  smooth <- matrix(NA_real_, n, 366L)
  half <- (c$climatology_window - 1L) %/% 2L
  for (d in 1:366) {
    ix <- ((d + seq.int(-half, half) - 1L) %% 366L) + 1L
    smooth[, d] <- rowMeans(daily[, ix, drop = FALSE], na.rm = FALSE)
  }
  list(cell_id = cell_ids(grid, tile), tmax_climatology = smooth, nvalid = counts)
}
stage_reference <- function(c) {
  inv <- load_inventory(c)
  sources <- if (is.null(c$reference_sources)) c('prec', 'tmax') else c$reference_sources
  for (v in sources) {
    grid <- terra::rast(inv[[v]]$path[1]); tiles <- make_tiles(grid, c)
    for (k in seq_len(nrow(tiles))) {
      tile <- tiles[k, ]
      checkpoint(c, paste0('reference_', v, '_', tile$tile_id), function() {
        x <- if (v == 'prec') reference_precipitation(c, inv[[v]], tile, grid)
             else reference_temperature(c, inv[[v]], tile, grid)
        # Compresión aquí: los océanos y los conteos repetidos comprimen mucho.
        dest <- ref_path(c, v, tile$tile_id); ensure_dir(dirname(dest))
        tmp <- paste0(dest, '.partial'); saveRDS(x, tmp, compress = TRUE)
        publish_file(tmp, dest)
      })
    }
  }
}
