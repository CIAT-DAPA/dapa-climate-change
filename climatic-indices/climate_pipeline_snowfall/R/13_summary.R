read_enso <- function(c) {
  if (!nzchar(c$enso_csv)) return(NULL)
  if (!file.exists(c$enso_csv)) stop('Falta CSV ENOS. Configure enso_csv o use "" para omitirlo.')
  d <- read.csv(c$enso_csv)
  if (!all(c('year', 'month', 'oni') %in% names(d))) stop('CSV ENOS requiere year,month,oni.')
  if (anyDuplicated(d[c('year', 'month')]) || any(!d$month %in% 1:12) || anyNA(d$oni))
    stop('CSV ENOS con duplicados/valores inválidos.')
  expected <- expand.grid(year = c$years, month = 1:12)
  if (!all(paste(expected$year, expected$month) %in% paste(d$year, d$month)))
    stop('Faltan condiciones ENOS para meses del periodo.')
  d$enso <- ifelse(d$oni >= 0.5, 'elnino', ifelse(d$oni <= -0.5, 'lanina', 'normal'))
  d
}
# Todos los umbrales son TEMPORALES por celda y mes, nunca percentiles
# espaciales del bloque/globales. Referencia común para periodos y ENOS.
decile_thresholds <- function(mat, years, c) {
  ref <- mat[, years %in% c$reference_years, drop = FALSE]
  q <- row_q(ref, c$class_probs)
  nref <- rowSums(!is.na(ref))
  q[nref < ceiling(length(c$reference_years) * c$min_reference_fraction), ] <- NA_real_
  colnames(q) <- paste0('p', round(100 * c$class_probs))
  list(q = q, nref = nref)
}
annual_classes <- function(mat, q) {
  out <- matrix(NA_integer_, nrow(mat), ncol(mat))
  for (j in seq_len(ncol(mat))) out[, j] <- class_values(mat[, j], q)
  out
}
class_frequencies <- function(classes) {
  n <- rowSums(!is.na(classes))
  counts <- vapply(1:10, function(k) rowSums(classes == k, na.rm = TRUE), numeric(nrow(classes)))
  # rowSums devuelve doubles: implementación independiente del tipo de almacenamiento.
  counts <- matrix(counts, nrow = nrow(classes), ncol = 10L)
  pct <- 100 * counts / n
  pct[n == 0L, ] <- NA_real_
  colnames(counts) <- paste0('class_', 1:10, '_n')
  colnames(pct) <- paste0('class_', 1:10, '_pct')
  data.frame(n_classified = n, counts, pct, check.names = FALSE)
}
stage_summary <- function(c) {
  enso <- read_enso(c); inv <- load_inventory(c)
  metrics <- list(
    precipitation = c('prec_total_mm', 'cdd_days', 'drd_days', 'p95_wet_days_pct'),
    temperature = c('tmax_mean_c', 'tmin_mean_c', 'tmean_est_c', 'frd_days'),
    hwd = c('hwd_days', 'hwd_events', 'hwd_longest_days'))
  if (c$flood_enabled) metrics$fld <- 'fld_score'
  if (!is.null(c$summary_sources)) metrics <- metrics[c$summary_sources]
  for (stage in names(metrics)) {
    grid_id <- if (stage %in% c('temperature', 'hwd')) 'temperature' else 'prec'
    grid <- terra::rast(if (grid_id == 'temperature') inv$tmax$path[1] else inv$prec$path[1])
    tiles <- make_tiles(grid, c)
    for (k in seq_len(nrow(tiles))) for (m in 1:12) {
      tile <- tiles[k, ]
      checkpoint(c, paste0('summary_', stage, '_', tile$tile_id, '_m', m), function() {
        annual <- lapply(c$years, function(y) {
          d <- read_table(table_path(c, stage, tile$tile_id, y), c)
          d[d$month == m, c('cell_id', metrics[[stage]]), drop = FALSE]
        })
        ids <- sort(unique(unlist(lapply(annual, function(d) d$cell_id))))
        if (!length(ids)) return(invisible(NULL))
        result <- thresholds <- list(); a <- b <- 0L
        label <- paste0(tile$tile_id, '_m', sprintf('%02d', m))
        for (metric in metrics[[stage]]) {
          mat <- vapply(annual, function(d) d[[metric]][match(ids, d$cell_id)], numeric(length(ids)))
          mat <- matrix(mat, nrow = length(ids), ncol = length(c$years))
          reference <- decile_thresholds(mat, c$years, c)
          q <- reference$q
          classes <- annual_classes(mat, q)
          b <- b + 1L
          thresholds[[b]] <- cbind(data.frame(grid_id = grid_id, cell_id = ids,
            month = m, metric = metric, n_reference = reference$nref), as.data.frame(q))
          # Tabla ANUAL independiente; mismos límites para todos los años.
          d <- data.frame(grid_id = grid_id, cell_id = rep(ids, length(c$years)),
            year = rep(c$years, each = length(ids)), month = m, metric = metric,
            value = as.vector(mat), magnitude = as.vector(classes))
          write_table(d, table_path(c, paste0('classification/', stage, '/', metric), label), c)
          for (period in names(c$periods)) for (condition in c('all', if (!is.null(enso)) c('elnino', 'lanina', 'normal'))) {
            ys <- intersect(c$periods[[period]], c$years)
            if (condition != 'all') ys <- intersect(ys, enso$year[enso$month == m & enso$enso == condition])
            ix <- which(c$years %in% ys)
            z <- mat[, ix, drop = FALSE]
            n <- rowSums(!is.na(z)); value <- rowMeans(z, na.rm = TRUE)
            required <- max(c$min_enso_years, ceiling(length(ix) * c$min_reference_fraction))
            value[n < required | !is.finite(value)] <- NA_real_
            # Incluye FLD: sin redondear su score ni excluirlo del método decílico.
            magnitude <- class_values(value, q)
            freq <- class_frequencies(classes[, ix, drop = FALSE])
            # Conteos disponibles para auditoría; porcentajes solo con muestra suficiente.
            bad <- freq$n_classified < required
            freq[bad, paste0('class_', 1:10, '_pct')] <- NA_real_
            a <- a + 1L
            result[[a]] <- cbind(data.frame(grid_id = grid_id, cell_id = ids,
              month = m, metric = metric, period = period, enso = condition,
              n_years = n, value = value, magnitude = magnitude), freq)
          }
        }
        write_table(do.call(rbind, result), table_path(c, paste0('summary/', stage), label), c)
        write_table(do.call(rbind, thresholds), table_path(c, paste0('thresholds/', stage), label), c)
      })
    }
  }
}
