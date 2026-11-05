# FLD: índice compuesto 1..5, no probabilidad ni lámina de inundación.
class_values <- function(value, thresholds) {
  # Empates hacia la clase inferior; umbrales repetidos pueden omitir clases.
  ans <- 1L + rowSums(value > thresholds)
  ans[is.na(value) | rowSums(is.na(thresholds)) > 0L] <- NA_integer_
  as.integer(ans)
}
index_fld <- function(prec_class, static) {
  # No renormalizar pesos si falta una capa.
  0.40 * prec_class + 0.15 * static$clay_class + 0.15 * static$slope_class +
    0.15 * static$wetness_class + 0.15 * static$landcover_class
}
stage_flood <- function(c) {
  if (!c$flood_enabled) return(invisible(NULL))
  inv <- load_inventory(c); grid <- terra::rast(inv$prec$path[1]); tiles <- make_tiles(grid, c)
  for (k in seq_len(nrow(tiles))) {
    tile <- tiles[k, ]
    checkpoint(c, paste0('fld_', tile$tile_id), function() {
      st <- read_table(table_path(c, 'static', tile$tile_id), c)
      annual <- lapply(c$years, function(y)
        read_table(table_path(c, 'precipitation', tile$tile_id, y), c))
      ids <- sort(unique(unlist(lapply(annual, function(d) d$cell_id))))
      if (!length(ids)) {
        empty <- data.frame(grid_id = character(), cell_id = integer(), year = integer(),
                            month = integer(), fld_score = numeric())
        for (y in c$years) write_table(empty, table_path(c, 'fld', tile$tile_id, y), c)
        return(invisible(NULL))
      }
      st <- st[match(ids, st$cell_id), , drop = FALSE]
      output <- lapply(c$years, function(y) vector('list', 12L))
      thresholds_out <- vector('list', 12L)
      for (m in 1:12) {
        values <- vapply(annual, function(d) {
          d <- d[d$month == m, ]; d$prec_total_mm[match(ids, d$cell_id)]
        }, numeric(length(ids)))
        values <- matrix(values, nrow = length(ids), ncol = length(c$years))
        reference <- values[, c$years %in% c$reference_years, drop = FALSE]
        q <- row_q(reference, c$flood_rain_probs)
        q[rowSums(!is.na(reference)) < ceiling(length(c$reference_years) * c$min_reference_fraction), ] <- NA_real_
        thresholds_out[[m]] <- data.frame(cell_id = ids, month = m, q1 = q[, 1],
          q2 = q[, 2], q3 = q[, 3], q4 = q[, 4])
        for (j in seq_along(c$years)) {
          cls <- class_values(values[, j], q)
          output[[j]][[m]] <- data.frame(grid_id = 'prec', cell_id = ids,
            year = c$years[j], month = m, fld_score = index_fld(cls, st))
        }
      }
      write_table(do.call(rbind, thresholds_out), table_path(c, 'fld_thresholds', tile$tile_id), c)
      for (j in seq_along(c$years))
        write_table(do.call(rbind, output[[j]]), table_path(c, 'fld', tile$tile_id, c$years[j]), c)
    })
  }
}
