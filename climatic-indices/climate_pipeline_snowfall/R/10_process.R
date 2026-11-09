# Orquestación fusionada: cada bloque diario se lee una vez por fase.
# Las funciones de bases e índices permanecen separadas para revisión/reuso.
stage_precipitation <- function(c) {
  inv <- load_inventory(c)$prec; grid <- terra::rast(inv$path[1])
  tiles <- make_tiles(grid, c)
  for (k in seq_len(nrow(tiles))) {
    tile <- tiles[k, ]; ref <- readRDS(ref_path(c, 'prec', tile$tile_id))
    ids <- cell_ids(grid, tile)
    for (y in c$years) checkpoint(c, paste0('prec_', tile$tile_id, '_', y), function() {
      out <- vector('list', 12L)
      for (m in 1:12) {
        x <- read_tile(source_month(inv, y, m), tile, grid, 'prec')
        base <- base_precipitation(x, c)
        idx <- data.frame(cdd_days = index_cdd(x, c), drd_days = index_drd(x, c))
        idx <- cbind(idx, index_p95(x, ref$p95[, m], c))
        out[[m]] <- cbind(data.frame(grid_id = 'prec', cell_id = ids,
                                     year = y, month = m), base, idx)
        out[[m]] <- out[[m]][base$prec_nvalid > 0L, , drop = FALSE]
      }
      write_table(do.call(rbind, out), table_path(c, 'precipitation', tile$tile_id, y), c)
    })
  }
}
stage_temperature <- function(c) {
  inv <- load_inventory(c); grid <- terra::rast(inv$tmax$path[1])
  tiles <- make_tiles(grid, c)
  dates <- inv$tmax$date
  for (k in seq_len(nrow(tiles))) {
    tile <- tiles[k, ]
    checkpoint(c, paste0('temperature_', tile$tile_id), function() {
      ref <- readRDS(ref_path(c, 'tmax', tile$tile_id))
      ids <- cell_ids(grid, tile); s <- heat_state(length(ids), dates)
      offset <- 0L
      for (y in c$years) {
        out <- vector('list', 12L)
        for (m in 1:12) {
          tx <- read_tile(source_month(inv$tmax, y, m), tile, grid, 'tmax')
          tn <- read_tile(source_month(inv$tmin, y, m), tile, grid, 'tmin')
          if (any(tn > tx, na.rm = TRUE)) stop('Tmin > Tmax en ', y, '/', m)
          base <- base_temperature(tx, tn, c)
          out[[m]] <- cbind(data.frame(grid_id = 'temperature', cell_id = ids,
                                       year = y, month = m), base,
                            frd_days = index_frd(tn, c))
          out[[m]] <- out[[m]][base$tmax_nvalid > 0 | base$tmin_nvalid > 0, , drop = FALSE]
          dd <- month_dates(y, m)
          thresholds <- ref$tmax_climatology[, calendar_day(dd), drop = FALSE] + c$heat_delta_c
          hot <- tx > thresholds
          valid <- !is.na(tx) & !is.na(thresholds)
          for (j in seq_along(dd)) heat_update(s, hot[, j], valid[, j], offset + j, c)
          offset <- offset + length(dd)
        }
        write_table(do.call(rbind, out), table_path(c, 'temperature', tile$tile_id, y), c)
      }
      heat_finish(s, c)
      for (y in c$years) {
        out <- lapply(1:12, function(m) {
          j <- match(y * 100L + m, s$keys)
          d <- data.frame(grid_id = 'temperature', cell_id = ids, year = y, month = m,
                          hwd_days = s$days[, j], hwd_events = s$events[, j],
                          hwd_longest_days = s$longest[, j], hwd_nvalid = s$valid[, j])
          d[d$hwd_nvalid > 0L, , drop = FALSE]
        })
        write_table(do.call(rbind, out), table_path(c, 'hwd', tile$tile_id, y), c)
      }
    })
  }
}
