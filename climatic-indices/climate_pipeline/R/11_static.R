# HITO 04: cuatro capas estáticas alineadas a CHIRPS UNA sola vez.
# Se permiten estos rásteres intermedios: evita remuestrear datos estáticos
# repetidamente. slope ya debe contener pendiente en porcentaje.
class_clay <- function(x) as.numeric(cut(x, c(-Inf, 20, 40, 60, 80, Inf), labels = 1:5))
class_slope <- function(x) as.numeric(cut(x, c(-Inf, 10, 20, 30, 40, Inf), labels = 5:1))
class_wetness <- function(x) as.numeric(cut(x, c(-Inf, 1, 2, 4, 8, Inf), labels = 1:5))
stage_static <- function(c) {
  if (!c$flood_enabled) { message('FLD desactivado: faltan capas estáticas globales.'); return(invisible(NULL)) }
  if (any(!file.exists(unlist(c$static)))) stop('Configure las cuatro capas estáticas globales.')
  inv <- load_inventory(c); grid <- terra::rast(inv$prec$path[1])
  for (v in names(c$static)) checkpoint(c, paste0('static_align_', v), function() {
    r <- terra::rast(c$static[[v]])
    if (terra::nlyr(r) != 1L) stop('Se esperaba una capa estática: ', v)
    # Una capa regional como la arcilla de África debe rechazarse.
    bounds <- terra::project(terra::as.polygons(terra::ext(grid), crs = terra::crs(grid)), terra::crs(r))
    a <- as.vector(terra::ext(r)); b <- as.vector(terra::ext(bounds))
    if (a[1] > b[1] || a[2] < b[2] || a[3] > b[3] || a[4] < b[4])
      stop('La extensión de ', v, ' no cubre CHIRPS. Se necesita una fuente global.')
    dest <- pth(c, 'static_aligned', paste0(v, '.tif')); ensure_dir(dirname(dest))
    method <- if (v == 'landcover') 'near' else 'average'
    tmp <- paste0(dest, '.partial.tif')
    if (terra::same.crs(r, grid)) terra::resample(r, grid, method = method,
        filename = tmp, overwrite = TRUE, wopt = list(gdal = 'COMPRESS=LZW'))
    else terra::project(r, grid, method = method, filename = tmp,
        overwrite = TRUE, wopt = list(gdal = 'COMPRESS=LZW'))
    publish_file(tmp, dest)
  })
  r <- terra::rast(vapply(names(c$static), function(v) pth(c, 'static_aligned', paste0(v, '.tif')), character(1)))
  tiles <- make_tiles(grid, c)
  for (k in seq_len(nrow(tiles))) {
    tile <- tiles[k, ]
    checkpoint(c, paste0('static_table_', tile$tile_id), function() {
      x <- read_tile(terra::sources(r), tile, grid, 'static')
      if (any(x[, 1] < 0 | x[, 1] > 100, na.rm = TRUE) ||
          any(x[, 2] < 0, na.rm = TRUE) ||
          any(!x[, 4] %in% 1:5 & !is.na(x[, 4]))) stop('Unidades/códigos estáticos inválidos.')
      d <- data.frame(grid_id = 'prec', cell_id = cell_ids(grid, tile),
                      clay_class = class_clay(x[, 1]), slope_class = class_slope(x[, 2]),
                      wetness_class = class_wetness(x[, 3]), landcover_class = x[, 4])
      d <- d[complete.cases(d), , drop = FALSE]
      write_table(d, table_path(c, 'static', tile$tile_id), c)
    })
  }
}
