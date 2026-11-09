# HITO 01: inventario completo y registro independiente de cada malla.
scan_source <- function(root, var, years) {
  if (!dir.exists(root)) stop('No existe el directorio: ', root)
  paths <- unlist(lapply(years, function(y) list.files(file.path(root, y),
                  pattern = '\\.[Tt][Ii][Ff]([Ff])?$', full.names = TRUE)), use.names = FALSE)
  pattern <- switch(var,
    prec = '^chirps-v3\\.0\\.(rnl\\.)?[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.tiff?$',
    tmax = '^CHIRTS-ERA5\\.daily_Tmax\\.[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.tiff?$',
    tmin = '^CHIRTS-ERA5\\.daily_Tmin\\.[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.tiff?$')
  paths <- paths[grepl(pattern, basename(paths), ignore.case = TRUE)]
  dates <- as.Date(sub('.*([0-9]{4})\\.([0-9]{2})\\.([0-9]{2})\\.tiff?$',
                       '\\1-\\2-\\3', basename(paths), ignore.case = TRUE))
  if (anyNA(dates) || anyDuplicated(dates)) stop('Fechas inválidas/duplicadas en ', var)
  expected <- do.call(base::c, lapply(years, function(y)
    seq(as.Date(paste0(y, '-01-01')), as.Date(paste0(y, '-12-31')), by = 'day')))
  if (!setequal(dates, expected)) stop(var, ': faltan fechas o hay fechas fuera del periodo. Faltan: ',
    paste(head(as.character(expected[!expected %in% dates]), 10), collapse = ', '))
  info <- file.info(paths)
  d <- data.frame(date = dates, year = as.integer(format(dates, '%Y')),
                  month = as.integer(format(dates, '%m')), path = paths,
                  bytes = info$size, mtime = as.numeric(info$mtime))
  d[order(d$date), ]
}
stage_inventory <- function(c) {
  # Volver a ejecutar es barato comparado con leer los valores globales.
  inventories <- lapply(names(c$roots), function(v) scan_source(c$roots[[v]], v, c$years))
  names(inventories) <- names(c$roots)
  dest <- pth(c, 'inventory.rds')
  if (file.exists(dest)) {
    if (!identical(readRDS(dest), inventories))
      stop('Cambió el inventario de entrada. Cree un run_id nuevo.')
    return(invisible(NULL))
  }
  grids <- lapply(inventories, function(d) terra::rast(d$path[1]))
  if (!terra::compareGeom(grids$tmax, grids$tmin, stopOnError = FALSE))
    stop('Tmax y Tmin no comparten geometría. No se remuestrean automáticamente.')
  for (v in names(grids)) {
    g <- grids[[v]]
    if (!terra::is.lonlat(g) || any(abs(terra::res(g) - 0.05) > 1e-8))
      stop('Se esperaba una malla geográfica de 0.05 grados: ', v)
    # Cada geometría se guarda una sola vez como metadatos; sin valores.
    atomic_rds(list(nrow = nrow(g), ncol = ncol(g),
                   extent = as.vector(terra::ext(g)), crs = terra::crs(g),
                   resolution = terra::res(g)), pth(c, 'grids', paste0(v, '.rds')))
    write.csv(make_tiles(g, c), pth(c, 'grids', paste0(v, '_tiles.csv')), row.names = FALSE)
  }
  atomic_rds(readRDS(pth(c, 'grids', 'tmax.rds')), pth(c, 'grids', 'temperature.rds'))
  atomic_rds(inventories, dest)
}
load_inventory <- function(c) {
  if (!file.exists(pth(c, 'inventory.rds'))) stop('Ejecute inventory primero.')
  readRDS(pth(c, 'inventory.rds'))
}
source_month <- function(inv, y, m) inv$path[inv$year == y & inv$month == m]
ref_path <- function(c, var, tile) pth(c, 'reference', var, paste0(tile, '.rds'))
