# Publicación recuperable también en Windows (rename no reemplaza archivos).
# No ejecute dos procesos escribiendo al mismo run_id simultáneamente.
publish_file <- function(tmp, path) {
  if (file.exists(path) && unlink(path) != 0L) stop('No se pudo reemplazar: ', path)
  if (!file.rename(tmp, path)) stop('No se pudo publicar: ', path)
}
run_dir <- function(c) file.path(c$output, c$run_id)
pth <- function(c, ...) file.path(run_dir(c), ...)
ensure_dir <- function(x) dir.create(x, recursive = TRUE, showWarnings = FALSE)
atomic_rds <- function(x, path) {
  ensure_dir(dirname(path)); tmp <- paste0(path, '.partial')
  saveRDS(x, tmp, compress = FALSE)
  publish_file(tmp, path)
}
ext_table <- function(c) if (c$storage == 'parquet') 'parquet' else 'rds'
table_path <- function(c, stage, tile, year = NULL) {
  base <- pth(c, stage)
  if (!is.null(year)) base <- file.path(base, paste0('year=', year))
  file.path(base, paste0(tile, '.', ext_table(c)))
}
write_table <- function(x, path, c) {
  ensure_dir(dirname(path)); tmp <- paste0(path, '.partial')
  if (c$storage == 'parquet') arrow::write_parquet(x, tmp, compression = 'zstd')
  else saveRDS(x, tmp, compress = FALSE)
  publish_file(tmp, path)
}
read_table <- function(path, c) {
  if (!file.exists(path)) stop('Falta un hito: ', path)
  if (c$storage == 'parquet') as.data.frame(arrow::read_parquet(path)) else readRDS(path)
}
# El marcador solo se publica tras terminar todos los archivos del trabajo.
checkpoint <- function(c, key, fun) {
  done <- pth(c, 'checkpoints', paste0(key, '.rds'))
  if (file.exists(done)) return(invisible(NULL))
  message(format(Sys.time()), ' | ', key)
  fun()
  atomic_rds(list(completed = Sys.time(), key = key), done)
}
initialize <- function(c, check_version = TRUE) {
  if (check_version && getRversion() < '4.6.0') stop('Ejecute con R >= 4.6.0.')
  stopifnot(requireNamespace('terra', quietly = TRUE),
            c$storage %in% c('parquet', 'rds'),
            c$climatology_window %% 2 == 1,
            c$min_valid_fraction > 0, c$min_valid_fraction <= 1,
            c$min_reference_fraction > 0, c$min_reference_fraction <= 1,
            c$tile_nrow > 0, c$tile_ncol > 0,
            all(c$reference_years %in% c$years),
            identical(as.integer(c$years), sort(unique(as.integer(c$years)))),
            length(c$reference_years) > 0L,
            isTRUE(all.equal(c$class_probs, seq(0.1, 0.9, by = 0.1))),
            length(c$flood_rain_probs) == 4L)
  if (c$storage == 'parquet' && !requireNamespace('arrow', quietly = TRUE))
    stop('Instale arrow o configure storage = "rds".')
  ensure_dir(run_dir(c)); ensure_dir(c$tempdir)
  terra::terraOptions(tempdir = c$tempdir, memfrac = 0.4, progress = 0)
  # Impide reutilizar hitos con una configuración o código distintos.
  files <- sort(list.files('R', pattern = '\\.R$', full.names = TRUE))
  extra_inputs <- unique(c(c$enso_csv, if (c$flood_enabled) unlist(c$static)))
  extra_inputs <- extra_inputs[nzchar(extra_inputs)]
  info <- file.info(extra_inputs)
  signature_cfg <- c
  signature_cfg$parallel <- NULL # workers/memoria no alteran los resultados científicos.
  signature <- list(config = signature_cfg, code = tools::md5sum(c('run.R', files)),
    extra_inputs = data.frame(path = extra_inputs, bytes = info$size,
                              mtime = as.numeric(info$mtime)))
  lock <- pth(c, 'run_signature.rds')
  if (file.exists(lock) && !identical(readRDS(lock), signature))
    stop('Cambió código/configuración. Utilice un run_id NUEVO para evitar hitos obsoletos.')
  if (!file.exists(lock)) atomic_rds(signature, lock)
  writeLines(capture.output(sessionInfo()), pth(c, 'sessionInfo.txt'))
}
make_tiles <- function(r, c) {
  d <- expand.grid(row = seq.int(1L, nrow(r), c$tile_nrow),
                   col = seq.int(1L, ncol(r), c$tile_ncol))
  d$nrows <- pmin(c$tile_nrow, nrow(r) - d$row + 1L)
  d$ncols <- pmin(c$tile_ncol, ncol(r) - d$col + 1L)
  d$tile_id <- sprintf('r%05d_c%05d', d$row, d$col)
  if (!is.null(c$tile_ids)) {
    if (!all(c$tile_ids %in% d$tile_id)) stop('tile_ids no existe en la malla.')
    d <- d[d$tile_id %in% c$tile_ids, , drop = FALSE]
  }
  d
}
cell_ids <- function(r, tile) {
  as.integer(rep(seq.int(tile$row, length.out = tile$nrows) - 1L,
                 each = tile$ncols) * ncol(r) +
               rep(seq.int(tile$col, length.out = tile$ncols), tile$nrows))
}
read_tile <- function(files, tile, template, var) {
  begin <- proc.time()[['elapsed']]
  r <- terra::rast(files)
  if (!terra::compareGeom(r[[1]], template, stopOnError = FALSE))
    stop('Malla distinta para ', var, ': ', files[1])
  terra::readStart(r); on.exit(terra::readStop(r))
  opened <- proc.time()[['elapsed']]
  x <- terra::readValues(r, row = tile$row, nrows = tile$nrows,
                        col = tile$col, ncols = tile$ncols, mat = TRUE)
  if (exists('.yapu_metrics', envir=.GlobalEnv, inherits=FALSE)) {
    metrics <- get('.yapu_metrics',envir=.GlobalEnv)
    metrics$open_seconds <- metrics$open_seconds + opened - begin
    metrics$read_seconds <- metrics$read_seconds + proc.time()[['elapsed']] - opened
    metrics$read_calls <- metrics$read_calls + 1L
    metrics$file_opens <- metrics$file_opens + length(files)
  }
  x[!is.finite(x) | x <= -999] <- NA_real_
  if (var == 'prec') x[x < 0] <- NA_real_
  if (var %in% c('tmax', 'tmin') && any(x < -100 | x > 70, na.rm = TRUE))
    stop('Temperaturas fuera de rango: compruebe unidades Celsius y NoData.')
  x
}
month_dates <- function(y, m) {
  a <- as.Date(sprintf('%04d-%02d-01', y, m))
  b <- seq(a, by = 'month', length.out = 2L)[2L]
  seq(a, b - 1, by = 'day')
}
calendar_day <- function(dates) {
  as.integer(as.Date(paste0('2000-', format(dates, '%m-%d'))) - as.Date('2000-01-01')) + 1L
}
row_q <- function(x, probs) {
  if (!nrow(x)) return(matrix(numeric(), 0L, length(probs)))
  if (requireNamespace('matrixStats', quietly = TRUE))
    return(matrixStats::rowQuantiles(x, probs = probs, na.rm = TRUE, drop = FALSE))
  # Alternativa de base R; matrixStats acelera esta operación.
  ans <- vapply(seq_len(nrow(x)), function(i)
    if (all(is.na(x[i, ]))) rep(NA_real_, length(probs))
    else as.numeric(quantile(x[i, ], probs, na.rm = TRUE, type = 7)),
    numeric(length(probs)))
  t(matrix(ans, nrow = length(probs), ncol = nrow(x)))
}
