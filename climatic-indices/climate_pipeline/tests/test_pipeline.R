# Integración sintética: 2 años, grillas distintas, RDS, sin unidades de red.
for (f in sort(list.files('R', pattern = '\\.R$', full.names = TRUE))) source(f)
source('config.R')
c <- cfg
root <- tempfile('yapu_test_'); dir.create(root)
c$roots <- setNames(as.list(file.path(root, c('prec','tmax','tmin'))), c('prec','tmax','tmin'))
c$output <- file.path(root, 'output'); c$tempdir <- file.path(root, 'tmp')
c$run_id <- 'synthetic'; c$years <- 2000:2001; c$reference_years <- 2000:2001
c$periods <- list(historical=2000:2001, recent_past=2001L)
c$storage <- 'rds'; c$enso_csv <- ''; c$min_enso_years <- 1L
c$min_wet_reference_days <- 3L # solo dos años en este ensayo sintético
c$tile_nrow <- 2L; c$tile_ncol <- 2L; c$flood_enabled <- TRUE
p <- terra::rast(nrows=4, ncols=4, xmin=-0.1, xmax=0.1, ymin=-0.1, ymax=0.1, crs='EPSG:4326')
t <- terra::rast(nrows=6, ncols=4, xmin=-0.1, xmax=0.1, ymin=-0.15, ymax=0.15, crs='EPSG:4326')
dates <- seq(as.Date('2000-01-01'), as.Date('2001-12-31'), by='day')
for (v in names(c$roots)) {
  for (y in c$years) ensure_dir(file.path(c$roots[[v]], y))
  for (j in seq_along(dates)) {
    d <- dates[j]; y <- format(d, '%Y'); stamp <- format(d, '%Y.%m.%d')
    name <- switch(v, prec=paste0('chirps-v3.0.', stamp, '.tif'),
      tmax=paste0('CHIRTS-ERA5.daily_Tmax.', stamp, '.tif'),
      tmin=paste0('CHIRTS-ERA5.daily_Tmin.', stamp, '.tif'))
    r <- if (v=='prec') p else t
    value <- switch(v, prec=if (j %% 4L == 0L) 10 else 0,
                    tmax=25 + sin(j / 10), tmin=-1 + sin(j / 10))
    terra::values(r) <- value
    terra::values(r)[1] <- NA_real_ # Celda sin datos en toda la serie.
    if (v == 'prec') terra::values(r)[c(9, 10, 11, 12, 13, 15, 16)] <- NA_real_
    # También se ensayan un bloque vacío y un bloque con una sola celda válida.
    terra::writeRaster(r, file.path(c$roots[[v]], y, name), overwrite=TRUE)
  }
}
for (v in names(c$static)) {
  r <- p; terra::values(r) <- switch(v, clay=50, slope=15, wetness=3, landcover=4)
  path <- file.path(root, paste0(v, '.tif')); terra::writeRaster(r, path)
  c$static[[v]] <- path
}
# ENOS sintético incluye grupos con cero años para probar porcentajes NA.
enso <- expand.grid(year=c$years, month=1:12); enso$oni <- 0
enso$oni[enso$year==2000 & enso$month==1] <- .6
enso$oni[enso$year==2001 & enso$month==2] <- -.6
c$enso_csv <- file.path(root, 'enso.csv'); write.csv(enso,c$enso_csv,row.names=FALSE)
initialize(c, check_version=FALSE)
stage_inventory(c); stage_reference(c); stage_precipitation(c)
stage_temperature(c); stage_static(c); stage_flood(c); stage_summary(c)
# Cada bloque permite volver a localizar sus celdas sin duplicar coordenadas.
d <- read_table(table_path(c, 'precipitation', 'r00001_c00001', 2000), c)
stopifnot(nrow(d)==36L, !1L %in% d$cell_id,
          all(d$prec_nvalid[d$month==2] == 29L),
          all(d$prec_total_mm %% 10 == 0), all(d$cdd_days <= 3))
f <- read_table(table_path(c, 'fld', 'r00001_c00001', 2000), c)
stopifnot(all(f$fld_score >= 1 & f$fld_score <= 5, na.rm=TRUE))
s <- read_table(table_path(c, 'summary/precipitation', 'r00001_c00001_m02'), c)
all_conditions <- s
s <- s[s$enso=='all', ]
stopifnot(all(s$enso=='all'), setequal(s$period,c('historical','recent_past')),
          !anyNA(s$value), all(s$n_years %in% 1:2))
single <- read_table(table_path(c, 'summary/precipitation', 'r00003_c00001_m02'), c)
single <- single[single$enso=='all', ]
stopifnot(all(single$cell_id == 14L), !anyNA(single$value))
empty <- read_table(table_path(c, 'fld', 'r00003_c00003', 2000), c)
stopifnot(nrow(empty) == 0L)
# Categorías anuales separadas y nueve umbrales por índice/celda/mes.
q <- read_table(table_path(c, 'thresholds/precipitation', 'r00001_c00001_m02'), c)
stopifnot(all(paste0('p',seq(10,90,10)) %in% names(q)))
for (metric in c('cdd_days','drd_days','p95_wet_days_pct')) {
  a <- read_table(table_path(c,paste0('classification/precipitation/',metric),'r00001_c00001_m02'),c)
  qq <- q[q$metric==metric, ]
  expected <- class_values(a$value, as.matrix(qq[match(a$cell_id,qq$cell_id),paste0('p',seq(10,90,10))]))
  stopifnot(identical(a$magnitude,expected),all(a$magnitude %in% 1:10))
}
qs <- read_table(table_path(c,'thresholds/fld','r00001_c00001_m02'),c)
a <- read_table(table_path(c,'classification/fld/fld_score','r00001_c00001_m02'),c)
stopifnot(identical(a$magnitude,class_values(a$value,as.matrix(qs[match(a$cell_id,qs$cell_id),paste0('p',seq(10,90,10))]))))
stopifnot(all(abs(rowSums(s[paste0('class_',1:10,'_pct')])-100)<1e-8))
no_years <- all_conditions[all_conditions$enso=='elnino', ]
stopifnot(all(no_years$n_classified==0),all(is.na(no_years$class_1_pct)))
# Reanudar no vuelve a escribir un hito terminado.
path <- table_path(c, 'precipitation', 'r00001_c00001', 2000)
before <- file.info(path)$mtime
stage_precipitation(c)
stopifnot(identical(before, file.info(path)$mtime))
# El inventario rechaza archivos faltantes.
unlink(load_inventory(c)$prec$path[1])
stopifnot(inherits(try(stage_inventory(c), silent=TRUE), 'try-error'))
cat('OK: integración, referencias, bases, seis índices, resúmenes y reanudación.\n')
unlink(root, recursive=TRUE)
