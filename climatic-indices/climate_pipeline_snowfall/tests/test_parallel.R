# Integración SOCK real: comparar con secuencial, errores, hitos y benchmark.
# Requiere snowfall/snow/terra. Rscript tests/test_parallel.R
stopifnot(requireNamespace('snowfall',quietly=TRUE))
Sys.setenv(YAPU_KEEP_TEST_DATA='1')
source('tests/test_pipeline.R',encoding='UTF-8')
serial <- c
p <- c; p$run_id <- 'synthetic_parallel'; p$parallel$enabled <- TRUE
p$parallel$workers <- 2L
initialize(p,check_version=FALSE); stage_inventory(p)
stage_reference(p); stage_precipitation(p); stage_temperature(p)
stage_static(p); stage_flood(p); stage_summary(p)
# Todas las referencias y tablas deben ser idénticas; se excluyen tiempos/logs.
for (folder in c('reference','precipitation','temperature','hwd','static','fld',
                 'fld_thresholds','classification','thresholds','summary')) {
  a <- list.files(pth(serial,folder),pattern='\\.rds$',recursive=TRUE)
  b <- list.files(pth(p,folder),pattern='\\.rds$',recursive=TRUE)
  stopifnot(identical(a,b))
  for (f in a) stopifnot(identical(readRDS(pth(serial,folder,f)),readRDS(pth(p,folder,f))))
}
status <- parallel_status(p)
stopifnot(all(status$status=='done'),length(unique(status$pid))>=2L,
          all(status$elapsed_seconds>=0),sum(status$open_seconds,na.rm=TRUE)>0)
# Una etapa terminada no abre workers ni reescribe salidas.
file <- table_path(p,'precipitation','r00001_c00001',2000)
mtime <- file.info(file)$mtime; stage_precipitation(p)
stopifnot(identical(mtime,file.info(file)$mtime))
# Cambiar solo workers no invalida una corrida científica existente.
p$parallel$workers <- 1L; initialize(p,check_version=FALSE); p$parallel$workers <- 2L
# Error en un bloque: el resto termina y el bloque fallido se reanuda.
p$tile_ids <- c('r00001_c00001','r00003_c00001')
for (id in p$tile_ids) unlink(pth(p,'checkpoints',paste0('prec_',id,'_',p$years,'.rds')))
r <- ref_path(p,'prec','r00001_c00001'); stopifnot(file.rename(r,paste0(r,'.bak')))
failed <- try(stage_precipitation(p),silent=TRUE)
stopifnot(inherits(failed,'try-error'),
          all(file.exists(pth(p,'checkpoints',paste0('prec_r00003_c00001_',p$years,'.rds')))),
          !file.exists(pth(p,'.spatial_stage_lock')))
stopifnot(file.rename(paste0(r,'.bak'),r)); stage_precipitation(p)
stopifnot(all(file.exists(pth(p,'checkpoints',paste0('prec_r00001_c00001_',p$years,'.rds')))))
# Benchmark 1/2/4 con resultados aislados para no medir saltos de hitos.
p$tile_ids <- NULL
ids <- c('r00001_c00001','r00003_c00001','r00001_c00003','r00003_c00003')
b <- benchmark_reference(p,ids,workers=c(1L,2L,4L),label='test',check_version=FALSE)
stopifnot(identical(b$workers,c(1L,2L,4L)),all(b$blocks==4L))
cat('OK: SOCK 2 workers idéntico a secuencial, errores recuperables, hitos y benchmark 1/2/4.\n')
unlink(root,recursive=TRUE)
