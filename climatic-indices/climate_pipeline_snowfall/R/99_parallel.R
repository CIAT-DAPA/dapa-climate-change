# Snowfall local (SOCK), tareas independientes por bloque. No se exportan rásteres.
# Estos alias apuntan al motor secuencial; los workers invocan SOLO los alias.
stage_reference_serial <- stage_reference
stage_precipitation_serial <- stage_precipitation
stage_temperature_serial <- stage_temperature
stage_flood_serial <- stage_flood
stage_summary_serial <- stage_summary

parallel_options <- function(c) {
  p <- c$parallel
  if (is.null(p)) p <- list(enabled = FALSE, workers = 1L,
                            threads_per_worker = 1L, memfrac_per_worker = .1)
  if (!isTRUE(p$enabled)) p$workers <- 1L
  stopifnot(length(p$workers) == 1L, is.finite(p$workers),
            p$workers >= 1L, p$workers == as.integer(p$workers),
            p$threads_per_worker >= 1L,
            p$memfrac_per_worker > 0, p$memfrac_per_worker < 1)
  p
}
# Construir trabajos con IDs únicos y excluir hitos ya terminados.
spatial_jobs <- function(c, stage) {
  inv <- load_inventory(c)
  groups <- switch(stage,
    reference = if (is.null(c$reference_sources)) c('prec','tmax') else c$reference_sources,
    precipitation = 'prec', temperature = 'tmax', flood = 'prec',
    summary = if (is.null(c$summary_sources)) c('precipitation','temperature','hwd',
                 if (c$flood_enabled) 'fld') else c$summary_sources,
    stop('Etapa no paralelizable: ', stage))
  jobs <- list()
  for (group in groups) {
    source <- if (group %in% c('tmax','temperature','hwd')) 'tmax' else 'prec'
    tiles <- make_tiles(terra::rast(inv[[source]]$path[1]), c)
    for (i in seq_len(nrow(tiles))) {
      id <- tiles$tile_id[i]
      keys <- switch(stage,
        reference = paste0('reference_',group,'_',id),
        precipitation = paste0('prec_',id,'_',c$years),
        temperature = paste0('temperature_',id),
        flood = paste0('fld_',id),
        summary = paste0('summary_',group,'_',id,'_m',1:12))
      if (all(file.exists(pth(c,'checkpoints',paste0(keys,'.rds'))))) next
      jobs[[length(jobs)+1L]] <- list(stage=stage, group=group, tile_id=id,
                                     key=paste(stage,group,id,sep='_'))
    }
  }
  if (anyDuplicated(vapply(jobs, function(j) j$key, character(1)))) stop('Trabajos duplicados.')
  jobs
}
# Función autocontenida: es lo único que debe ejecutarse antes de cargar módulos
# en un worker nuevo. Hereda las bibliotecas de R del proceso principal.
worker_bootstrap <- function(cfg, project, libraries, session_dir) {
  .libPaths(libraries)
  setwd(project)
  threads <- as.character(cfg$parallel$threads_per_worker)
  Sys.setenv(OMP_NUM_THREADS=threads, OPENBLAS_NUM_THREADS=threads,
             MKL_NUM_THREADS=threads, GDAL_NUM_THREADS=threads,
             ARROW_NUM_THREADS=threads)
  for (f in sort(list.files('R',pattern='\\.R$',full.names=TRUE)))
    source(f,local=.GlobalEnv,encoding='UTF-8')
  if (!requireNamespace('terra',quietly=TRUE)) stop('terra no está disponible en el worker.')
  if (cfg$storage=='parquet' && !requireNamespace('arrow',quietly=TRUE))
    stop('arrow no está disponible en el worker.')
  if (cfg$storage=='parquet') arrow::set_cpu_count(as.integer(threads))
  cfg$parallel$enabled <- FALSE
  cfg$tempdir <- file.path(cfg$tempdir,cfg$run_id,paste0('worker_',Sys.getpid()))
  dir.create(cfg$tempdir,recursive=TRUE,showWarnings=FALSE)
  if (!all(vapply(cfg$roots,dir.exists,logical(1))))
    stop('El worker no puede acceder a las rutas de entrada. Compruebe X: o use rutas UNC.')
  terra::terraOptions(tempdir=cfg$tempdir,memfrac=cfg$parallel$memfrac_per_worker,progress=0)
  log_path <- file.path(session_dir,paste0('worker_',Sys.getpid(),'.log'))
  # Cada proceso tiene su propio archivo de log, sin escrituras compartidas.
  con <- file(log_path,open='at'); sink(con); sink(con,type='message')
  assign('.yapu_worker_log_connection',con,envir=.GlobalEnv)
  assign('.yapu_worker_cfg',cfg,envir=.GlobalEnv)
  assign('.yapu_session_dir',session_dir,envir=.GlobalEnv)
  list(pid=Sys.getpid(),tempdir=cfg$tempdir,log=log_path)
}
# El worker publica duración/estado incluso si falla el procesamiento de un bloque.
spatial_worker <- function(job) {
  c <- get('.yapu_worker_cfg',envir=.GlobalEnv)
  session <- get('.yapu_session_dir',envir=.GlobalEnv)
  c$tile_ids <- job$tile_id
  if (job$stage=='reference') c$reference_sources <- job$group
  if (job$stage=='summary') c$summary_sources <- job$group
  start <- Sys.time(); cpu_start <- sum(proc.time()[c('user.self','sys.self')])
  metrics <- new.env(parent=emptyenv())
  metrics$open_seconds <- metrics$read_seconds <- 0
  metrics$read_calls <- metrics$file_opens <- 0L
  assign('.yapu_metrics',metrics,envir=.GlobalEnv)
  on.exit(rm('.yapu_metrics',envir=.GlobalEnv),add=TRUE)
  record <- list(key=job$key, stage=job$stage, group=job$group,
                 tile_id=job$tile_id,pid=Sys.getpid(),start=start,
                 status='running',elapsed_seconds=NA_real_,error=NA_character_)
  path <- file.path(session,paste0(job$key,'.rds'))
  atomic_rds(record,path)
  message(format(start),' | START ',job$key,' | PID ',Sys.getpid())
  result <- tryCatch({
    get(paste0('stage_',job$stage,'_serial'),mode='function')(c)
    NULL
  },error=function(e) conditionMessage(e))
  record$end <- Sys.time()
  record$elapsed_seconds <- as.numeric(difftime(record$end,start,units='secs'))
  record$cpu_seconds <- sum(proc.time()[c('user.self','sys.self')]) - cpu_start
  record$open_seconds <- metrics$open_seconds
  record$read_seconds <- metrics$read_seconds
  record$read_calls <- metrics$read_calls
  record$file_opens <- metrics$file_opens
  record$other_seconds <- max(0,record$elapsed_seconds-metrics$open_seconds-metrics$read_seconds)
  record$status <- if (is.null(result)) 'done' else 'error'
  if (!is.null(result)) record$error <- result
  atomic_rds(record,path)
  message(format(record$end),' | ',toupper(record$status),' ',job$key,
          ' | ',round(record$elapsed_seconds,1),' s')
  if (!is.null(result)) message(result)
  gc(verbose=FALSE)
  record
}
# Mantener controles/timings también en modo 1 proceso, sin redirigir la consola.
run_local_job <- function(job,c,session) {
  assign('.yapu_worker_cfg',c,envir=.GlobalEnv)
  assign('.yapu_session_dir',session,envir=.GlobalEnv)
  spatial_worker(job)
}
run_spatial_stage <- function(c,stage) {
  if (stage=='flood' && !c$flood_enabled) return(invisible(NULL))
  p <- parallel_options(c); c$parallel <- p
  if (!dir.exists('R')) stop('Sitúese en la carpeta climate_pipeline antes de ejecutar.')
  # Protección frente a dos ejecuciones que escriban en la misma corrida.
  ensure_dir(run_dir(c)); lock <- pth(c,'.spatial_stage_lock')
  if (!dir.create(lock,showWarnings=FALSE))
    stop('Otra etapa usa esta corrida o quedó un bloqueo tras un cierre abrupto: ',lock)
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  writeLines(c(paste0('pid=',Sys.getpid()),paste0('stage=',stage)),file.path(lock,'owner.txt'))
  jobs <- spatial_jobs(c,stage)
  if (!length(jobs)) { message(stage,': todos los bloques están terminados.'); return(invisible(NULL)) }
  workers <- min(as.integer(p$workers),length(jobs))
  project <- normalizePath(getwd(),winslash='/',mustWork=TRUE)
  c$output <- normalizePath(c$output,winslash='/',mustWork=TRUE)
  ensure_dir(c$tempdir); c$tempdir <- normalizePath(c$tempdir,winslash='/',mustWork=TRUE)
  # Las rutas relativas se resuelven una sola vez en el proceso principal.
  c$roots <- lapply(c$roots,normalizePath,winslash='/',mustWork=TRUE)
  if (nzchar(c$enso_csv)) c$enso_csv <- normalizePath(c$enso_csv,winslash='/',mustWork=FALSE)
  session <- pth(c,'parallel_logs',paste0(stage,'_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  ensure_dir(session); session <- normalizePath(session,winslash='/',mustWork=TRUE)
  message(stage,': ',length(jobs),' bloques pendientes; ',workers,' trabajadores.\nRegistros: ',session)
  start <- Sys.time()
  if (workers>1L) {
    if (!requireNamespace('snowfall',quietly=TRUE)) stop('Instale snowfall: install.packages("snowfall")')
    snowfall::sfInit(parallel=TRUE,cpus=workers,type='SOCK',
                     slaveOutfile=file.path(session,'startup.log'))
    on.exit(try(snowfall::sfStop(),silent=TRUE),add=TRUE,after=FALSE)
    if (!snowfall::sfParallel() || snowfall::sfCpus()!=workers)
      stop('Snowfall no inició el número solicitado de trabajadores SOCK.')
    snowfall::sfClusterCall(worker_bootstrap,c,project,.libPaths(),session)
    records <- snowfall::sfClusterApplyLB(jobs,spatial_worker)
  } else {
    original <- terra::terraOptions(print=FALSE)
    on.exit(terra::terraOptions(tempdir=original$tempdir,memfrac=original$memfrac,progress=0),add=TRUE)
    c$tempdir <- file.path(c$tempdir,c$run_id,paste0('worker_',Sys.getpid()))
    ensure_dir(c$tempdir)
    terra::terraOptions(tempdir=c$tempdir,memfrac=p$memfrac_per_worker,progress=0)
    records <- lapply(jobs,run_local_job,c=c,session=session)
  }
  elapsed <- as.numeric(difftime(Sys.time(),start,units='secs'))
  atomic_rds(list(stage=stage,workers=workers,elapsed_seconds=elapsed,
                 n_jobs=length(jobs),records=records),file.path(session,'stage_result.rds'))
  failed <- vapply(records,function(x) x$status=='error',logical(1))
  message(stage,': ',sum(!failed),' completos, ',sum(failed),' fallidos; ',round(elapsed/60,2),' minutos.')
  if (any(failed)) stop('Fallaron ',sum(failed),' bloques. Revise parallel_logs y vuelva a ejecutar: los completos se omiten.')
  invisible(list(workers=workers,elapsed_seconds=elapsed,n_jobs=length(jobs),session=session))
}
stage_reference <- function(c) run_spatial_stage(c,'reference')
stage_precipitation <- function(c) run_spatial_stage(c,'precipitation')
stage_temperature <- function(c) run_spatial_stage(c,'temperature')
stage_flood <- function(c) run_spatial_stage(c,'flood')
stage_summary <- function(c) run_spatial_stage(c,'summary')

# Consultar desde OTRA sesión de R mientras la primera está ocupada.
parallel_status <- function(c) {
  files <- list.files(pth(c,'parallel_logs'),pattern='\\.rds$',recursive=TRUE,full.names=TRUE)
  files <- files[basename(files)!='stage_result.rds']
  records <- lapply(files,function(f) tryCatch(readRDS(f),error=function(e) NULL))
  records <- Filter(Negate(is.null),records)
  if (!length(records)) return(data.frame())
  do.call(rbind,lapply(records,function(x) data.frame(stage=x$stage,group=x$group,
    tile_id=x$tile_id,pid=x$pid,start=as.character(x$start),status=x$status,
    elapsed_seconds=if (x$status=='running') as.numeric(difftime(Sys.time(),x$start,units='secs')) else x$elapsed_seconds,
    open_seconds=if (is.null(x$open_seconds)) NA_real_ else x$open_seconds,
    read_seconds=if (is.null(x$read_seconds)) NA_real_ else x$read_seconds,
    cpu_seconds=if (is.null(x$cpu_seconds)) NA_real_ else x$cpu_seconds, error=x$error)))
}

# Benchmark sin reutilizar resultados entre modos: cada modo tiene su propia
# corrida de prueba y mide la misma referencia/fuentes/años/bloques.
benchmark_reference <- function(c,tile_ids,workers=c(1L,2L,4L),source='prec',
                                label=format(Sys.time(),'%Y%m%d_%H%M%S'),
                                check_version=TRUE) {
  stopifnot(length(tile_ids)>0L,!anyDuplicated(tile_ids),
            all(workers>=1L),!anyDuplicated(workers),source %in% c('prec','tmax'))
  # Impide comparar workers solicitados con un número real menor de tareas.
  if (length(tile_ids)<max(workers)) stop('Seleccione al menos tantos bloques como el máximo de workers.')
  result <- vector('list',length(workers))
  for (i in seq_along(workers)) {
    b <- c
    b$run_id <- paste0(c$run_id,'_benchmark_',label,'_',workers[i],'w')
    if (dir.exists(run_dir(b))) stop('La corrida de benchmark ya existe; cambie label para una medición limpia.')
    b$tile_ids <- tile_ids; b$reference_sources <- source
    b$parallel$enabled <- workers[i]>1L; b$parallel$workers <- workers[i]
    initialize(b,check_version=check_version); stage_inventory(b)
    measured <- stage_reference(b)
    result[[i]] <- data.frame(workers=measured$workers,blocks=measured$n_jobs,
       elapsed_seconds=measured$elapsed_seconds,
       blocks_per_hour=3600*measured$n_jobs/measured$elapsed_seconds,
       run_id=b$run_id)
  }
  result <- do.call(rbind,result)
  atomic_rds(result,pth(c,paste0('benchmark_',label,'.rds')))
  print(result,row.names=FALSE)
  invisible(result)
}
