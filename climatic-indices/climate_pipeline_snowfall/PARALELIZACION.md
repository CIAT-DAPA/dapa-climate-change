# Ejecución con snowfall

## En el servidor

1. Detener la ejecución anterior y cerrar sus procesos antes de reemplazar el
   código. Conservar su carpeta de resultados. La firma de código cambia en
   esta versión: las corridas anteriores no se migran automáticamente.
2. Descomprimir la nueva versión en una carpeta nueva y trasladar las rutas y
   periodos propios a `config.R`. Usar un **run_id nuevo**, por ejemplo el incluido
   `1981_2025_deciles_snowfall_v3`. No borrar el historial previo.
3. Instalar las bibliotecas con la misma versión de R que ejecutará el proyecto:

```r
install.packages(c("terra", "arrow", "matrixStats", "snowfall"))
```

4. Configurar en `config.R`:

```r
parallel = list(
  enabled = TRUE,
  workers = 4L,
  threads_per_worker = 1L,
  memfrac_per_worker = 0.10
),
```

Cuatro trabajadores son un punto de partida, no una garantía de aceleración.
Si la memoria o la lectura de X: se saturan, probar con dos. No aumentar al
número total de procesadores sin medir. `memfrac_per_worker` es una fracción
por proceso que terra usa para sus operaciones, no un límite duro de la RAM
utilizada por todas las matrices de R. El consumo de memoria crece con workers.

5. En la consola de R, no en Terminal:

```r
setwd("C:/_scripts/dapa-climate-change/climatic-indices/climate_pipeline")
source("start.R", encoding = "UTF-8")

stage_reference(cfg)
# Esperar a que vuelva el símbolo > antes de continuar.
stage_precipitation(cfg)
stage_temperature(cfg)
stage_static(cfg)
stage_flood(cfg)
stage_summary(cfg)
```

`start.R` carga las funciones, inicializa la corrida y valida el inventario;
no inicia referencias ni índices. No es necesario llamar manualmente a `sfInit`,
exportar objetos ni detener el cluster: cada etapa lo hace y libera los procesos
al terminar o al detectar un error.

También funciona la interfaz de Terminal:

```text
Rscript run.R reference
Rscript run.R precipitation
Rscript run.R temperature
Rscript run.R static flood
Rscript run.R summary
```

La carpeta de trabajo debe ser la del proyecto. `run.R` sin argumentos ejecuta
todas las etapas. Por defecto FLD sigue desactivado hasta configurar las capas.

## Qué se paraleliza

- Referencias: un trabajo por fuente (`prec` o `tmax`) y bloque.
- Precipitación: un trabajo por bloque con todos sus años.
- Temperatura: un trabajo por bloque con todos sus años EN ORDEN; mantiene
  continuidad de episodios de calor entre meses y años.
- Inundación: un trabajo por bloque, tras la preparación de capas estáticas.
- Resúmenes/clasificación: un trabajo por fuente de índices y bloque.

El inventario y la etapa `static` permanecen secuenciales. Se espera a terminar
una etapa antes de ejecutar la siguiente que dependa de ella. El tamaño/ID de
bloque no cambia al aumentar workers. Los cálculos científicos y las categorías
por deciles se conservan.

`snowfall::sfClusterApplyLB()` reparte el siguiente bloque al trabajador que
queda disponible. No se envían objetos `SpatRaster` por sockets: cada proceso
abre sus propias fuentes. Las bibliotecas se cargan desde las rutas del R
principal; los workers deben poder ver X:, F: y las demás unidades utilizadas.
Si Windows no expone una unidad al proceso hijo, configurar la ruta UNC de esa
unidad y una nueva corrida con la configuración correspondiente.

Cada trabajador usa un temporal bajo:

```text
<tempdir>/<run_id>/worker_<PID>/
```

El controlador crea un bloqueo de corrida para impedir dos etapas espaciales
simultáneas sobre los mismos archivos. Si el servidor se apaga bruscamente,
podría quedar `.spatial_stage_lock`. Antes de retirarlo manualmente, comprobar
que no siga ningún proceso usando esa corrida. Los errores normales lo liberan.

## Seguir el avance y reanudar

Al iniciar una etapa aparecen su número de bloques pendientes, trabajadores
reales y la carpeta de registros. La consola principal espera al reparto de
trabajos: sus mensajes no representan todo el progreso. En `parallel_logs/`
cada trabajador tiene un `.log` con inicio/fin y cada trabajo un `.rds` con su
estado y duración. Desde OTRA sesión de R se puede consultar:

```r
setwd("C:/_scripts/dapa-climate-change/climatic-indices/climate_pipeline")
source("config.R", encoding="UTF-8")
for (f in sort(list.files("R",pattern="\\.R$",full.names=TRUE))) {
  source(f,encoding="UTF-8")
}
x <- parallel_status(cfg)
table(x$status)
tail(x, 10)
```

Este estado es el último registro guardado: un `running` tras un cierre forzado
no demuestra que el proceso siga vivo. Se conservan sesiones anteriores en la
consulta; mirar su fecha/carpeta para interpretar reintentos.

Los tiempos incluyen `elapsed_seconds` (duración total), `open_seconds`
(construcción/apertura de las fuentes diarias), `read_seconds` (lectura de
valores), y `cpu_seconds` (tiempo de CPU del proceso). `other_seconds`, en los
registros RDS, incluye cálculo, limpieza, lectura de tablas y escritura; no es
una medición exclusiva de cálculo. Las aperturas/lecturas de tablas Parquet/RDS
no se contabilizan en las métricas de GeoTIFF.

Un error de bloque se registra y los demás bloques pueden terminar. La etapa
informa de los fallos al final. Corregir la causa y ejecutar la misma etapa:
los hitos completos se omiten. No borrar salidas conservando sus marcadores.
Un bloque de temperatura incompleto se recalcula desde el primer año.

Dentro de esta versión se puede cambiar solo `cfg$parallel$workers` o editar
`parallel` en `config.R` sin cambiar `run_id`. Para otros cambios científicos,
usar una corrida nueva. No arrancar otra etapa del mismo run mientras la actual
siga en marcha.

## Comparar 1, 2 y 4 trabajadores

La función exige al menos cuatro bloques distintos si se prueban cuatro workers:

```r
# Elegir cuatro IDs REALES de grids/prec_tiles.csv, preferentemente de tierra.
ids <- c("ID_BLOQUE_1", "ID_BLOQUE_2", "ID_BLOQUE_3", "ID_BLOQUE_4")
benchmark_reference(cfg, tile_ids=ids, workers=c(1L,2L,4L), source="prec")
```

Los nombres del ejemplo son marcadores: sustituirlos por IDs del inventario,
como `r00001_c00001`. Evitar un ensayo exclusivamente oceánico. Cada modo usa
los mismos archivos, años y bloques y guarda resultados en un `run_id` de
benchmark distinto, de modo que no se cronometren saltos de hitos existentes.
El benchmark rechaza nombres de corridas de prueba ya existentes.

La tabla final muestra duración total y bloques/hora, incluyendo arranque de
workers. Los ensayos pueden ser largos con todos los años. Para un primer
ensayo corto se puede usar una copia de configuración:

```r
b <- cfg
b$reference_years <- 2018:2020
benchmark_reference(b,tile_ids=ids,workers=c(1L,2L,4L),source="prec")
```

Ese ensayo solo sirve para explorar la concurrencia de lectura; no estima de
forma exacta el tiempo de la referencia completa. Sus resultados no sustituyen
los de la corrida de producción. La caché del sistema puede favorecer los modos
posteriores; repetir invirtiendo el orden y comparar bloques/hora antes de
subir workers. No se promete aceleración lineal sobre una unidad de red.

## Pruebas locales

```text
Rscript tests/test_indices.R
Rscript tests/test_parallel.R
```

`test_parallel.R` ejecuta primero la integración secuencial y luego compara
referencias y tablas con dos trabajadores SOCK. También prueba recuperación de
errores, omisión de hitos y el benchmark 1/2/4. Los datos son sintéticos y pequeños:
no son una medición de rendimiento sobre X: ni una validación de R 4.6 Windows.

Documentación de snowfall:
https://search.r-project.org/CRAN/refmans/snowfall/html/snowfall-b-init.html
https://search.r-project.org/CRAN/refmans/snowfall/html/snowfall-c-calculation.html

En la prueba local, snowfall 1.84-6.3 emitió `Unknown option on commandline: --file`
al ejecutarse con Rscript. La advertencia procede del análisis de argumentos
del paquete; no impidió los cálculos ni las pruebas de igualdad. Los errores
de conexión o de bloques sí se informan como fallos y deben revisarse.
