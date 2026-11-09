# Procesamiento climático global en tablas — R 4.6

Este proyecto sustituye los rásteres diarios/intermedios por lectura espacial por
bloques y tablas de resultados mensuales. Conserva las fuentes GeoTIFF originales.
No corta países o continentes ni genera shapefiles: eso corresponde al código
posterior de empaquetamiento.

## Preparación

1. Copiar esta carpeta a una ubicación local de trabajo en Windows.
2. Usar R 4.6 o posterior. Instalar desde CRAN:

```r
install.packages(c("terra", "arrow", "matrixStats", "snowfall"))
```

`terra` es obligatorio. `arrow` es obligatorio para Parquet; se puede elegir
`storage = "rds"` para guardar las mismas tablas como objetos R sin Arrow.
`matrixStats` acelera los percentiles; existe una alternativa de base R.
`snowfall` es obligatorio con dos o más trabajadores; instala `snow` como
dependencia. Con `cfg$parallel$enabled = FALSE` se puede ejecutar sin snowfall.
No se utilizan `rgdal`, `rgeos`, `maptools`, `raster`, CDO ni comandos externos.
Registrar las versiones instaladas con el archivo `sessionInfo.txt` de cada corrida.
La disponibilidad de binarios de CRAN depende de la instalación y plataforma;
no se fijan versiones arbitrarias de paquetes sin probarlas en el equipo destino.

3. Editar `config.R`. Las rutas corresponden a las imágenes:

```text
X:/Observed_Climate data/CHIRPS_V3/CHIRPS_V3_ERA5_Daily/
  1981/chirps-v3.0.1981.01.01.tif
X:/Observed_Climate data/CHIRTS_ERA5/tmax_daily_1981_2026/
  1981/CHIRTS-ERA5.daily_Tmax.1981.01.01.tif
X:/Observed_Climate data/CHIRTS_ERA5/tmin_daily_1981_2026/
  1981/CHIRTS-ERA5.daily_Tmin.1981.01.01.tif
```

La carpeta de Tmin se infiere por simetría con Tmax: verificar su nombre real.
También se acepta `chirps-v3.0.rnl.AAAA.MM.DD.tif`. Los nombres sin `rnl` de las
imágenes se aceptan, pero el nombre por sí solo no permite comprobar su origen:
la carpeta debe contener efectivamente la variante diaria ERA5.

Los periodos iniciales son 1981–2025 (histórico), 1981–2000 (anterior) y
2001–2025 (reciente), con referencia 1991–2020. La corrida usa
`run_id = "1981_2025_deciles_snowfall_v3"`.
No se incluyen años incompletos. Cambiar `years`, `reference_years`, `periods` y
`run_id` para otra corrida. Guardar salida y temporales preferentemente en disco
local rápido; la lectura de miles de archivos sobre X: puede dominar el tiempo.

4. Crear un CSV ENOS normalizado con columnas `year,month,oni`, una fila por mes:

```csv
year,month,oni
1981,1,-0.3
1981,2,-0.4
```

El ejemplo solo muestra el formato; no es una serie para usar en producción.
Se incluye `data/enso_monthly.csv` con ONI ERSSTv6 de NOAA para 1981–2025,
asignando cada trimestre al mes central (DJF=enero, ..., NDJ=diciembre).
Copiarlo a la ruta de `enso_csv` o cambiar esa ruta a `data/enso_monthly.csv`
antes de iniciar una corrida nueva. Procedencia en `data/enso_monthly_source.txt`.
Para otra fuente, hay que normalizar las filas por año y columnas por mes.
El índice se interpreta como Niño si >=0.5, Niña si <=-0.5, neutral entre ambos.
Es una agrupación mensual por umbral; no aplica el requisito de persistencia de
una declaración oficial de episodios ENOS. Usar `enso_csv = ""` para omitir la
agrupación y producir únicamente la condición `all`.

## Ejecución

En RStudio abrir esta carpeta como proyecto o usar su ruta completa:

```r
setwd("F:/ruta/climate_pipeline")
source("run.R")
```

Por consola se pueden ejecutar etapas separadas:

```text
Rscript run.R inventory
Rscript run.R reference
Rscript run.R precipitation
Rscript run.R temperature
Rscript run.R static flood
Rscript run.R summary
```

Sin argumentos se ejecutan en ese orden. El inventario se valida al inicio de
cualquier ejecución. No lanzar dos procesos contra el mismo `run_id`.

Para un ensayo económico, crear un `run_id` distinto, elegir unos pocos años
completos y sus años de referencia, y limitar `tile_ids` a IDs del inventario.
Los IDs representan bloques espaciales, no países. Antes de la ejecución global,
comprobar que el bloque elegido intersecte tierra y el tiempo/espacio disponible.

## Módulos e hitos

| Archivo | Procesamiento |
|---|---|
| `config.R` | Rutas, periodos, umbrales y tamaño de bloque |
| `R/00_core.R` | Lectura por bloques, escritura y reanudación |
| `R/01_inventory.R` | Fechas, duplicados, inventario y geometría de mallas |
| `R/02_reference.R` | P95 mensual húmedo y climatología diaria de Tmax |
| `R/03_base_precipitation.R` | Precipitación acumulada mensual |
| `R/04_base_temperature.R` | Tmax/Tmin medias mensuales y Tmedia aproximada |
| `R/05_index_cdd.R` | Máxima racha seca mensual |
| `R/06_index_drd.R` | Número total de días secos |
| `R/07_index_p95.R` | Porcentaje de días húmedos extremos |
| `R/08_index_frd.R` | Días con helada |
| `R/09_index_hwd.R` | Días, eventos y duración máxima de olas de calor |
| `R/10_process.R` | Lectura compartida de bases e índices por fuente |
| `R/11_static.R` | Preparación de capas estáticas globales |
| `R/12_index_fld.R` | Índice compuesto de susceptibilidad a inundación |
| `R/13_summary.R` | Resúmenes ENOS, periodos y magnitudes |
| `R/99_parallel.R` | Distribución SOCK por bloque, registros y benchmark |
| `start.R` | Carga manual sin lanzar las etapas de cálculo |

Las funciones están separadas, pero el motor calcula las bases y los índices
que usan los mismos datos en una lectura compartida. No es necesario releer
cada GeoTIFF para calcular CDD, DRD y P95 por separado.

```text
global_tables/<run_id>/
  run_signature.rds           Configuración y huellas del código
  sessionInfo.txt             Versión de R, paquetes y plataforma
  inventory.rds              Archivo/fecha/tamaño/fecha de modificación
  grids/                     Geometrías y límites de los bloques
  reference/prec/             Umbrales P95 por celda y mes
  reference/tmax/             Climatología diaria por celda
  precipitation/year=1981/    Bases mensuales + CDD, DRD, P95
  temperature/year=1981/      Bases mensuales + FRD
  hwd/year=1981/              Días/eventos/longitud de olas de calor
  static_aligned/            Cuatro rásteres estáticos, si se activa FLD
  static/                    Clases estáticas en tablas
  fld_thresholds/            Cuantiles de lluvia mensual para FLD
  fld/year=1981/              Susceptibilidad anual por mes
  classification/<fuente>/<índice>/  Valores y categorías anuales 1–10
  thresholds/<fuente>/       P10, P20… P90 por celda y mes
  summary/<fuente>/          Promedios por periodo/ENOS y magnitudes
  checkpoints/               Marcadores de trabajos terminados
  parallel_logs/             Registros por trabajador y tiempos por bloque
```

Los archivos se nombran por bloque. No se guardan tablas diarias permanentes.
La referencia utiliza RDS comprimido para matrices; las salidas mensuales usan
Parquet comprimido con Zstandard o RDS, según configuración. Un archivo por año
y bloque reúne los doce meses; no se genera un archivo por celda o por día.
Los bloques completamente sin datos producen tablas vacías o ningún resumen.

Cada marcador se escribe después de completar las salidas correspondientes.
Al reiniciar se omiten trabajos terminados; si falla un bloque a mitad de camino,
se vuelve a procesar. Temperatura retoma por bloque completo, porque las olas de
calor mantienen estado entre años. No borrar manualmente salidas conservando
sus marcadores. Si se dañan o se eliminan, borrar también el marcador afectado.
Los archivos `.partial` de un fallo se pueden eliminar; se vuelven a escribir.

La firma evita mezclar una configuración científica/código diferente con hitos existentes.
Cambiar solo `parallel` (trabajadores/memoria/hilos) no invalida los hitos;
`config.R` se valida por su objeto `cfg`, sin depender de espacios/comentarios.
Usar un nuevo `run_id` después de cambiarlos. El inventario detecta cambios de
ruta, fecha, tamaño y modificación de las fuentes; no calcula hashes de todos
los GeoTIFF ni protege frente a cambios que conserven exactamente esos metadatos.
No modificar las fuentes durante una ejecución. Las capas estáticas y el CSV
ENOS también se consideran entradas de la corrida.

## Esquema espacial y unidades

La clave de las tablas anuales es `(grid_id, cell_id, year, month)`.
`cell_id` es el número de celda de terra, empezando en 1, de izquierda a derecha
y desde la fila norte. Las coordenadas se reconstruyen con la geometría de
`grids/` y `terra::xyFromCell()`; no se repiten en cada fila de cada año.

CHIRPS y CHIRTS pueden tener distinta extensión latitudinal. Se conservan ambas:
`grid_id = prec` y `grid_id = temperature`. `temperature.rds` es un alias de la
geometría Tmax; Tmin debe coincidir con ella. **No unir ambas fuentes solamente
por cell_id.** El empaquetamiento posterior deberá unir por coordenadas o por
una malla destino explícita.

CHIRPS está en mm/día; su suma mensual es mm/mes. CHIRTS-ERA5 se lee directamente
en °C, sin restar 273.15. `tmax_mean_c` es la media de las máximas diarias;
`tmin_mean_c`, la media de las mínimas. `tmean_est_c` es la media mensual de
`(Tmax + Tmin)/2`, una aproximación, no una temperatura media horaria observada.
Una malla 0.05° no equivale a celdas métricas exactas de 5 × 5 km.

## Criterios científicos explícitos

- **CDD:** máximo número consecutivo de días con lluvia <1 mm dentro del mes.
  Se reinicia cada mes para conservar ese criterio del script antiguo.
- **DRD:** total mensual de días con lluvia <1 mm. Usa 28/29/30/31 días reales.
- **P95:** porcentaje de días húmedos (>=1 mm) que superan estrictamente el
  percentil 95 de los días húmedos históricos de la misma celda y mes. Percentil
  empírico tipo 7, mínimo 30 días húmedos de referencia. Si no hay días húmedos
  en el mes evaluado, el porcentaje es NA. No es R95pTOT ni porcentaje de volumen.
  **Cambio metodológico:** el script antiguo intentaba un P95 móvil de cinco
  días del calendario; esta implementación usa un P95 mensual explícito.
- **FRD:** días con Tmin <0 °C; no se aplica conversión a Kelvin.
- **HWD:** episodio de al menos seis días consecutivos con Tmax mayor que la
  climatología diaria +5 °C. La climatología es la media por día del calendario
  entre años de referencia, suavizada con cinco días circulares, manteniendo
  29 de febrero. Es un indicador definido aquí; no se presenta como WSDI.
  Se guardan `hwd_days`, `hwd_events` y `hwd_longest_days`. Los días se atribuyen
  al mes donde ocurren, el evento al mes donde comienza, y la longitud completa
  del episodio a cada mes que toca. Esa longitud puede superar los días del mes.
  Episodios en los extremos del periodo solo cuentan con los días observados;
  no se infiere su continuación fuera de la serie. Fechas discontinuas cortan.
- **FLD:** 40% clase de precipitación mensual +15% de cada clase de arcilla,
  pendiente, humedad topográfica y cobertura. Escala continua 1–5, sin sustituir
  capas faltantes ni renormalizar sus pesos. No simula caudales ni inundaciones.
  **Cambio metodológico:** se calcula para cada año/mes y luego se promedian los
  índices; el código antiguo clasificaba primero la lluvia promedio por ENOS.

Por defecto se exige el 100% de observaciones válidas del mes para publicar sus
bases e índices. NoData, no finitos y valores <=-999 se convierten a NA. La lluvia
negativa se considera inválida. Temperaturas fuera de [-100,70] °C detienen el
proceso para revisar unidades; Tmin > Tmax también lo detiene.
Los conteos `*_nvalid` permiten auditar la cobertura. Si se relaja la cobertura,
no se imputan días: NA corta las rachas y los acumulados son parciales.

Las referencias y resúmenes requieren al menos 90% de observaciones/años de la
selección correspondiente, y los resúmenes al menos tres años válidos. Son
parámetros configurables. Si faltan datos, se guarda NA, nunca una falsa clase.

## Magnitudes y comparación temporal

Para **todos los índices**, incluido DRD y FLD, se calculan los nueve percentiles
**P10, P20, P30, P40, P50, P60, P70, P80 y P90** de los valores anuales de
`reference_years`, de forma independiente para **cada celda y mes**. Se usa el
percentil empírico tipo 7 de R, sin asumir normalidad ni usar desviaciones estándar.
No se mezclan celdas del bloque, continentes ni países para estimar los límites.
Los mismos límites se aplican a todos los años, periodos y condiciones ENOS.

| Nivel | Intervalo |
|---|---|
| 1 | valor ≤ P10 |
| 2 | P10 < valor ≤ P20 |
| 3–9 | intervalos sucesivos hasta P90 |
| 10 | valor > P90 |

Los ceros participan como valores válidos: por ahora no se usa un método
especial para heladas/olas de calor. Empates quedan en el nivel inferior.
Los límites repetidos pueden dejar niveles vacíos; no se añade ruido ni se
fuerza a distribuir un 10% de observaciones en cada nivel. Una serie siempre
cero queda en nivel 1. Datos o referencias insuficientes producen categoría NA.
Con 30 años los deciles se estiman con pocas observaciones: diez niveles no
implican diez niveles de certeza estadística.

Se guardan tres productos:

1. **`thresholds/<fuente>/`**: `grid_id`, `cell_id`, `month`, `metric`,
   `n_reference` y nueve columnas `p10`…`p90`.
2. **`classification/<fuente>/<índice>/`**: valor físico y `magnitude` 1–10 para
   cada celda, año y mes. Archivo por bloque y mes; contiene todos los años.
3. **`summary/<fuente>/`**: conserva el promedio físico `value` por periodo y
   ENOS y su categoría `magnitude` respecto a la distribución de años individuales.
   Añade `n_classified`, `class_1_n`…`class_10_n` y
   `class_1_pct`…`class_10_pct`: conteos y porcentajes de **años clasificados**
   que caen en cada nivel. Estos porcentajes no son percentiles del promedio.
   Los conteos se conservan para auditoría; si no hay suficientes años
   clasificados, los porcentajes son NA. No se estiman probabilidades futuras.

Se mantiene la escala interna 1–5 de los componentes estáticos y de la lluvia
para **construir** `fld_score`, usando `flood_rain_probs` exclusivamente para ese
componente. Esto conserva los pesos y el valor del índice compuesto existente.
Su **clasificación final**, como la de todos los demás índices, usa P10–P90
por celda/mes y produce niveles 1–10; ya no se obtiene redondeando `fld_score`.

Los niveles expresan posición respecto a la historia local, no riesgo agrícola
validado ni umbrales físicos comunes entre regiones. Los valores físicos se
conservan para esas comparaciones. Las variables base de temperatura y lluvia
reciben también el mismo método, pero no se interpretan automáticamente como daño.

Este cambio añade salidas anuales y frecuencias: requiere un `run_id` nuevo.
No mezclar los hitos ni archivos de la versión anterior de cinco categorías.

## Capas estáticas para FLD

`flood_enabled = FALSE` es deliberado hasta disponer de cuatro fuentes globales:

- `clay`: porcentaje de arcilla 0–100, con una profundidad definida por el usuario.
- `slope`: pendiente en porcentaje, calculada previamente a partir del DEM.
- `wetness`: índice de humedad topográfica compatible con la escala SAGA usada.
- `landcover`: cobertura reclasificada previamente a susceptibilidad 1–5.

El script original usaba una capa africana de arcilla; no permite FLD global.
Tampoco puede deducirse una reclasificación hidrológica válida de cualquier
leyenda de cobertura. Preparar esas entradas explícitamente y documentar sus
fuentes/profundidades. El módulo verifica extensión, unidades/rangos y alinea
las capas continuas mediante promedio, la cobertura mediante vecino más cercano.
La extensión global no garantiza cobertura válida en cada celda; donde una capa
sea NA, FLD será NA. No se rellenan huecos con valores inventados.

## Rendimiento y alcance de las pruebas

El tamaño de RAM se limita con `tile_nrow` y `tile_ncol`. La referencia P95 lee
un mes de todos los años de referencia de un bloque; la temperatura mantiene
estadísticas de calendario y contadores mensuales. Con 64×256 celdas y 41 años,
la matriz de lluvia de referencia de un mes tiene aproximadamente 170 MB en
doble precisión, antes de copias. Los máximos reales serán mayores. Reducir el
bloque si falta RAM. Hay miles de bloques globales: una corrida sigue siendo
costosa y genera muchas tablas. No se promete un tiempo sin medir el equipo y X:.

Las referencias y el cálculo anual son fases distintas: las fuentes se leen una
vez para construir referencias y otra para calcular índices. Los resúmenes leen
las tablas anuales por mes; se prioriza limitar RAM, a costa de releerlas.

```text
Rscript tests/test_indices.R
Rscript tests/test_pipeline.R
Rscript tests/test_parallel.R
```

Las pruebas comprueban rachas, bisiestos, percentiles, NoData, empates, continuidad
de olas de calor y una integración con GeoTIFF sintéticos, grillas distintas,
los seis índices, tablas RDS y reanudación. Se verifica además la clasificación
por deciles de DRD y FLD, la independencia entre celdas, los niveles anuales,
las frecuencias por ENOS y el tratamiento de grupos sin años válidos. La máquina de desarrollo dispone de
R 4.5.3 y terra; se ejecuta esa prueba sin el control de versión exclusivamente
en tests. La ejecución normal exige R >=4.6.0. No se ha ejecutado con los datos
de X:, ni se ha verificado aquí la escritura Parquet o R 4.6 en Windows.
La prueba SOCK adicional compara las tablas secuenciales y paralelas, verifica
reanudación/errores recuperables y ejecuta un benchmark con 1/2/4 trabajadores.

Referencias de las APIs utilizadas:
- https://rspatial.github.io/terra/reference/readwrite.html
- https://arrow.apache.org/docs/r/reference/write_parquet.html

No incluye todavía agregación administrativa, recortes continentales,
cartografía, exposición ni estimación de pérdidas.
