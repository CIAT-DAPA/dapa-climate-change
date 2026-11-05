# R >= 4.6.0. Edite rutas y periodos ANTES de crear una nueva ejecución.
cfg <- list(
  schema = 'yapu-global-v2-deciles',
  roots = list(
    prec = 'X:/Observed_Climate data/CHIRPS_V3/CHIRPS_V3_ERA5_Daily',
    tmax = 'X:/Observed_Climate data/CHIRTS_ERA5/tmax_daily_1981_2026',
    tmin = 'X:/Observed_Climate data/CHIRTS_ERA5/tmin_daily_1981_2026'
  ),
  output = 'F:/yapu_climate_risk/global_tables',
  run_id = '1981_2025_deciles_v2',
  years = 1981:2025,
  reference_years = 1991:2020,
  periods = list(
    historical = 1981:2025,
    earlier = 1981:2000,
    recent_past = 2001:2025
  ),
  # Bloques ESPACIALES, no recortes por país. ~16 384 celdas por bloque.
  tile_nrow = 64L, tile_ncol = 256L,
  storage = 'parquet', # requiere arrow; 'rds' permite tablas R sin arrow.
  wet_mm = 1, frost_c = 0,
  heat_delta_c = 5, heat_min_days = 6L, climatology_window = 5L,
  # Política conservadora: un mes incompleto no produce índices ni bases.
  min_valid_fraction = 1,
  min_reference_fraction = 0.9,
  min_wet_reference_days = 30L,
  min_enso_years = 3L,
  # Clasificación FINAL uniforme de todos los índices: diez niveles por celda/mes.
  class_probs = seq(0.1, 0.9, by = 0.1),
  # Solo componente interno de lluvia para construir FLD en su escala física 1..5.
  # La clasificación FINAL de fld_score también usa class_probs (P10..P90).
  flood_rain_probs = c(0.159, 0.309, 0.691, 0.841),
  # CSV normalizado: year,month,oni. Sin CSV, solo se resume condición 'all'.
  enso_csv = 'F:/yapu_climate_risk/enso_monthly.csv',
  # Índice compuesto opcional. Las cuatro capas deben cubrir la malla CHIRPS.
  # clay: % arcilla; slope: pendiente en %; wetness: índice SAGA;
  # landcover: códigos ya reclasificados a susceptibilidad 1..5.
  flood_enabled = FALSE,
  static = list(clay = '', slope = '', wetness = '', landcover = ''),
  # Para ensayos: NULL procesa TODO. Un vector de IDs limita bloques.
  tile_ids = NULL,
  tempdir = 'F:/yapu_climate_risk/terra_tmp'
)
