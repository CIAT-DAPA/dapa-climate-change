# Desde la carpeta climate_pipeline:
# Rscript run.R inventory reference precipitation temperature static flood summary
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) args <- c('inventory', 'reference', 'precipitation',
                            'temperature', 'static', 'summary') #'flood', 
source('config.R')
for (f in sort(list.files('R', pattern = '\\.R$', full.names = TRUE))) source(f)
initialize(cfg)
stage_inventory(cfg)
for (stage in args) {
  message('\n=== ', stage, ' ===')
  switch(stage,
    inventory = stage_inventory(cfg),
    reference = stage_reference(cfg),
    precipitation = stage_precipitation(cfg),
    temperature = stage_temperature(cfg),
    static = stage_static(cfg),
    # flood = stage_flood(cfg),
    summary = stage_summary(cfg),
    stop('Etapa desconocida: ', stage))
}
