# Carga manual en la consola de R: no ejecuta referencias ni índices.
source('config.R', encoding='UTF-8')
for (f in sort(list.files('R',pattern='\\.R$',full.names=TRUE)))
  source(f,encoding='UTF-8')
initialize(cfg)
stage_inventory(cfg)
message('Listo. Ejecute stage_reference(cfg) y después las siguientes etapas.')
