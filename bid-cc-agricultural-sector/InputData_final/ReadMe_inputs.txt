Guide to input processing codes:
22 Julio, 2015

1) Mapas Zonas de Cultivo_Monfreda.r:  Carga los datos crudos de Monfreda con rendimientos y areas en 5 minutos.  Agrega a 0.5 grados y calcula diferentes subconjuntos con 4 criterios (95 y 99% de producción y area).  Guarda la información en archivos 08-Cells_toRun/(crop).loc.Rdat.

2) ReadCropCalendar_Sacks_MIRCA_new.r:  carga los datos crudos de Sacks y MIRCA para fechas de siembra.  Para MIRCA selecciona el sistema por pixel con la mayor área.  Actualiza los matrices (crop).loc.cal.Rdat con esta información.

3) Coordenates_addCountry.r:  agrega el pais por cada pixel para todos los pixeles en 08-Cells_toRun/(crop).loc.cal.Rdat. 

4) ReadGGCMI_fertilizers_new.r:  carga los datos originales de N, P, y K2O en 0.5 grado por todo el mundo.  Mapea estos puntos a los puntos de cultivos (la lista grande de Monfreda) y agregan a los matrices (crop).loc.cal.Rdat en 08-Cells_toRun.

5) ReadSPAM_growingAreas_new.r:  carga los datos crudos de área física de SPAM y agrega de 5 minutos a 0.5 grado, junta datos de SPAM con fechas de siembra de MIRCA, escala datos de N para que riego es 2x lo de secano; guarda archivos intermedios se llaman (cultivo)_RS.dat en 09-MIRCA2000/Growing_area_grids/RS/

6) Matrices_porCultivo_new.r:  agrega toda la información de areas de SPAM, aplicaciones de N, FPU, regiones y variedades.  Hace los matrices crop_riego y crop_secano que están usado por las corridas de DSSAT.