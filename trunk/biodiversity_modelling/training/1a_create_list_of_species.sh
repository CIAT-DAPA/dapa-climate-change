#!/bin/bash

# Create a list of species (gbif species id) to be consiered in a run

psql -U model1 -h 192.168.20.228 -d gisdb -t -c "select distinct(speciesid) from points, latinamerica where points.speciesid in (select speciesid from points group by speciesid having count(speciesid) > 9) and points.geom && latinamerica.the_geom and ST_Contains(latinamerica.the_geom, points.geom)" > data/species/species_lists/run0.txt
