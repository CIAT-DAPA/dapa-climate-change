#!/bin/bash

# Start GRASS shell up at location and mapset
# first argument in command line) is c_2000_current | a2_2050_ensemble | a1b_2050_ensemble

# loop through locations (i.e. c_2000_current)
# create a new mapset in each location named summary
# loop through mapsets (folders with s*) and summarise the stuff using the richness@s* in r.mapcalc
# if current, only summarise richness else summarise everything
# at the end of script, loop through the future ones and calculate turnover, 
# percent change in richness and absolute

# easy!

for location in c_2000_current a2_2050_future a1b_2050_future
do
	
done