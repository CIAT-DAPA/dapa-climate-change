#!/bin/bash

#Calculate all final metrics for the weather station density Gaussian kernel:
#1. reclassify in quartiles
#2. calculate amount of area in pixels and hectares that is below the 1st quartile
#3. import altitude if it does not exist
#4. reclassify altitude every 100 meters
#5. extract average values of densities for these ranges, for the three variables
#6. import latitude if it does not exist
#7. reclassify latitude every 5 degree
#8. extract average values of densities for these ranges, for the three variables (rain, tean, dtr)

#Set zeros to null
r.mapcalc "temp=if($IN_RAST==0,null(),$IN_RAST)"

#Calculate quantiles
QLIST=$(r.quantile -r input=temp quantiles=4)
g.remove rast=temp

#Now calculate the amount of area that is below 1st quartile...

#Extract values of lower limit, upper limit
Q1=$(echo $Q | awk '{gsub(":"," ",$1); print}')
Q1_LL=$(echo $Q1 | awk '{print $1}')
Q1_UL=$(echo $Q1 | awk '{print $2}')

#Now set everything else to null in temporal raster
r.mapcalc "temp=if($IN_RAST<=$Q1_UL,null(),1)"

#Cut to every country, to each region and also calculate for whole region


#For each region report the area that equals to 1
r.report map=temp@climate_data units=k,p null=* nsteps=255

#Convert a vector layer of country polygons to raster
v.to.rast input=ccafs_regions_adm0@climate_data output=ccafs_regions_adm0_grd use=cat type=area layer=1 rows=4096 --overwrite

