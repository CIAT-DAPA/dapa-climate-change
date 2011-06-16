#!/bin/bash

#Calculate all final metrics for the weather station density Gaussian kernel:
#1. mask the raster, calculate quartiles, reclassify in quartiles
#2. calculate amount of area in pixels and hectares that is below the 1st quartile
#3. import altitude if it does not exist
#4. reclassify altitude every 100 meters
#5. extract average values of densities for these ranges, for the three variables
#6. import latitude if it does not exist
#7. reclassify latitude every 5 degree
#8. extract average values of densities for these ranges, for the three variables (rain, tean, dtr)

#Locations
DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/density-map
ZIP_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/mask-srtm

#####PRELIMINARIES
#Convert a vector layer of country polygons to raster, if it does not exist
eval `g.findfile element=cell file=ccafs_adm0_grd`
if [ ! $file ]
then
	v.to.rast input=ccafs_adm0 output=ccafs_adm0_grd use=cat type=area layer=1 rows=4096
fi

#Import altitude raster if it does not exist
eval `g.findfile element=cell file=alt`
if [ ! $file ]
then
	unzip $ZIP_PATH/mask-srtm.zip altitude.asc -d $ZIP_PATH
	r.in.gdal -o input=$ZIP_PATH/altitude.asc output=alt
	rm $ZIP_PATH/altitude.asc
fi

#Import latitude raster if it does not exist
eval `g.findfile element=cell file=lat`
if [ ! $file ]
then
	unzip $ZIP_PATH/mask-srtm.zip altitude.asc -d $ZIP_PATH
	r.in.gdal -o input=ccafs_adm0 output=ccafs_adm0_grd
	rm $ZIP_PATH/altitude.asc
fi

#define input raster
IN_RAST=dtr_kernel_sd1_0
OUT_FILE=$IN_PATH/country_areas.csv
ALT_INT=100
LAT_INT=5
echo "COUNTRY,VALUE,AREA.M2" > $OUT_FILE

#masking the raster into a temporal, first delete temporal
eval `g.findfile element=cell file=temp`
if [ $file ]
then
	g.remove rast=temp
fi
r.mapcalc "temp=$IN_RAST"

#Calculate quantiles for the specified raster
QLIST=$(r.quantile -r input=temp quantiles=4)
g.remove rast=temp

#Now calculate the amount of area that is below 1st quartile...
#Extract values of lower limit, upper limit
Q1=$(echo $QLIST | awk '{gsub(":"," ",$1); print}')
Q1_LL=$(echo $Q1 | awk '{print $1}')
Q1_UL=$(echo $Q1 | awk '{print $2}')

#Now set everything else (than the lowest quartile) to null in temporal raster
r.mapcalc "temp=if($IN_RAST<=$Q1_UL,1,null())"

#List values in countries raster
VALUES=$(r.describe -1n map=ccafs_adm0_grd)

#Loop with country values and cut to every country to calculate area that is below 1st quartile
echo "Calculating country values"
for COUNTRY in $VALUES
do
	#Isolate country
	r.mapcalc "tempc=if(ccafs_adm0_grd==$COUNTRY,temp,null())"
	#For each region report the area that equals to 1
	AREAS=$(r.stats -a input=tempc fs=":")
	for sv in $AREAS
	do
		VAL=$(echo $AREAS | awk '{print substr($1,1,1)}')
		AREA=$(echo $AREAS | awk '{print substr($1,3)}')
		echo $COUNTRY,$VAL,$AREA >> $OUT_FILE
	done
done

#Checking the range in altitude raster
GRD_RANGE=$(r.describe -n map=alt)
GRD_RANGE=$(echo $GRD_RANGE | awk '{gsub("thru","",$0);print}')
GRD_LL=$(echo $GRD_RANGE | awk '{print $1}')
GRD_UL=$(echo $GRD_RANGE | awk '{print $NF}')

RG_LENGTH=$(echo "($GRD_UL - $GRD_LL)" | bc)
NINT=$(echo "scale=0;$RG_LENGTH/$ALT_INT" | bc)

#output file 
OUT_FILE=$IN_PATH/alt_rule.txt

#calculating intervals and creating the rule file
for INT in {1..$NINT}
do
	if [ $INT == 1 ]
	then
		LL=$(echo "scale=0;$GRD_LL" | bc)
		UL=$(echo "scale=0;$GRD_LL+$ALT_INT" | bc)
	else
		LL=$(echo "scale=0; $UL+1"| bc)
		UL=$(echo "scale=0;$LL+$ALT_INT" | bc)
	fi
	
	if [ $INT == $NINT ]
	then
		UL=$GRD_UL
	fi
	
	echo "Writing interval $INT ($LL to $UL)"
	echo $LL thru $UL = $INT >> $OUT_FILE
done


