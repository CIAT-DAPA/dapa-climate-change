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

#define input raster and other parameters
IN_RAST=$2 #dtr_kernel_sd1_0
VAR=$1 #alt
INTERVAL=$3 #100
DOCOUNTRIES=$4

#####PRELIMINARIES
#Convert a vector layer of country polygons to raster, if it does not exist
echo "Converting country vectors to raster"
eval `g.findfile element=cell file=ccafs_adm0_grd`
if [ ! $file ]
then
	v.to.rast input=ccafs_adm0 output=ccafs_adm0_grd use=cat type=area layer=1 rows=4096
fi

#Import *itude raster if it does not exist
echo "Importing $VAR\itude raster, if it does not exist"
eval `g.findfile element=cell file=$VAR`
if [ ! $file ]
then
	unzip $ZIP_PATH/mask-srtm.zip $VAR\itude.asc -d $ZIP_PATH
	r.in.gdal -o input=$ZIP_PATH/$VAR\itude.asc output=$VAR
	rm $ZIP_PATH/$VAR\itude.asc
fi

if [ $DOCOUNTRIES == T ]
then
	#Creating output file
	OUT_FILE=$IN_PATH/country_areas_$IN_RAST.csv
	echo "COUNTRY,VALUE,AREA.M2" > $OUT_FILE
	
	#masking the raster into a temporal, first delete temporal
	echo "Masking raster into a temporal"
	eval `g.findfile element=cell file=temp`
	if [ $file ]
	then
		g.remove rast=temp
	fi
	r.mapcalc "temp=$IN_RAST"

	#Calculate quantiles for the specified raster
	echo "Calculating quartiles"
	QLIST=$(r.quantile -r input=temp quantiles=4)
	g.remove rast=temp

	#Now calculate the amount of area that is below 1st quartile...
	#Extract values of lower limit, upper limit
	Q1=$(echo $QLIST | awk '{gsub(":"," ",$1); print}')
	Q1_LL=$(echo $Q1 | awk '{print $1}')
	Q1_UL=$(echo $Q1 | awk '{print $2}')

	#Now set everything else (than the lowest quartile) to null in temporal raster
	echo "Calculating mask below the 1st quartile"
	r.mapcalc "temp=if($IN_RAST<=$Q1_UL,1,null())"

	#List values in countries raster
	VALUES=$(r.describe -1n map=ccafs_adm0_grd)
	
	#Loop with country values and cut to every country to calculate area that is below 1st quartile
	echo "Calculating country values"
	for COUNTRY in $VALUES
	do
		echo "Computing $COUNTRY"
		#Remove temporal country raster if exists
		eval `g.findfile element=cell file=tempc`
		if [ $file ]
		then
			g.remove rast=tempc
		fi
		
		#Isolate country
		r.mapcalc "tempc=if(ccafs_adm0_grd==$COUNTRY,temp,null())"
		
		#For each region report the area that equals to 1
		AREAS=$(r.stats -a input=tempc fs=":")
		for sv in $AREAS
		do
			VAL=$(echo $sv | awk '{print substr($1,1,1)}')
			AREA=$(echo $sv | awk '{print substr($1,3)}')
			echo $COUNTRY,$VAL,$AREA >> $OUT_FILE
		done
	done
fi

#Removing temporal files
eval `g.findfile element=cell file=tempc`
if [ $file ]
then
	g.remove rast=tempc
fi

eval `g.findfile element=cell file=temp`
if [ $file ]
then
	g.remove rast=temp
fi

#Checking if reclassified raster exists
echo "Reclassifying if required"
eval `g.findfile element=cell file=$VAR\_reclass`
if [ ! $file ]
then
	
	#Checking the range in altitude raster
	GRD_RANGE=$(r.describe -n map=$VAR)
	GRD_RANGE=$(echo $GRD_RANGE | awk '{gsub("thru","",$0);print}')
	GRD_LL=$(echo $GRD_RANGE | awk '{print $1}')
	GRD_UL=$(echo $GRD_RANGE | awk '{print $NF}')
	
	if [ $GRD_LL < 0 ]
	then
		GRD_LL=$(echo "scale=0;$GRD_LL-0.5" | bc)
	else
		GRD_LL=$(echo "scale=0;$GRD_LL+0.5" | bc)
	fi
	GRD_LL=${GRD_LL%.*}
	
	if [ $GRD_UL < 0 ]
	then
		GRD_UL=$(echo "scale=0;$GRD_UL-0.5" | bc)
	else
		GRD_UL=$(echo "scale=0;$GRD_UL+0.5" | bc)
	fi
	GRD_UL=${GRD_UL%.*}
	
	RG_LENGTH=$(echo "($GRD_UL - $GRD_LL)" | bc)
	NINT=$(echo "scale=0;$RG_LENGTH/$INTERVAL" | bc)
	
	echo "Looping $NINT in a range of $RG_LENGTH between $GRD_LL and $GRD_UL"
	
	#output file 
	OUT_FILE=$IN_PATH/$VAR\_rule.txt

	#calculating intervals and creating the rule file
	for (( INT=1; INT <= $NINT; INT++ ))
	do
		if [ $INT == 1 ]
		then
			LL=$(echo "scale=0;$GRD_LL" | bc)
			UL=$(echo "scale=0;$GRD_LL+$INTERVAL" | bc)
		else
			LL=$(echo "scale=0; $UL+1"| bc)
			UL=$(echo "scale=0;$LL+$INTERVAL" | bc)
		fi
		
		if [ $INT == $NINT ] && [ $UL < $GRD_UL ]
		then
			UL=$GRD_UL
		fi
		
		echo "Writing interval $INT ($LL to $UL)"
		echo $LL thru $UL = $INT >> $OUT_FILE
	done

	#Now reclassify the raster
	r.reclass input=$VAR output=$VAR\_reclass rules=$IN_PATH/$VAR\_rule.txt --overwrite
fi

#Now calculating the average values within each altitude class
echo "Calculating average values for altitude classes"

r.mapcalc "temp=int($IN_RAST*1000)"
r.statistics  base=$VAR\_reclass cover=temp method=average output=$IN_RAST\_$VAR\_zonal --overwrite
r.category map=$IN_RAST\_$VAR\_zonal > $IN_PATH/$IN_RAST\_$VAR\_classes.txt

eval `g.findfile element=cell file=temp`
if [ $file ]
then
	g.remove rast=temp
fi
