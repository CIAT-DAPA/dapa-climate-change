#!/bin/bash

# Batch job, to be started in current directory
# which folder should I work in
folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=' | sed 's/s//')

# This job will import all tifs into the GRASS database for further calculation,
# while at the same time will calculate the binary grid (presence/absence) using the 300km
# convex hull (taken from a postGreSQL database).
# 
# to run, you'd need to go to the folder /data1/TNC/results/grass/lam_5k_c_2000_current/s$NF
# where $NF is the part of data (i.e. 1327, and others), that are present in the folder
# where raw tif projections are /data1/TNC/results/lam_5k_c_2000_current

# go to folder
cd /data1/TNC/results/lam_5k_c_2000_current/$folder

# make folder were vrts are stored
mkdir vrts

# make folder for reclassification tables  
mkdir rc.tables

# make folder for shapefiles and chull tifs
mkdir shps
mkdir chull.tifs

# Cycle through all adapttation scenarios, buffer width in [m]
# 300000 - null
# 306000 - 2030 realistic   60 years a 100m (1975 - 2035)
# 330000 - 2030 optimistic  60 years a 500m
# 308000 - 2050 realistic   80 years a 100m
# 340000 - 2050 optimistic  80 years a 500m

# cycle through each tif
for tif in *.tif
do
		# species id
		base=$(echo $tif | sed 's/.tif//')
		
		# echoing
		echo "importing species $base"
		
		# build gdal virtual grid
		gdalbuildvrt vrts/$base.vrt $base.tif

		# getting threshold value (already rescaled to 1 - 256)
		threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")

		# only one buffer distance is needed (300 km)
		bufDis=300000
		#if [ ! -d "vrts.chull.$bufDis" ]
		#then
		#	mkdir "vrts.chull.$bufDis"
		#fi
		
		# cut to chull according to buffer distance
		# transform to global mercator, perform the buffer of convex hulls and transform back
		# below commented by jramirez
		# gdalwarp -cutline "PG:dbname=gisdb user=model1" -csql "select ST_Transform(ST_Buffer(ST_Transform(geom,3395),$bufDis),4326) from convexhulls where speciesid='$base'" -q vrts/$base.vrt vrts.chull.$bufDis/$base.vrt
		
		# dump the chull from the PG database and import into grass
		pgsql2shp -f shps/$base.shp -u model1 -h andromeda.ciat.cgiar.org -g geom gisdb "select * from convexhulls where speciesid='$base'"
		cp /data1/TNC/env/dummy.tif chull.tifs/$base.tif
		gdal_rasterize -a HASCHULL -l $base shps/$base.shp chull.tifs/$base.tif
		gdalbuildvrt vrts/$base.chull.vrt chull.tifs/$base.tif
		r.in.gdal in=vrts/$base.chull.vrt out=s$base.ch -o
		r.buffer input=s$base.ch output=$base.tmp distances=$bufDis units=meters
		r.mapcalc "$base.bf=if($base.tmp>=0,1,null())"
		r.mapcalc "s$base.bf=if(isnull($base.bf),0,$base.bf)"
		g.mremove rast=$base.tmp,$base.bf -f
		
		# register rasters in GRASS
		r.in.gdal in=vrts/$base.vrt out=s$base.pr -o --o
		#r.in.gdal in=vrts.chull.$bufDis/$base.vrt out=s$base.$bufDis -o --o

		# create reclassification table (0 absence, 1 presence, for the buffer
		#echo -e "0 = 0 \n1 thru 300 = 1 " > rc.tables/$base.b.rc 
		echo -e "0 thru $threshold = 0 \n$threshold thru 255 = 1" > rc.tables/$base.rc

		# reclass
		#r.reclass in=s$base.$bufDis out=s$base.$bufDis.b rules=rc.tables/$base.b.rc --o
		r.reclass in=s$base.pr out=s$base.th.b rules=rc.tables/$base.rc --o
		
		#done

		r.mapcalc "s$base.th=s$base.th.b"
		r.mapcalc "s$base.pa=if(s$base.th==1 & s$base.bf==1,1,0)"

		# remove original maps
		g.remove rast="s$base.th.b" -f
		#g.remove rast="s$base.$bufDis.b" -f
		#g.remove rast="s$base.$bufDis" -f
		
		# rasters produced here are the ones listed below, per species:
		# 
		# s$base.pr = probabilities scaled to 0-255
		# s$base.th = thresholded probabilities (using ROC threshold)
		# s$base.ch = convex hull of species
		# s$base.bf = 300km of convex hull
		# s$base.pa = presence (1) and absence (0) of the species
		#

done

  rm -r vrts
  rm -r rc.tables
  rm -r shps
  rm -r chull.tifs

cd ..
