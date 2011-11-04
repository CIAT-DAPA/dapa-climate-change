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
	# define initial stuff
	MODEL=lam_5k_$location
	GISDB_PATH=/data1/TNC/results/grass
	
	# create summary folder if it does not exist
	if [ ! -d "$GISDB_PATH/$MODEL/summary" ]
	then
		echo "Creating mapset for summary"
		cp -r $GISDB_PATH/$MODEL/tmp $GISDB_PATH/$MODEL/summary
	fi
	
	# change to summary mapset
	g.mapset summary location=$MODEL
	
	if [ $location == "c_2000_current" ]
	then
		r.mapcalc "richness=0"
	else
		r.mapcalc "richness.um=0"
		r.mapcalc "richness.nm=0"
		r.mapcalc "richness.re=0"
		r.mapcalc "richness.op=0"
		r.mapcalc "gain.um=0"
		r.mapcalc "gain.re=0"
		r.mapcalc "gain.op=0"
		r.mapcalc "loss.um=0"
		r.mapcalc "loss.nm=0"
		r.mapcalc "loss.re=0"
		r.mapcalc "loss.op=0"
	fi
	
	# loop through mapsets
	while read folder
	do
		echo "calculating for s$folder"
		if [ $location == "c_2000_current" ]
		then
			# calculate richness
			eval `g.findfile element=cell file=richness@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "richness.tmp=richness+richness@s$folder"
			g.remove rast=richness -f
			r.mapcalc "richness=richness.tmp"
			g.remove rast=richness.tmp
		else
			# calculate richness, migration scenarios
			eval `g.findfile element=cell file=richness.um@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "richness.um.tmp=richness.um+richness.um@s$folder"
			g.remove rast=richness.um -f
			r.mapcalc "richness.um=richness.um.tmp"
			g.remove rast=richness.um.tmp
			
			eval `g.findfile element=cell file=richness.nm@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "richness.nm.tmp=richness.nm+richness.nm@s$folder"
			g.remove rast=richness.nm -f
			r.mapcalc "richness.nm=richness.nm.tmp"
			g.remove rast=richness.nm.tmp
			
			eval `g.findfile element=cell file=richness.re@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "richness.re.tmp=richness.re+richness.re@s$folder"
			g.remove rast=richness.re -f
			r.mapcalc "richness.re=richness.re.tmp"
			g.remove rast=richness.re.tmp
			
			eval `g.findfile element=cell file=richness.op@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "richness.op.tmp=richness.op+richness.op@s$folder"
			g.remove rast=richness.op -f
			r.mapcalc "richness.op=richness.op.tmp"
			g.remove rast=richness.op.tmp
			
			# calculate gain
			eval `g.findfile element=cell file=gain.um@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "gain.um.tmp=gain.um+gain.um@s$folder"
			g.remove rast=gain.um -f
			r.mapcalc "gain.um=gain.um.tmp"
			g.remove rast=gain.um.tmp
			
			eval `g.findfile element=cell file=gain.re@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "gain.re.tmp=gain.re+gain.re@s$folder"
			g.remove rast=gain.re -f
			r.mapcalc "gain.re=gain.re.tmp"
			g.remove rast=gain.re.tmp
			
			eval `g.findfile element=cell file=gain.op@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "gain.op.tmp=gain.op+gain.op@s$folder"
			g.remove rast=gain.op -f
			r.mapcalc "gain.op=gain.op.tmp"
			g.remove rast=gain.op.tmp
			
			# calculate loss
			eval `g.findfile element=cell file=loss.um@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "loss.um.tmp=loss.um+loss.um@s$folder"
			g.remove rast=loss.um -f
			r.mapcalc "loss.um=loss.um.tmp"
			g.remove rast=loss.um.tmp
			
			eval `g.findfile element=cell file=loss.nm@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "loss.nm.tmp=loss.nm+loss.nm@s$folder"
			g.remove rast=loss.nm -f
			r.mapcalc "loss.nm=loss.nm.tmp"
			g.remove rast=loss.nm.tmp
			
			eval `g.findfile element=cell file=loss.re@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "loss.re.tmp=loss.re+loss.re@s$folder"
			g.remove rast=loss.re -f
			r.mapcalc "loss.re=loss.re.tmp"
			g.remove rast=loss.re.tmp
			
			eval `g.findfile element=cell file=loss.op@s$folder`
			if [ ! $file ]
			then
				return 0
			fi
			r.mapcalc "loss.op.tmp=loss.op+loss.op@s$folder"
			g.remove rast=loss.op -f
			r.mapcalc "loss.op=loss.op.tmp"
			g.remove rast=loss.op.tmp
		fi
	done < /data1/TNC/lib/dirlist_all.lst
	
	if [ $location == "c_2000_current" ]
	then
		echo "current richness done successfully, exporting"
		r.out.gdal input=richness format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.asc nodata=-9999 type=Int32
		r.out.gdal input=richness format=GTiff output=/data1/TNC/results/summaries/richness.$location.tif nodata=-9999 type=Int32
	else
		echo "future $location richness done successfully, exporting"
		# link current folder for easy usage of these data
		ln -s $GISDB_PATH/lam_5k_c_2000_current/summary $GISDB_PATH/$MODEL/c_summary
		
		# export rasters, different scenarios
		r.out.gdal input=richness.um format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.um.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.um format=GTiff output=/data1/TNC/results/summaries/richness.$location.um.tif nodata=-9999 type=Int32
		
		r.out.gdal input=richness.nm format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.nm.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.nm format=GTiff output=/data1/TNC/results/summaries/richness.$location.nm.tif nodata=-9999 type=Int32
		
		r.out.gdal input=richness.re format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.re.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.re format=GTiff output=/data1/TNC/results/summaries/richness.$location.re.tif nodata=-9999 type=Int32
		
		r.out.gdal input=richness.op format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.op.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.op format=GTiff output=/data1/TNC/results/summaries/richness.$location.op.tif nodata=-9999 type=Int32
		
		# calculate abs change in richness, different scenarios
		r.mapcalc "richness.abs.chg.um=richness.um-richness@c_summary"
		r.out.gdal input=richness.abs.chg.um format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.abs.chg.um.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.abs.chg.um format=GTiff output=/data1/TNC/results/summaries/richness.$location.abs.chg.um.tif nodata=-9999 type=Int32
		
		r.mapcalc "richness.abs.chg.nm=richness.nm-richness@c_summary"
		r.out.gdal input=richness.abs.chg.nm format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.abs.chg.nm.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.abs.chg.nm format=GTiff output=/data1/TNC/results/summaries/richness.$location.abs.chg.nm.tif nodata=-9999 type=Int32
		
		r.mapcalc "richness.abs.chg.re=richness.re-richness@c_summary"
		r.out.gdal input=richness.abs.chg.re format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.abs.chg.re.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.abs.chg.re format=GTiff output=/data1/TNC/results/summaries/richness.$location.abs.chg.re.tif nodata=-9999 type=Int32
		
		r.mapcalc "richness.abs.chg.op=richness.op-richness@c_summary"
		r.out.gdal input=richness.abs.chg.op format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.abs.chg.op.asc nodata=-9999 type=Int32
		r.out.gdal input=richness.abs.chg.op format=GTiff output=/data1/TNC/results/summaries/richness.$location.abs.chg.op.tif nodata=-9999 type=Int32
		
		# calculate rel change in richness, different scenarios
		r.mapcalc "richness.rel.chg.um=float(richness.abs.chg.um)/float(richness@c_summary+1)"
		r.out.gdal input=richness.rel.chg.um format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.rel.chg.um.asc nodata=-9999
		r.out.gdal input=richness.rel.chg.um format=GTiff output=/data1/TNC/results/summaries/richness.$location.rel.chg.um.tif nodata=-9999
		
		r.mapcalc "richness.rel.chg.nm=float(richness.abs.chg.nm)/float(richness@c_summary+1)"
		r.out.gdal input=richness.rel.chg.nm format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.rel.chg.nm.asc nodata=-9999
		r.out.gdal input=richness.rel.chg.nm format=GTiff output=/data1/TNC/results/summaries/richness.$location.rel.chg.nm.tif nodata=-9999
		
		r.mapcalc "richness.rel.chg.re=float(richness.abs.chg.re)/float(richness@c_summary+1)"
		r.out.gdal input=richness.rel.chg.re format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.rel.chg.re.asc nodata=-9999
		r.out.gdal input=richness.rel.chg.re format=GTiff output=/data1/TNC/results/summaries/richness.$location.rel.chg.re.tif nodata=-9999
		
		r.mapcalc "richness.rel.chg.op=float(richness.abs.chg.op)/float(richness@c_summary+1)"
		r.out.gdal input=richness.rel.chg.op format=AAIGrid output=/data1/TNC/results/summaries/richness.$location.rel.chg.op.asc nodata=-9999
		r.out.gdal input=richness.rel.chg.op format=GTiff output=/data1/TNC/results/summaries/richness.$location.rel.chg.op.tif nodata=-9999
		
		# calculate turnover, different scenarios
		# eq. 100 * ((gain+loss)/(richness+gain))
		r.mapcalc "turnover.um=100*(float(gain.um+loss.um)/float(richness@c_summary+gain.um))"
		r.out.gdal input=turnover.um format=AAIGrid output=/data1/TNC/results/summaries/turnover.$location.um.asc nodata=-9999
		r.out.gdal input=turnover.um format=GTiff output=/data1/TNC/results/summaries/turnover.$location.um.tif nodata=-9999
		
		r.mapcalc "turnover.re=100*(float(gain.re+loss.re)/float(richness@c_summary+gain.re))"
		r.out.gdal input=turnover.re format=AAIGrid output=/data1/TNC/results/summaries/turnover.$location.re.asc nodata=-9999
		r.out.gdal input=turnover.re format=GTiff output=/data1/TNC/results/summaries/turnover.$location.re.tif nodata=-9999
		
		r.mapcalc "turnover.op=100*(float(gain.op+loss.op)/float(richness@c_summary+gain.op))"
		r.out.gdal input=turnover.op format=AAIGrid output=/data1/TNC/results/summaries/turnover.$location.op.asc nodata=-9999
		r.out.gdal input=turnover.op format=GTiff output=/data1/TNC/results/summaries/turnover.$location.op.tif nodata=-9999
		
	fi
done

