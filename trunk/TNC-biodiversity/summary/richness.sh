# shell

for i in *.tif
do
 base=$(echo $i | sed 's/.tif//')
 gdalbuildvrt $base.vrt $i
 gdalwrap 
 done
 
 ogr2ogr -f "ESRI Shapefile" mydata.shp PG:"host=localhost user=model1 dbname=gisdb" -sql "select * from convexhulls limig 1"
 
  ogr2ogr -f "ESRI Shapefile" mydata3.shp PG:"user=model1 dbname=gisdb" -sql "select * from convexhulls where speciesid='13649533'"
  
   curl "http://svn.osgeo.org/gdal/trunk/gdal/swig/python/scripts/gdal_calc.py" > gdal_calc.py
  
  
  
  
   gdalwarp -cut_to_line PG:"user=model1 dbname=gisdb" -sql "select * from convexhulls where speciesid='13649533'" 13649533.tif c13649533.tif
   
   gdalwarp -cut_to_line mydata3.shp 13649533.tif c13649533.tif
   
   gdal_calc -A c13649533.tif --calc "A>100" --outfile=results.tif --NoDataValue=0
   
	r.reclass 
	
	mkdir rc
	echo "0 thru 100 = 0
100 thru 1000 = 1 " > rc/rast.rc

v.in.ogr dsn=PG:dbname=gisdb host=localhost user=model1 output=chull where="select * from chull where geom is not null limit 1"

##########################################################################
# In one location
# provide name of the run as [region]_[res]_[sres]_[year]_[gcm]
# run in the folder of the run!
# 
# 1. Create a location for each run
# 2. Create a mapset for each 4digits code of species. One with original croped maps and one with thresholded
# 3. Symlink the current to the futur so we can calcualte turnovers etc. 

run=$1

# Copy the location
cp /GRASS/locations/template /GRASS/locations/$run

# Start GRASS shell up at location and mapset

# specify proper variables here
/usr/bin/grass -text 

g.region latinamerica@PERMANENT

# do for each folder seperately
ls | while read folder
do
	# go to that folder
	cd $folder
	
	# folder for all the vrts
	mkdir vrts
	
	# folder for all the reclassification rule
	mkdir rc.tables
	
	# mkdir for errors
	mkdir errors
	
	# mdri reclassed rasters
	mkdir cut.vrts
	
	# make new mapset
	g.mapset -c mapset=$run
	g.region latinamerica@PERMANENT
	
	g.mapset -c mapset=$folder.th
	g.region latinamerica@PERMANENT
	
	g.mpaset -c mapset=summary
	g.region latinamerica@PERMANENT


	# cycle through folder
	for subf in *.tif
	do
		base=$(echo $subf | sed 's/.tif//')
		# build virtual
		gdalbuildvrt vrts/$base.vrt $base.tif
		
		# cut virtual with chull
		# ogr2ogr -f "ESRI Shapefile" $base.shp PG:"user=model1 dbname=gisdb" -sql "select * from convexhulls where speciesid='$base'"
		# gdalwarp -q -cutline vrts/$base.shp vrts/$base.vrt c$base.vrt
		gdalwarp -cutline "PG:dbname=gisdb user=model1" -csql "select ST_Buffer(geom,3) from convexhulls where speciesid='$base'" -q $base.tif cut.vrts/$base.tif

		
		# get value for reclass
		threshold=$(psql -U model1 -d gisdb -t -c "select threshold from models where rundid=$run and speciesid=$base")
		
		# register with r.external in grass
		g.mapset mapset=$run.$base
		r.external in=vrts/$base.vrt out=$base -o

		# write reclass table
		echo "0 thru $threshold = 0 
		$threshold thru 300 = 1 " > rc.tables/$base.rc
		
		# reclass
		g.mapset mapset=$folder.th
		r.reclass in=$base@$folder out=$base

		# add reclassed together
		g.mapset mapset=$run.$mapset
		

# export added raster