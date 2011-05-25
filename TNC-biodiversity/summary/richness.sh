##########################################################################
# In one location
# provide name of the run as [region]_[res]_[sres]_[year]_[gcm]
# run in the folder of the run!
# 
# 1. Create a location for each run
# 2. Create a mapset for each 4digits code of species. One with original croped maps and one with thresholded
# 3. Symlink the current to the futur so we can calcualte turnovers etc. 
##########################################################################

##########################################################################
# Make templetae location 
#

/usr/bin/grass64 -text

# projection: WGS84
# g.region res=00:02.5 -p
# -----------------------
# rojection: 3 (Latitude-Longitude)
# zone:       0
# datum:      wgs84
# ellipsoid:  wgs84
# north:      35N
# south:      60S
# west:       118W
# east:       33W
# nsres:      0:02:30
# ewres:      0:02:30
# rows:       2280
# cols:       2040
# cells:      4651200

# Create default region for Latin America
g.region save=latinamerica

# Create default summary grids
r.mapcalc template.summary=0

exit


##########################################################################
# Set up the current scenarios and calculate current species richness
#
# 1. make grass location
# 2. cycle through all parts
# 3. for each part cycle through all tifs
#   a. build virtual
#   b. cut convexhull (chull)
#   c. get threshold value
#   d. register them to grass
#   e. reclassify to (2,0) for turnover calculations
#   f. sum in parts
# 4. sum parts
##########################################################################

# Copy the location
cp -r /data/TNC/grass/template /data/TNC/grass/current

# Start GRASS shell up at location and mapset

# specify proper variables here
/usr/bin/grass64 -text /data/TNC/grass/current/PERMANENT

g.region latinamerica@PERMANENT

# Create a mapset that will contain the summaries of each sub folder and a total summary of this run
g.mapset -c mapset=summary
g.region latinamerica@PERMANENT
g.copy rast=template.summary@PERMANENT,richness

# Create mapsets
ls | grep -e "^[0-9]" | while read folder
do
  # create mapset for folder
  g.mapset -c mapset=s$folder --quiet
  g.region latinamerica@PERMANENT
  
  # run processes
  for bufDis in 300000 305000 308000 325000 340000
    do

      # mapset for cut unclassified grids
      g.mapset -c mapset=s$folder.$bufDis --quiet
      g.region latinamerica@PERMANENT

      # mapset for cut classified grids
      g.mapset -c mapset=s$folder.th.$bufDis --quiet
      g.region latinamerica@PERMANENT
  done
done
exit

# Check if function for parallelizing are here
svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/util/bash_parallel.sh /data/TNC/src/bash_parallel.sh
. /data/TNC/src/bash_parallel.sh


# check if the batch bash script is available
svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/summary/batch_current.sh  /data/TNC/src/batch_current.sh 


# allow writing access
chmod u+x /data/TNC/src/batch_current.sh 

####
# Batch wrapper

function runbatch {
  folder=$1  
  export GRASS_BATCH_JOB=/data/TNC/src/batch_current.sh 
  /usr/bin/grass64 /data/TNC/grass/current/s$folder  
  unset GRASS_BATCH_JOB

}



# dir for console output
mkdir err

# parallel
QUEUE=""
MAX_NPROC=10

# run in parallel
ls | grep -e "^[0-9]" | while read folder
do
  # run processes
  runbatch $folder &> err/$folder.txt &

  PID=$!
  queue $PID

  while [ $NUM -ge $MAX_NPROC ] 
  do
    checkqueue
    sleep 0.5
  done
done



# export added raster

################# batch

export GRASS_BATCH_JOB=/data/TNC/src/batch_current.sh 
/usr/bin/grass64 -text /data/TNC/results/grass/current/s$folder


##########################################################################
# FUTUR SRES
###########################################################################

# Copy the location
cp /GRASS/locations/template /GRASS/locations/$run

# Start GRASS shell up at location and mapset

# specify proper variables here
/usr/bin/grass64 -text /data/TNC/grass/test1/PERMANENT

g.region latinamerica@PERMANENT

# Create a mapset that will contain the summaries of each sub folder and a total summary of this run
g.mpaset -c mapset=summary
g.region latinamerica@PERMANENT

# do for each folder seperately
ls | grep "^[0-9]*" | while read folder
do
	# get fold
	cd $folder
	
	# folder for all the vrts
	mkdir vrts
	
	# folder for all the reclassification rule
	mkdir rc.tables
	
	# mkdir for errors
	mkdir errors
	
	# mkdri reclassed rasters
	mkdir cut.vrts
	
	# make new mapset
	g.mapset -c mapset=$folder
	g.region latinamerica@PERMANENT
	
	g.mapset -c mapset=$folder.th
	g.region latinamerica@PERMANENT

	# cycle through folder
	for tif in *.tif
	do
    base=$(echo $tif | sed 's/.tif//')
    # build virtual
    gdalbuildvrt vrts/$base.vrt $base.tif
    
    
    # cut virtual with chull
    # ogr2ogr -f "ESRI Shapefile" $base.shp PG:"user=model1 dbname=gisdb" -sql "select * from convexhulls where speciesid='$base'"
    # gdalwarp -q -cutline vrts/$base.shp vrts/$base.vrt c$base.vrt
    gdalwarp -cutline "PG:dbname=gisdb user=model1" -csql "select ST_Transform(ST_Buffer(ST_Transform(geom,3395),$bufdis),4326) from convexhulls where speciesid='$base'" -q vrts/$base.vrt cut.vrts/$base.vrt
    
    # get value for reclass
    threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")
		
    # register with r.external in grass
    g.mapset mapset=$folder
    r.external in=cut.vrts/$base.vrt out=$base -o

    # write reclass table
    # use the -e to make a new line
    echo -e "0 thru $threshold = 0 \n$threshold thru 300 = 1 " > rc.tables/$base.rc

    # reclass
    g.mapset mapset=$folder.th
    r.reclass in=$base@$folder out=$base rules=rc.tables/$base.rc

		# add reclassed together
		g.mapset mapset=summary
		

# export added raster
