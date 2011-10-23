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

function renameRuns {

  # new location

  NLOCATION=$1

  # go to the new location and split folders
  cd /data/TNC/results/$NLOCATION

  if [ -d "part.1" ]
  then
  
    # split each part
    for i in 1 2 3 4 5 6 7 8; 
    do  
      for j in part.$i/*;  
      do
        id=$(echo $j | cut -f2 -d/);   
        nf=$(echo ${id:0:4});   
        if [ -e "$nf" ];   
        then
          mv -v $j $nf;   
        else    
          mkdir $nf;    
          mv -v $j $nf;   
        fi;
      done; 
    done

    rm -r part*
  fi



  # Copy the location
  cp -r /mnt/GIS-HD716/TNC_global_plants/grass/template /data/TNC/grass/$NLOCATION

}

  # Start GRASS shell up at location and mapset
  # specify proper variables here

while read line
do
  g.mapset location=$line mapset=PERMANENT

  g.region latinamerica@PERMANENT

  # Create a mapset that will contain the summaries of each sub folder and a total summary of this run
  g.mapset -c mapset=summary
  g.region latinamerica@PERMANENT
  g.copy rast=template.summary@PERMANENT,richness

  # Create mapsets
  # prefix mapsets with s -> subfolder
  
  cd /data/TNC/results/$line

  ls | grep -e "^[0-9]" | while read folder
  do
    # create mapset for folder
    g.mapset -c mapset=s$folder --quiet
    g.region latinamerica@PERMANENT

  done
done < list.todo


####
# Batch wrapper

function runbatch_current {
  folder=$1  
  export GRASS_BATCH_JOB=/data/TNC/src/batch_current.sh 
  /usr/bin/grass64 /data/TNC/grass/current/s$folder
  unset GRASS_BATCH_JOB

}

function runbatch_future {
  folder=$1  
  NLOCATION=$2
  export GRASS_BATCH_JOB=/data/TNC/src/batch_future.sh 
  /usr/bin/grass64 /data/TNC/grass/$NLOCATION/s$folder  
  unset GRASS_BATCH_JOB

}

function calculateRichness {

  NLOCATION=$1

   # Check if function for parallelizing are here
  svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/util/bash_parallel.sh /data/TNC/src/bash_parallel.sh

  . /data/TNC/src/bash_parallel.sh


  # check if the batch bash script is available (present
  svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/summary/batch_current.sh  /data/TNC/src/batch_current.sh 

  # check if the batch bash script is available (futur)
  svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/summary/batch_future.sh  /data/TNC/src/batch_future.sh 


  # allow writing access
  chmod u+x /data/TNC/src/batch_current.sh 
  chmod u+x /data/TNC/src/batch_future.sh 

  
  # go to the new location
  cd /data/TNC/results/$NLOCATION

  # dir for console output
  if [ ! -d "err" ]
  then
    mkdir err
  fi
  
  # parallel
  QUEUE=""
  MAX_NPROC=22

  # run in parallel
  ls | grep -e "^[0-9]" | while read folder
  do
    # run processes current
    # runbatch_current $folder &> err/$folder.txt &

    # run process future
    echo "..... starting with $NLOCATION $folder ........."
    runbatch_future $folder $NLOCATION &> err/$folder.txt &

    PID=$!
    queue $PID

    while [ $NUM -ge $MAX_NPROC ] 
    do
      checkqueue
      sleep 0.5
    done
  done
}

while read line 
do
  calculateRichness $line
  wait ${!}
done < list.todo




