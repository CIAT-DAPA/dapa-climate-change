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

# Start GRASS shell up at location and mapset
# specify proper variables here

####
# Batch wrapper

function calculateRichness {
  sfolder=$1
  location=$2
  
  export GRASS_BATCH_JOB=/data1/TNC/results/grass/$location/s$sfolder/richness_mapset.rw.sh
  /usr/bin/grass64 /data1/TNC/results/grass/$location/s$sfolder
  unset GRASS_BATCH_JOB
}

while read folder 
do
  echo "doing folder $folder"
  MODEL=lam_5k_$1
  cp /data1/TNC/src/summary/richness_mapset.sh /data1/TNC/results/grass/$MODEL/s$folder/richness_mapset.rw.sh
  chmod 777 /data1/TNC/results/grass/$MODEL/s$folder/richness_mapset.rw.sh
  
  calculateRichness $folder $MODEL
  rm /data1/TNC/results/grass/$MODEL/s$folder/richness_mapset.rw.sh
done



