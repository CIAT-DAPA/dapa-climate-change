#!/bin/bash

# Script to create directory structure and all swd so that new 
# models can be trained with maxent

# Adjust paths, files and database connection here:

# Database:
USER='model1'
HOST='192.168.20.228'
DB='gisdb'

# Paths to programms and files
ID_FILE='data/species/species_lists/run0.txt'  # this files holds all the gbif species ids for the species in this run
SAVE_TO='data/species/species_swd'  # this is the directory where all the results will be saved to

# script to extract values at xy locations
EXTRACTXY='src/scripts/utils/extractxy.py'

# location of environmental variables
ENV_VAR_LOCATION='data/env'
ENV_VAR="$ENV_VAR_LOCATION/bio/bio_1/hdr.adf $ENV_VAR_LOCATION/bio/bio_2/hdr.adf $ENV_VAR_LOCATION/bio/bio_3/hdr.adf $ENV_VAR_LOCATION/bio/bio_4/hdr.adf $ENV_VAR_LOCATION/bio/bio_5/hdr.adf $ENV_VAR_LOCATION/bio/bio_6/hdr.adf $ENV_VAR_LOCATION/bio/bio_8/hdr.adf $ENV_VAR_LOCATION/bio/bio_9/hdr.adf $ENV_VAR_LOCATION/bio/bio_12/hdr.adf $ENV_VAR_LOCATION/bio/bio_13/hdr.adf $ENV_VAR_LOCATION/bio/bio_14/hdr.adf $ENV_VAR_LOCATION/bio/bio_15/hdr.adf $ENV_VAR_LOCATION/bio/bio_18/hdr.adf $ENV_VAR_LOCATION/bio/bio_19/hdr.adf"
ENV_VAR_HEADER='species,lon,lat,bio1,bio2,bio3,bio4,bio5,bio6,bio8,bio9,bio12,bio13,bio14,bio15,bio18,bio19'

# path to raster of biomes and continents
BIOMES="$ENV_VAR_LOCATION/biomes/biomes.tif"
CONTINENTS="$ENV_VAR_LOCATION/continents/continents.tif"

# number of cores:
MAX_NPROC=20
QUEUE=""

# No further changes chould be make below
# ---------------------------------------

# Can I connect to the database?
psql -U $USER -h $HOST -d $DB -t -c "SELECT * FROM models limit 0"

# If that failed stop script
if [ $? != 0 ]
then
  echo "Can not connect to database"
  exit 1
fi

# Do the files required exists?
if [ ! -f $ID_FILE ]
then
   echo "No file with species ids provided, or path is wrong"
   exit 1
fi

# Do dir exists?
if [ ! -f $SAVE_TO ]
then
   echo "Creating dir ($OUT_FILE)"
   mkdir -p $SAVE_TO
fi

# Load what else is needed
. src/scripts/utils/bash_parallel.sh


# Now cycle through all species and create files

function create_swd
{
   # species id
   spid=$1

   # Get the complete path
   SAVE_TO_COMPLETE=$(echo $SAVE_TO/${spid:0:4}/$spid)

   # Create the directory
   mkdir -p $SAVE_TO_COMPLETE

   # Print header
   echo "long,lat" > $SAVE_TO_COMPLETE/xy.tmp

   # Get points from database
   psql -U $USER -h $HOST -d $DB -t -c "SELECT lon,lat FROM points where speciesid='$spid'" | sed 's/|/,/g' | sed 's/ //g' >> $SAVE_TO_COMPLETE/xy.tmp

   # mark the points as used in a model
   psql -U $USER -h $HOST -d $DB -t -c "UPDATE points SET inmodel='t' where speciesid='$spid' and inmodel='f'" 
   
   # Get values of environmental variables
   python $EXTRACTXY -f $ENV_VAR -xy $SAVE_TO_COMPLETE/xy.tmp > $SAVE_TO_COMPLETE/points_swd.tmp
   echo $ENV_VAR_HEADER > $SAVE_TO_COMPLETE/$spid.swd
   awk 'BEGIN{OFS=","}{print '"$spid"',$0}' $SAVE_TO_COMPLETE/points_swd.tmp | grep -v 32768 >> $SAVE_TO_COMPLETE/$spid.swd

   # Get the background swd file
   # From which biome
   T_BIOMES=$(python $EXTRACTXY -f $BIOMES  -xy $SAVE_TO_COMPLETE/xy.tmp | cut -f3 -d, | grep -v 9999 | sort -u)
   T_CONTINENTS=$(python $EXTRACTXY -f $CONTINENTS -xy $SAVE_TO_COMPLETE/xy.tmp | cut -f3 -d, | grep -v 255 | sort -u)

   # select background points from database
   qstring="SELECT lon,lat,bio1,bio2,bio3,bio4,bio5,bio6,bio8,bio9,bio12,bio13,bio14,bio15,bio18,bio19 FROM background where biome="$(echo \'$T_BIOMES\' | sed "s/ /\' OR biome=\' /g")" AND continent="$(echo \'$T_CONTINENTS\' | sed "s/ /\' OR continent=\' /g")" ORDER BY random() LIMIT 10000"

   echo $ENV_VAR_HEADER > $SAVE_TO_COMPLETE/background.swd
   psql -U $USER -h $HOST -d $DB -t -c "$qstring" | sed 's/|/,/g' | sed 's/ //g' | grep -v 9999  > $SAVE_TO_COMPLETE/bg_swd.tmp
   awk 'BEGIN{OFS=","}{print '"background"',$0}' $SAVE_TO_COMPLETE/bg_swd.tmp >> $SAVE_TO_COMPLETE/background.swd
   
   # remove all tmp files
   rm $SAVE_TO_COMPLETE/*.tmp
   zip $SAVE_TO_COMPLETE/$spid.zip $SAVE_TO_COMPLETE/*
   rm $SAVE_TO_COMPLETE/*.swd
}


while read spid
do
   create_swd $spid &

   queue $!

   while [ $NUM -ge $MAX_NPROC ]
   do
      checkqueue
      sleep 0.4
   done
                                                                           
   sleep 0.1
done < $ID_FILE


