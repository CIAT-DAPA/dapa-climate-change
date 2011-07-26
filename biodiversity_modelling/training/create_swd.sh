#!/bin/bash

# Script to create directory structure and all swd so that new 
# models can be trained with maxent

# Adjust paths, files and database connection here:

# Database:
USER='model1'
HOST='192.168.20.228'
DB='gisdb'

# Paths to programms and files
ID_FILE='ids_run.txt'  # this files holds all the gbif species ids for the species in this run
SAVE_TO='/data/tnc/results/training_v2'  # this is the directory where all the results will be saved to

EXTRACTXY='/data/TNC/src/extractxy.py'

ENV_VAR_LOCATION='/mnt/GIS-HD716/TNC_global_plants/data/env'
ENV_VAR="$ENV_VAR_LOCATION/bio/bio_1/hdr.adf $ENV_VAR_LOCATION/bio/bio_2/hdr.adf $ENV_VAR_LOCATION/bio/bio_3/hdr.adf $ENV_VAR_LOCATION/bio/bio_4/hdr.adf $ENV_VAR_LOCATION/bio/bio_5/hdr.adf $ENV_VAR_LOCATION/bio/bio_6/hdr.adf $ENV_VAR_LOCATION/bio/bio_6/hdr.adf $ENV_VAR_LOCATION/bio/bio_7/hdr.adf $ENV_VAR_LOCATION/bio/bio_8/hdr.adf $ENV_VAR_LOCATION/bio/bio_9/hdr.adf"

BIOMES='path to biomes'
CONTINENTS='path to continents'

# No further changes chould be make below
# ---------------------------------------

# Can I connect to the database?
psql -U $USER -h $HOST -d $DB -t -c "SELECT * FROM models limit 0"

# If that failed stop script
if [ $? != 0 ]
then
  echo " Can not connect to database"
  exit 1
fi

# Do the files required exists?
if [ ! -f $ID_FILE ]
then
   echo "No file with species ids provided, or path is wrong"
   exit 1
fi

# Do outfile exists?
if [ ! -f $SAVE_TO]
then
   echo "Creating outfile ($OUT_FILE)"
   mkdir -p $SAVE_TO
fi


# Now cycle through all species and create files

while read spid
do
   # Get the complete path
   SAVE_TO_COMPLETE=$(echo $SAVE_TO/${spid:0:4}/$spid)

   # Create the directory
   mkdir -p $SAVE_TO_COMPLETE

   # Get points from database
   psql -U $USER -h $HOST -d $DB -t -c "SELECT lon,lat FROM points where speciesid='$spid'" | sed 's/|/,/g' | sed 's/ //g' > $SAVE_TO_COMPLETE/tmp.xy

   # mark the points as used in a model
   psql -U $USER -h $HOST -d $DB -t -c "UPDATE points SET inmodel='t' where speciesid='$spid' and inmodel='f'" 
   
   # Get values of environmental variables
   python $EXTRACTXY -f $ENV_VAR -xy $SAVE_TO_COMPLETE/tmp.xy > $SAVE_TO_COMPLETE/tmp.bio

   # Paste coordinates and environmental values together
   paste -d, $SAVE_TO_COMPLETE/tmp.xy $SAVE_TO_COMPLETE/tmp.bio > $SAVE_TO_COMPLETE/species.swd

   # Get the background swd file

   python $EXTRACTXY -f $ENV_VAR -xy $SAVE_TO_COMPLETE/tmp.xy > $SAVE_TO_COMPLETE/tmp.bio

   # remove all tmp files
   rm $SAVE_TO_COMPLETE/tmp.xy $SAVE_TO_COMPLETE/tmp.bio

done < $ID_FILE




