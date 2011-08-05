#!/bin/bash

# Create a list of species (gbif species id) to be consiered in a run
# species from which training should be projected?
MODEL_RUN_ID=2

# what is the auc threshold?
AUC_THRESHOLD=0.7

# where to save projected distributions

# which scenarion should be projected? 
sres="A1B A2"
years="2030 2050"
gcms="avg"

# create list to be projected
# c_2000 project the current first
PROJ_QUEUE="current_2000_avg"

for i in $sres
do
  for j in $years
  do
    for k in $gcms
    do
      tmp=$PROJ_QUEUE
      PROJ_QUEUE="$tmp ${i}_${j}_${k}"
    done
  done
done 
      

# Database:
USER='model1'
HOST='192.168.20.228'
DB='gisdb'

# more information on the projection run
resolution="5k"
region="latin_america"

# ------------------ NO CHANGE BELOW --------------------------------------- #

# create entry for porj run
for i in $PROJ_QUEUE
do
  sres=$(echo $i | cut -f1 -d'_')
  year=$(echo $i | cut -f2 -d'_')
  gcm=$(echo $i | cut -f3 -d'_')

  psql -U $USER -h $HOST -d $DB -c "INSERT INTO runsprojecting (sres,year,gcm,resolution,region) VALUES ('$sres', '$year', '$gcm', '$resolution', '$region')"
  
  # get proj id
  PROJ_ID=$(psql -U $USER -h $HOST -d $DB -t -c "SELECT runpid from runsprojecting where sres='$sres' AND year='$year' AND gcm='$gcm' AND resolution='$resolution' AND region='$region'")

  # Add all the species that should be projected into modelprojections
  for ID in $(psql -U $USER -d $DB -h $HOST -t -c "SELECT speciesid FROM models where runtrainingid=$MODEL_RUN_ID AND auc > $AUC_THRESHOLD AND issuccessfull='t'")
  do
    # get model id
    M_ID=$(psql -U $USER -d $DB -h $HOST -t -c "SELECT modelid FROM models WHERE runtrainingid=$MODEL_RUN_ID AND speciesid=$ID")
    
    # insert into table
    psql -U $USER -d $DB -h $HOST -c "INSERT INTO modelprojections (modelid, runprojectid) VALUES ('$M_ID', '$PROJ_ID')"
  done
done















