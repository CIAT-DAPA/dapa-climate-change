#!/bin/bash

# ---------------------------------------------------------------------------- # 
# Script to train models in a master slave environment with multiple slaves 
# servers.
# ---------------------------------------------------------------------------- # 

# Steps:
# 1. Inform database
# 2. Copy scripts to the slaves
# 3. Run models on slaves and copy back results
# 4. Store results correctly
# 5. Update database
# 6. Archive results on local HD
# 7. Store mv results to network drive

# Variables

# Database:
USER='model1'
HOST='192.168.20.228'
DB='gisdb'

# Paths to programms and files
ID_FILE='data/species/species_lists/run0.txt'  # this files holds all the gbif species ids for the species in this run
SAVE_TO='data/species/species_swd'  # this is the directory where all the results will be saved to

# Quality check
# threshold for training files (species and background swd) in kb
TRAINING_FILE_TH=200

# Information on the model
RUNID=2
NUMBER_SPECIES=`wc -l $ID_FILE`
NOTES="Training run for latin america"
LOCATION="/mnt/GIS-HD716/TNC_global_plants/results/training_1"

# Information on available server (ssh keys need to be enabled inorder for this to work)
# format: server:number_cores
SERVERS="flora:15 fauna:24 andromeda:10 gisbif.ciat.cgiar.org:10"
SERVER_INIT_SCRIPT="src/scripts/training/slave_init.sh"


# ---------------------- NO CHANGES BELOW ---------------------------------- #

# F U N C T I O N S
function run_training
{
   slave=$1
   id=$2
   
   # call script at slve
   ssh $slave sh tnc_tmp/src/scripts/training/train_slave.sh $id $RUNID

   # tell db that trianing is done
   psql -U $USER -d $DB -h $HOST  -c "UPDATE models SET modelfinished=now() where runtrainingid=$RUNID AND speciesid ='$ID'"
}

# Copy finished models to final destination and extract statistics
function clear_finished
{
   id=$1

   # get AUC and 
   
   # rm swd files

}

# 1. Inform database
# add model
psql -U $USER -d $DB -h $HOST -c "INSERT INTO runstraining (runtid,notes,location) VALUES ('$RUNID', '$NOTES', '$LOCATION')"

# for each species that is in the ID_FILE add it to the database for this model
if [ $1 = "import" ]
then
	while read spid
	do
	   COMPLETE_PATH=`echo $SAVE_TO/${spid:0:4}/$spid`
	   
	   # check if files are complete 
	   size=$(du $COMPLETE_PATH | cut -f1)

	   # update database
	   if (($size  < $TRAINING_FILE_TH))
	   then
	      qs="INSERT INTO models (speciesid, modelstarted, modelfinished, mostrecent, issuccessfull, exitstatus, runtrainingid) VALUES ('$spid', now(), now(), 't', 'f', 'err swd', '$RUNID')"
	   else
	      qs="INSERT INTO models (speciesid, mostrecent, runtrainingid) VALUES ('$spid', 't',  '$RUNID')"
	   fi

	   psql -U $USER -d $DB -h $HOST -c "UPDATE models SET mostrecent='f' where speciesid = '$spid' AND runtrainingid <> '$RUNID'"
	   psql -U $USER -d $DB -h $HOST -c "$qs"

	done < $ID_FILE
fi
   
# 2. Copy scripts to the slaves

# run server init 
for server in $SERVERS
do
   t_server=$(echo $server | cut -f1 -d:)
   ssh $t_server rm -r tnc_tmp
   scp $SERVER_INIT_SCRIPT $t_server:
   ssh $t_server sh slave_init.sh
done

# 3. Run models on slaves and copy back results
# generate init_queue and run_queue

INIT_QUEUE=""
RUN_QUEUE=""

for server in $SERVERS
do
   t_server=$(echo $server | cut -f1 -d:)
   n_cores=$(echo $server | cut -f2 -d:)

   INIT_QUEUE="$INIT_QUEUE $(yes $t_server | head -n $n_cores)"
done

# initiate runs
for slave in $INIT_QUEUE
do
   ID=$(psql -U $USER -d $DB -h $HOST -t -c "SELECT speciesid FROM models where runtrainingid=$RUNID AND modelstarted IS NULL limit 1")
   run_training $slave $ID &
   # updated here to prevent that the same species is modelled twice
   psql -U $USER -d $DB -h $HOST  -c "UPDATE models SET modelstarted=now() where runtrainingid=$RUNID AND speciesid ='$ID'"

   RUN_QUEUE="$RUN_QUEUE $(echo $slave:$ID | sed 's/ //')"
done

# read an other id
ID=$(psql -U $USER -d $DB -h $HOST -t -c "SELECT speciesid FROM models where runtrainingid=$RUNID AND modelstarted IS NULL limit 1")

while [ -n "$ID" ]
do
   # check queue if file exists, if exists 
   GET_NEW_ID=""
   while [ "$GET_NEW_ID" != "TRUE" ]
   do
      for i in $RUN_QUEUE
      do
         slave=$(echo $i | cut -f1 -d:)
         finished_id=$(echo $i | cut -f2 -d:)

         finished_file="results/$RUNID/${finished_id:0:4}/$finished_id.zip"

         if [ -f $finished_file ]
         then
            # delete file from queue
            OLD_RUN_QUEUE=$RUN_QUEUE
            RUN_QUEUE=$(echo $OLD_RUN_QUEUE | sed 's/'"$slave:$finished_id"'//')

            # rund next species
            run_training $slave $ID &
	    echo Starting new run!
            # updated here to prevent that the same species is modelled twice
            psql -U $USER -d $DB -h $HOST  -c "UPDATE models SET modelstarted=now() where runtrainingid=$RUNID AND speciesid ='$ID'"
            RUN_QUEUE="$RUN_QUEUE $(echo $slave:$ID | sed 's/ //')"

            # clear finished
            # at the moment everything is saved locally 

            # get new id and break loop
            GET_NEW_ID="TRUE"
            break
         fi
      done
   done
        
   ID=$(psql -U $USER -d $DB -h $HOST -t -c "SELECT speciesid FROM models where runtrainingid=$RUNID AND modelstarted IS NULL limit 1")

done



# 4. Store results correctly
# 5. Update database
# 6. Archive results on local HD
# 7. Store mv results to network drive
