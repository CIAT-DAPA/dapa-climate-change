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

# Model id, models that should be used as base of projection
RUN_MODEL_ID=2

# Information on available server (ssh keys need to be enabled inorder for this to work)
# format: server:number_cores
SERVERS="flora:10 fauna:14 andromeda:12 gisbif.ciat.cgiar.org:15"
SERVER_INIT_SCRIPT="src/scripts/proj/slave_init.sh"

# Where all the data will be stored after projection (/data/tnc/results/proj/)
DATA="/data/tnc/results/proj"

# ---------------------- NO CHANGES BELOW ---------------------------------- #
# F U N C T I O N S
function run_proj
{
   slave=$1
   sres=$2
   year=$3
   spid=$4
   modelprojectionid=$5
   runmodelid=$6

   env=${sres}_${year}

   # call script at slave
   ssh -f -n $slave sh tnc_tmp/src/scripts/proj/proj_slave.sh $modelprojectionid $env $spid $runmodelid
}

# 1. Copy scripts to the slaves

# run server init 
for server in $SERVERS
do
   t_server=$(echo $server | cut -f1 -d:)
   ssh $t_server rm -r tnc_tmp
   scp $SERVER_INIT_SCRIPT $t_server:
   ssh $t_server sh slave_init.sh
done

# 2. Run models on slaves and copy back results
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
   # read an other proj_id, sres, year, spid and modelid from database
   read runprojectid sres year spid modelprojectionid <<< $(psql -U $USER -d $DB -h $HOST -t -c "select runprojectid,sres,year,speciesid,modelprojectionid from modelprojections as mp join runsprojecting as rp on mp.runprojectid = rp.runpid join models as m on mp.modelid = m.modelid where projectionstarted is null limit 1;" | sed 's/ //g;s/|/ /g')
   
   run_proj $slave $sres $year $spid $modelprojectionid $RUN_MODEL_ID &

   # updated here to prevent that the same species is modelled twice
   psql -U $USER -d $DB -h $HOST  -c "UPDATE modelprojections SET projectionstarted=now() where modelprojectionid='$modelprojectionid'"

   RUN_QUEUE="$RUN_QUEUE $(echo $slave:$spid:$modelprojectionid | sed 's/ //')"
done

# read an other proj_id, sres, year, spid and modelid from database
read runprojectid sres year spid modelprojectionid <<< $(psql -U $USER -d $DB -h $HOST -t -c "select runprojectid,sres,year,speciesid,modelprojectionid from modelprojections as mp join runsprojecting as rp on mp.runprojectid = rp.runpid join models as m on mp.modelid = m.modelid where projectionstarted is null limit 1;" | sed 's/ //g;s/|/ /g')


while [ -n "$spid" ]
do
   # check queue if file exists, if exists 
   GET_NEW_ID=""
   while [ "$GET_NEW_ID" != "TRUE" ]
   do
      for i in $RUN_QUEUE
      do
         slave=$(echo $i | cut -f1 -d:)
         finished_id=$(echo $i | cut -f2 -d:)
         finished_modelprojectionid=$(echo $i | cut -f3 -d:)

         finished_file="tmp/${finished_modelprojectionid}.txt"

         # if a run finished
         if [ -f $finished_file ]
         then
  
            finished_exit=$(cat $finished_file)

            # tell db that trianing is done
            psql -U $USER -d $DB -h $HOST  -c "UPDATE modelprojections SET projectionfinished=now(),exitstatus='$finished_exit' where modelprojectionid='$finished_modelprojectionid'"


            if [ "$finished_exit" == "0" ]
            then
              dir=$(echo "$finished_modelprojectionid + 10000000" | bc)
              mkdir -p $DATA/${dir:0:4}
              mv results/proj/$finished_modelprojectionid.tif.gz $DATA/${dir:0:4} &
            fi
	    
            rm $finished_file &

            # delete file from queue
            OLD_RUN_QUEUE=$RUN_QUEUE
            RUN_QUEUE=$(echo $OLD_RUN_QUEUE | sed 's/'"$slave:$finished_id:$finished_modelprojectionid"'//')

            # project next species
            run_proj $slave $sres $year $spid $modelprojectionid $RUN_MODEL_ID &

            # updated here to prevent that the same species is modelled twice
            psql -U $USER -d $DB -h $HOST  -c "UPDATE modelprojections SET projectionstarted=now() where modelprojectionid='$modelprojectionid'"
            RUN_QUEUE="$RUN_QUEUE $(echo $slave:$spid:$modelprojectionid | sed 's/ //')"

            # clear finished
            # at the moment everything is saved locally 

            # get new id and break loop
            GET_NEW_ID="TRUE"
            break
         fi
      done
   done
        
   # read an other proj_id, sres, year, spid and modelid from database
   read runprojectid sres year spid modelprojectionid <<< $(psql -U $USER -d $DB -h $HOST -t -c "select runprojectid,sres,year,speciesid,modelprojectionid from modelprojections as mp join runsprojecting as rp on mp.runprojectid = rp.runpid join models as m on mp.modelid = m.modelid where projectionstarted is null limit 1;" | sed 's/ //g;s/|/ /g')

done

