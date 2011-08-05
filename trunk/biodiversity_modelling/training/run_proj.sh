#!/bin/bash

function run_proj
{
   slave=$1
   runprojectid=$2
   sres=$3
   year=$4
   spid=$5
   modelprojectionid=$6
   runmodelid=$7
   USER=$8
   DB=$9
   HOST=${10}

   env=${sres}_${year}

   # call script at slave
   ssh $slave 
   sh tnc_tmp/src/scripts/proj/proj_slave.sh $runprojectid $env $spid $runmodelid
   exit

   # tell db that trianing is done
#   psql -U $USER -d $DB -h $HOST  -c "UPDATE runsprojecting SET projectionfinished=now() where runprojectid='$runprojectid' AND modelprojectionid='$modelprojectionid'"
}

