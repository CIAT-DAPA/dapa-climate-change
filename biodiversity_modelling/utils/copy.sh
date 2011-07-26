#!/bin/bash

# ---------------------------------------------------------------------------- #
# copy a directory with all its sub directory to a different HD and check each
# file for consistentency
# Usage:
# ------
# sh copy.sh dirA /home/jsigner/dirB
# copies dirA into dirB, when located in the dir that where dira is. At the 
# moment the cp process need to be started from the parent dir, otherwise paths
# will get screwed up
# ---------------------------------------------------------------------------- #

function cp_file
{
   cp $1 $2
   while [ $? != 0 ]
   do
      echo 'lost connection, but still trying'
      sleep 1
      cp $1 $2
   done
}


# create directory structure
for dir in $(find $1 -type d)
do
   mkdir -p "$2$dir"
   while [ $? != 0 ]
   do
      echo 'lost connection, but still trying'
      sleep 5
      mkdir -p "$2$dir"
   done
done

echo "created files"

# copy each folder
for file in $(find $1 -type f)
do
   echo $file
   h1=$(md5sum $file | cut -d' ' -f1)
   cp_file $file $2$file
   h2=$(md5sum $2$file | cut -d' ' -f1)

   while [ "$h1" != "$h2" ]
   do
      cp_file $file $2$file
      h2=$(md5sum $2$file | cut -d' ' -f1)
      echo "hash failed, cping again" 
   done
done

