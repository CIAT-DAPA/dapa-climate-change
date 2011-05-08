#!/bin/bash

# $1: Number of processes to start
# $2: Name of the file, the file needs to have the following structure: family,genus,species,lon,lat,database


MAX_NPROC=$1
FILE=$2


# check wether or not functions for running bash in parallel exists
if [ -f bash_parallel.sh ]
then  
  echo "loading functions .. "
  . bash_parallel.sh
else
  echo "downloading parallel functions from svn"
  svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/util/bash_parallel.sh
  . bash_parallel.sh
fi

function add_point {

  line="$1"
  
  family=$(echo "$line" | cut -d, -f1)
  genus=$(echo "$line" | cut -d, -f2)
  species=$(echo "$line" | cut -d, -f3)
  lon=$(echo "$line" | cut -d, -f4)
  lat=$(echo "$line" | cut -d, -f5)
  db=$(echo "$line" | cut -d, -f6)
  
  # get the gbif id for the point
  
  # do family check only if family infofrmation is available
  if [ "$family" == "" -o "$family" == "NA" ]
  then
    toAddID=$(psql -U model1 -d gisdb -t -c "SELECT speciesid FROM taxspecies WHERE speciesname='$species';")    
  else
    toAddID=$(psql -U model1 -d gisdb -t -c "SELECT ts.speciesid FROM taxspecies AS ts JOIN taxgenera AS tg ON ts.genusid = tg.genusid JOIN taxfamilies AS tf ON tg.familyid = tf.familyid WHERE tf.familyname='$family' AND ts.speciesname='$species';")
  fi
  
  if [ "$toAddID" != "" ]
  then
    exists=$(psql -U model1 -d gisdb -t -c "SELECT ST_GeomFromText('POINT($lon $lat)',4326) IN (SELECT geom FROM Points WHERE SpeciesId='$toAddID');")
    
    # add the point
    if [ $exists == "f" ]
    then
      psql -U model1 -d gisdb -c "INSERT INTO Points (SpeciesID,Lon,Lat,geom,Source,InModel,DateAdded) VALUES ('$toAddID','$lon','$lat',ST_GeomFromText('POINT($lon $lat)',4326),'$db','f',current_date)"
    else
      echo "$line" >> point_already_in_db.csv
    fi
  else
    echo "$line" >> taxonomy_not_found.csv
  fi
}

##################
# load variabiles
# get first ID

while read LINE
do
	# check points
	add_point "$LINE" &

	PID=$!
	queue $PID

	while [ $NUM -ge $MAX_NPROC ]; do
		checkqueue
		sleep 0.1
	done
	
done < $FILE




