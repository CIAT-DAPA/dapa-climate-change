#!/bin/bash

# $1: Number of processes to start
# $2: Name of the file, the file needs to have the following structure: family,genus,species,lon,lat,database
# $3: Name of column for this import, it is suggested to use: importYYYYMMDD


MAX_NPROC=$1
FILE=$2


# check wether or not functions for running bash in parallel exists
if [ -f bash_parallel.sh ]
then  
  echo "loading functions .. "
  . bash_parallel.sh
else
  echo "downloading parallel functions from svn"
  svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/utils/bash_parallel.sh
  . bash_parallel.sh
fi

function add_point {

  line=$1
  name=$2
  
  family=$(echo $line | cut -d, -f1)
  genus=$(echo $line | cut -d, -f2)
  species=$(echo $line | cut -d, -f3)
  lon=$(echo $line | cut -d, -f4)
  lat=$(echo $line | cut -d, -f5)
  db=$(echo $line | cut -d, -f6)
  
  # get the gbif id for the point
  toAddID=$(psql -U model1 -d gisdb -t -c "SELECT ts.speciesid FROM taxspecies AS ts 
      JOIN taxgenera AS tg ON ts.genusid = tg.genusid 
      JOIN taxfamilies AS tf ON tg.familyid = tf.familyid WHERE tf.familyname='$family' AND ts.speciesname='$species';")
  
  if [ -n "toAddID" ]
  then
    exists=$(psql -U model1 -d gisdb -t -c "SELECT ST_GeomFromText('POINT($lon $lat)',4326) IN (SELECT geom FROM Points WHERE SpeciesId='$toAddID');")
    
    # add the point
    if [ $exists = "f" ]
      psql -U model1 -d gisdb -c "INSERT INTO Points (SpeciesID,Lon,Lat,geom,Source,InModel,$name) VALUES ('$toAddID','$lon','$lat',ST_GeomFromText('POINT($lon $lat)',4326),'$db','f','t')"
    else
      echo $line >> point_already_in_db.csv
    fi
  else
    echo $line >> taxonomy_not_found.csv
  fi
}

##################
# load variabiles
# get first ID

	NAME=$3
  psql -U model1 -d gisdb -c "ALTER TABLE Points ADD column $NAME boolean;"
	
while read line
do
	# check points
	add_point $ID $NAME &

	PID=$!
	queue $PID

	while [ $NUM -ge $MAX_NPROC ]; do
		checkqueue
		sleep 0.1
	done
	
done < $2





