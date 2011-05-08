#!/bin/bash

# $1: Number of processes to start
# $2: Name of the file, the file needs to have the following structure: family,genus,species,lon,lat,database
# $3: Name of column for this import, it is suggested to use: importYYYYMMDD


MAX_NPROC=$1
FILE=$2
TABLE=$3


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


function get_tax_and_add
{
 
  taxon=$1
  genus=$(echo $taxon | cut -d' ' -f1)
  species=$(echo $taxon | cut -d' ' -f2)
  
  col=$2
  
  # Get species ID
  speciesId=$(curl "http://data.gbif.org/species/nameSearch?rank=species&query=${genus}%20${species}&returnType=nameId&maxResults=1" | cut -f1)

  if [ $speciesId == "" ]
  then
    echo $line >> not_in_gbif
  else
  
    # get famly name
    familyId=$(psql -U model1 -d gisdb -t -c "select tf.familyid from taxgenera as tg join taxfamilies as tf on tg.familyid = tf.familyid where tg.genusname='$genus'")
    
    # check if family is already in database
    if [ $familyId != "" ]
    then  
      genusId=$(psql -U model1 -d gisdb -t -c "select genusid from taxgenera where genusname='$genus'")
    
      # add record to species table and then all the points
      psql -U model1 -d gisdb -t -c "insert inot table taxspecies (speciesid,speciesname,genusid) values ('$speciesId','$genus $species','$genusId');"
    else
      # for family and genus get name and id from GBIF
      # get family name 
      family=$(curl "http://data.gbif.org/species/classificationSearch?query=$genus%20$species&retrieveChildren=false" | awk 'NR==5{print}')
    
      # get family id
      familyId=$(curl "http://data.gbif.org/species/nameSearch?rank=family&query=$family&returnType=nameId&maxResults=1" | cut -f1)
      
      # get genus id
      familyId=$(curl "http://data.gbif.org/species/nameSearch?rank=genus&query=$genus&returnType=nameId&maxResults=1" | cut -f1)
      
      # insert into database
      # family
      psql -U model1 -d gisdb -t -c "insert inot table taxfamilies (familyid,familyname) values ('$familyId','$family');"
      
      # genus
      psql -U model1 -d gisdb -t -c "insert inot table taxgenera (genusid,genusname,familyid) values ('$genusId','$genus', '$familyId');"
      
      # species
      psql -U model1 -d gisdb -t -c "insert inot table taxspecies (speciesid,speciesname,genusid) values ('$speciesId','$genus $species','$genusId');"
    fi
  fi
  
  # now add all points of this taxon to the database
  
  awk -F',' '$3 ~ /'"$genus $species"'/{print $0}' codesan.csv | while read line
  do 
    lon=$(echo "$line" | cut -d, -f4)
    lat=$(echo "$line" | cut -d, -f5)
    db=$(echo "$line" | cut -d, -f6)
  
    psql -U model1 -d gisdb -c "INSERT INTO Points (SpeciesID,Lon,Lat,geom,Source,InModel,$col) VALUES ('$speciesId','$lon','$lat',ST_GeomFromText('POINT($lon $lat)',4326),'$db','f','t')"
     
  done
}

#################################################################

cat $FILE | cut -f3 -d, | sort -u -t','| while read LINE
do
	# check points
	add_point "$LINE" "$NAME" &

	PID=$!
	queue $PID

	while [ $NUM -ge $MAX_NPROC ]; do
		checkqueue
		sleep 0.1
	done
done 
