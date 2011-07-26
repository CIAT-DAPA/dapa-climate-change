#!/bin/bash

for i in 1 2 3 4 5 6 7 8; 
do  
  for j in part.$i/*;  
  do
   id=$(echo $j | cut -f2 -d/);   
   nf=$(echo ${id:0:4});   
   if [ -e "$nf" ];   
   then    
    mv -v $j $nf;   
   else    
    mkdir $nf;    
    mv -v $j $nf;   
  fi;
  done; 
done

rm -r part*



# if files are in no sub folders
for i in *.tif
do
  nf=$(echo ${i:0:4})
  if [ -e "$nf" ];   
  then    
    mv -v $i $nf;   
  else    
    mkdir $nf;    
    mv -v $i $nf;   
  fi;
done 
