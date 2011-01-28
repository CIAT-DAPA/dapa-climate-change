#--------------------------------------------------------------------#
# prepare species files

# 1. split species file into by species id
awk -F "," '{close(f);f=$1}{print > f".txt"}' World_filtered_Plantae.csv 


# 2. only use the once with more than then 10 records

while read line 
do
   nl=`wc -l $line | cut -d' ' -f1`
   if [ $(echo "$nl > 9" | bc) -gt 0 ]
   then
      echo "specie_id,specie,genus_id,genus,family_id,family,lat,lon,year" > all/$line
      cat $line >> all/$line
      echo "done with $line"
   fi
done < all.txt


