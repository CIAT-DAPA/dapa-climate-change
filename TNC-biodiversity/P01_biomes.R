######################################################################
# Prepare bimoes for the whole world
# Author: Johannes
# Email: jmsigner@gmail.com
# Date: 26.1.2011
######################################################################

#--------------------------------------------------------------------#
# Workflow in GRASS
#
# 1.) Mask for each continent
# 2.) Mask for each ecoregion in that continent
# 3.) Write xyz values 

for continent in 3 4 5 6  
do
   
   for biome in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 98 99
   do
      g.region res=00:10
      r.mapcalc "MASK=if(continents==$continent && biomes==$biome)"
      area=`r.report -h -n map=biomes@PERMANENT units=h  | awk -F'|' '/TOTAL/{print $3}' | sed 's/,//g'`
      if [  $(echo "$area > 0" | bc) -gt 0 ]
      then
         g.region res=00:00:30
         r.out.xyz in=biomes out=continent${continent}_biome${biome}.txt
      fi
      g.remove rast=MASK
      echo "continent: $continent -- biome: $biome"
   done
done

#--------------------------------------------------------------------#
# In R: for each biome in each region create 50 files with 100 points,
# with less points if the total number of points is only a few thousands

for (con in 1:6) {
   for (biome in c(1:14,98,99)) {
      r <- read.table(str_c("continent",con,"_biome",biome,".txt"),sep="|")
      for(i in 1:20) write.csv(r[sample(1:nrow(r),10000),],str_c("continent",con,"_biome",biome,"sample",i,".txt"))
       
   }
}

