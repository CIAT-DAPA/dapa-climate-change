# Current richness

g.region latinamerica

r.mapcalc "richness=0"

for i in $(g.mapset -l)
do
  mapset=$(echo $i | grep "^s[0-9]")
  
  if [ $mapset ]
  then
    for j in $(g.list rast mapset=$mapset | grep "stencil")
    do
      r.mapcalc "tmp=if(${j}@${mapset} == 102,1,0)"
      r.mapcalc "tmp1=richness+tmp"
      g.rename rast=tmp1,richness --o
    done
    
  fi  


done

# Future richness

g.region latinamerica

r.mapcalc "ngain=0"
r.mapcalc "nloss=0"
r.mapcalc "nrichness=0"

r.mapcalc "ogain=0"
r.mapcalc "oloss=0"
r.mapcalc "orichness=0"

r.mapcalc "rgain=0"
r.mapcalc "rloss=0"
r.mapcalc "rrichness=0"


for i in $(g.mapset -l)
do
  mapset=$(echo $i | grep "^s[0-9]")
  
  if [ $mapset ]
  then
    for adapt in n o r
    do
      for measure in gain loss richness
      do
        r.mapcalc "tmp=${adapt}${measure}@PERMANENT+${adapt}${measure}@${mapset}"       
        g.rename rast=tmp,${adapt}${measure}@PERMANENT --o
      done
    done    
  fi  
done  


# Create maps
for adapt in n o r
do

  r.mapcalc "${adapt}tunover=${adapt}gain + ${adapt}loss"

  for measure in gain loss richness turnover
  do
    # set colors
    r.colors -e map=${adapt}${measure} color=bcyr 

    # create background 
    v.in.region output=bckgrnd cat=1 --o

    # Location name
    location=$(g.gisenv -n | grep "LOCATION_NAME" | cut -d= -f2)


    # set shell variables
    export GRASS_RENDER_IMMEDIATE="FALSE"
    export GRASS_TRUECOLOR="TRUE"
    export GRASS_WIDTH=480
    export GRASS_HEIGHT=678
    export GRASS_PNGFILE="${location}_${adapt}${measure}.png"
    export GRASS_TRANSPARENT="FALSE"

    # Start the d.mon driver
    d.mon start=PNG

    # Create first frame for map
    d.frame -c at="12,100,0,100"

    # Add data layers
    d.vect map=bckgrnd type=area color=none fcolor=173:216:230
    d.rast -o map=${adapt}${measure}@PERMANENT

    # Add grid
    d.grid -c size=20
    
    # Create second frame for legend
    d.frame -c at="0,12,0,100"

    #Set font and add legend and text with title
    d.legend -v -s map=${adapt}${measure}@PERMANENT at=30,50,5,50
    d.text text="Number of species" at=5,70 size=20 align=ll color="black" font=arial

    #Stop the PNG driver, after which the layers are written to a png image.
    d.mon stop=PNG

  done
done    
