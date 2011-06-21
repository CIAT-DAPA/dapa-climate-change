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

    # Plot current   

    # set colors
    r.colors -e map=richness rules=richness.rules

    # create background 
    v.in.region output=bckgrnd cat=1 --o


    # set shell variables
    export GRASS_RENDER_IMMEDIATE="FALSE"
    export GRASS_TRUECOLOR="TRUE"
    export GRASS_WIDTH=480
    export GRASS_HEIGHT=678
    export GRASS_PNGFILE="current_richness.png"
    export GRASS_TRANSPARENT="FALSE"

    # Start the d.mon driver
    d.mon start=PNG

    # Create first frame for map
    d.frame -c at="12,100,0,100"

    # Add data layers
    d.vect map=bckgrnd type=area color=none fcolor=173:216:230
    d.rast -o map=richness

    # Add grid
    d.grid -c size=20
    
    # Create second frame for legend
    d.frame -c at="0,12,0,100"

    #Set font and add legend and text with title
    d.legend -v -s map=richness at=30,50,5,50
    d.text text="Number of species" at=5,70 size=20 align=ll color="black" font=arial

    #Stop the PNG driver, after which the layers are written to a png image.
    d.mon stop=PNG

# Future richness
  

# Create maps
for adapt in n o r
do

  r.mapcalc "${adapt}change=${adapt}richness - current_richness"
  r.mapcalc "${adapt}turnover=100 * (${adapt}gain * 1.00 + ${adapt}loss) / (current_richness * 1.00 + ${adapt}gain)"

  for measure in richness turnover change
  do
    # set colors
    r.colors -e map=${adapt}${measure} rules=$measure.rules
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
    if [ $measure = "turnover" ]
    then
      d.legend -v -s map=${adapt}${measure}@PERMANENT at=30,50,5,50 range=0,100
    elif [ $measure = "richness" ]
    then
      d.legend -v -s map=${adapt}${measure}@PERMANENT at=30,50,5,50 range=7,5533
    elif [ $measure = "change" ]
    then
      d.legend -v -s map=${adapt}${measure}@PERMANENT at=30,50,5,50 range=-2987,1001
    fi

    if [ $measure = "turnover" ]
    then
      d.text text="Species turnover" at=5,70 size=20 align=ll color="black" font=arial
    else
      d.text text="Number of species" at=5,70 size=20 align=ll color="black" font=arial
    fi

    #Stop the PNG driver, after which the layers are written to a png image.
    d.mon stop=PNG

  done
done    




# reclass all currents

# and buffer

g.region latinamerica

echo -e "102 = 1 \n * = NULL " > current.rc 

for i in $(g.mapset -l)
do
  mapset=$(echo $i | grep "^s[0-9]")
  
  if [ $mapset ]
  then
    # go to the mapset
    g.mapset mapset=$mapset

    for j in $(g.list rast mapset=$mapset | grep "stencil")
    do
      id=$(echo $j | sed 's/.stencil//')
      r.reclass in=$j out=$id.pressence rules=current.rc
      r.buffer in=$id.pressence out=$id.buf distances=5,8,30,40 units=kilometers --o
    done
    
  fi  
done
