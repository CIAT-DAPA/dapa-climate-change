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
  r.in.gdal in=current_richness.asc out=current_richness -o
  r.mapcalc MASK=current_richness

for adapt in f o r
do


  r.mapcalc "${adapt}change=${adapt}richness - current_richness"
  r.mapcalc "${adapt}turnover=100 * (${adapt}gain * 1.00 + loss) / (current_richness * 1.00 + ${adapt}gain)"

  for measure in richness turnover change
  do
    # set colors
    r.colors -e map=${adapt}${measure} color=rainbow
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

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Export Rasters in GRASS and make maps in R

# Calculate
r.in.gdal in=current_richness.asc out=current_richness -o
r.mapcalc MASK=current_richness

location=$(g.gisenv -n | grep "LOCATION_NAME" | cut -d= -f2)

for adapt in f o r
do


  r.mapcalc "${adapt}change=${adapt}richness - current_richness"
  r.mapcalc "${adapt}turnover=100 * (${adapt}gain * 1.00 + loss) / (current_richness * 1.00 + ${adapt}gain)"

  for measure in richness turnover change
  do
    r.out.arc in=$adapt$measure out=${location}_${adapt}_${measure}.asc
  done
done

# null adaption
r.out.arc in=nrichness out=${location}_nrichness.asc

r.mapcalc "nturnover=100 * (loss) / (current_richness * 1.00)"
r.mapcalc "nchange=nrichness - current_richness"

r.out.arc in=nturnover out=${location}_nturnover.asc
r.out.arc in=nchange out=${location}_nchange.asc

r.out.arc in=current_richness out=current_richness.asc



# ---------------------------------------------------------------------------- #
# In R

library(raster)
library(stringr)
library(maps)
library(fields)

# turnover

printMap <- function(path) {

  r <- raster(path)

  if (grepl("turnover", path)) {
    ra <- c(0,100)
  } 
  if (grepl("change", path)) {
    ra <- c(-3000, 3000)
  } 
  if (grepl("richness", path)) {
    ra <- c(0,7000)
  }

  colors <- rev(terrain.colors(7000))

  v <- getValues(r)
  from <- min(v, na.rm=T)
  to <- max(v, na.rm=T)

  from <- floor(rscale(from, ra))
  to <- floor(rscale(to, ra))

  png(height=680, width=480, str_c(str_sub(path,1,-4), "png"))
  par(mar=c(7,3,2,1))
  plot(0, xlim=c(-120,-33), ylim=c(-60,35), xaxt="n", yaxt="n", xlab="", ylab="", asp=95/87)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="cornflowerblue")
  map("world", add=T, fill=T, col="grey", lwd=0.5)
  image(r, col=colors[from:to], add=T)
  map("world", add=T, lwd=0.5)
  abline(h=0, lty=2, lwd=0.5)
  box()

  axis(2, at=seq(-60,60,10),label=F, tck=-0.02) 
  axis(2, at=seq(-60,60,20), las=T)

  axis(1, at=seq(-120,0,10),label=F, tck=-0.02) 
  axis(1, at=seq(-120,60,20), las=T)

  image.plot(zlim=ra, nlevel=100,legend.only=TRUE, horizontal=T,col=colors)
  
  dev.off()
  
}

rscale <- function(x, ra) {
  return(0 + (x - ra[1]) * (7000 - 0)/(ra[2] - ra[1]))
}


for (i in list.files(pattern="*.asc")) {
  printMap(i)
  print(i)
}



# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# reclass all and buffer

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
