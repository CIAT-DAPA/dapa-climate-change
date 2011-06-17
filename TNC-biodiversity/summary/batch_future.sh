# Batch job, to be started in current directory
  # which folder should I work in
  folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=' | cut -c2-)
  location=$(g.gisenv -n | grep 'LOCATION_NAME' | cut -f2 -d'=')
  gisdb=$(g.gisenv -n | grep 'GISDBASE' | cut -f2 -d'=')

# go to folder
  cd /data/TNC/results/$location/$folder

  # make folder for reclassification tables  
  mkdir rc.tables  

  # symlink to current of the mapset
  # prefix symlinked mapset with c, for current
  ln -s $gisdb/current/s$folder $gisdb/$location/c$folder

  # figure out which year we are treating

  year=$(echo $location | awk 'BEGIN{FS=OFS="_"}{print $4,$5}')

  # setting thresholds according to the year of the SRES
  # each sres correspongs to a value in the [spid].stencil.b 
  # 10 = current + chull (300km) = null adaption
  # 8 = current + chull (300km) + 6 km = 2030 realistic
  # 6 = current + chull (300km) + 8 km = 2050 realistic
  # 4 = current + chull (300km) + 30 km = 2030 optimistic
  # 2 = current + chull (300km) + 40 km = 2050 optimistic

  if [ $year = "2020_2049" ] 
  then
    realistic_richness="11 12"
    realistic_gain="12"
    optimistic_richness="11 12 13 14"
    optimistic_gain="12 13 14"
  elif [ $year = "2040_2069" ]
  then
    realistic_richness="11 12 13"
    realistic_gain="12 13"
    optimistic_richness="11 12 13 14 15"
    optimistic_gain="12 13 14 15"
  fi

  # Create reclass tables for gain, loss and richness
  echo -e "$realistic_gain = 1 \n * = 0 " > rc.tables/realistic_gain.rc 
  echo -e "$realistic_richness = 1 \n * = 0 " > rc.tables/realistic_richness.rc 
  echo -e "$optimistic_gain = 1 \n * = 0 " > rc.tables/optimistic_gain.rc 
  echo -e "$optimistic_richness = 1 \n * = 0 " > rc.tables/optimistic_richness.rc 

  # null, full and loss dont change
  echo -e "11 = 1 \n * = 0 " > rc.tables/null_richness.rc  # null has no gain
  echo -e "10 11 12 13 14 15 = 1 \n * = 0 " > rc.tables/full_richness.rc 
  echo -e "10 12 13 14 15 = 1 \n * = 0 " > rc.tables/full_gain.rc 
  echo -e "1 = 1 \n * = 0 " > rc.tables/loss.rc 

  r.mapcalc "rrichness.tmp=0"
  r.mapcalc "rgain.tmp=0"
  r.mapcalc "orichness.tmp=0"
  r.mapcalc "ogain.tmp=0"
  r.mapcalc "nrichness.tmp=0"
  r.mapcalc "fgain.tmp=0"
  r.mapcalc "frichness.tmp=0"
  r.mapcalc "loss.tmp=0"


  # cycle through each tif
  for tif in *.tif
  do
    # species id
    base=$(echo $tif | sed 's/.tif//')


    # getting threshold value (already rescaled to 1 - 256)
    # threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")
    threshold=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; select thresholdrs from species where species_id=$base;")

    # register raster in GRASS
    r.in.gdal in=$base.tif out=s$base -o 

    # create reclassification table (0 absence, 10 presence) 
    echo -e "0 thru $threshold = 0 \n$threshold thru 300 = 10 " > rc.tables/$base.rc 

    # reclass
    r.reclass in=s$base out=s$base.p rules=rc.tables/$base.rc --o

    # Add current buffered distribution and futur distribution together
    r.mapcalc "s$base.start=s$base.p + s$base.buf@c$folder"  
    
    # set s$base.start null to 0, in order to avoid problems
    r.null map=s$base.start null=0

    # realistic gain and richness
    r.reclass in=s$base.start out=s$base.rrichness rules=rc.tables/realistic_richness.rc 
    r.reclass in=s$base.start out=s$base.rgain rules=rc.tables/realistic_gain.rc 

    r.mapcalc "rrichness=rrichness.tmp + s$base.rrichness"
    g.rename rast=rrichness,rrichness.tmp --o

    r.mapcalc "rgain=rgain.tmp + s$base.rgain"
    g.rename rast=rgain,rgain.tmp --o
    
    # optimistic gain and richness
    r.reclass in=s$base.start out=s$base.orichness rules=rc.tables/optimistic_richness.rc 
    r.reclass in=s$base.start out=s$base.ogain rules=rc.tables/optimistic_gain.rc 

    r.mapcalc "orichness=orichness.tmp + s$base.orichness"
    g.rename rast=orichness,orichness.tmp --o

    r.mapcalc "ogain=ogain.tmp + s$base.ogain"
    g.rename rast=ogain,ogain.tmp --o
    
    # null richness
    r.reclass in=s$base.start out=s$base.nrichness rules=rc.tables/null_richness.rc

    r.mapcalc "nrichness=nrichness.tmp + s$base.nrichness"
    g.rename rast=nrichness,nrichness.tmp --o

    # full richness and gain
    r.reclass in=s$base.start out=s$base.frichness rules=rc.tables/full_richness.rc 
    r.reclass in=s$base.start out=s$base.fgain rules=rc.tables/full_gain.rc 

    r.mapcalc "frichness=frichness.tmp + s$base.frichness"
    g.rename rast=frichness,frichness.tmp --o

    r.mapcalc "fgain=fgain.tmp + s$base.fgain"
    g.rename rast=fgain,fgain.tmp --o    

    # loss for all
    r.reclass in=s$base.start out=s$base.loss rules=rc.tables/loss.rc 

    r.mapcalc "loss=loss.tmp + s$base.loss"
    g.rename rast=loss,loss.tmp --o

    # clean
    g.mremove rast="s$base.*" -f
    g.mremove rast="s$base.*" -f
    g.remove rast=s$base  
  done

  rm -r rc.tables

cd ..
