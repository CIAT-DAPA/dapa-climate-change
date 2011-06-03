# Batch job, to be started in current directory
  # which folder should I work in
  folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=' | sed 's/s//')
  location=$(g.gisenv -n | grep 'LOCATION_NAME' | cut -f2 -d'=' | sed 's/s//')
  gisdb=$(g.gisenv -n | grep 'GISDBASE' | cut -f2 -d'=')

# go to folder
  cd /data/TNC/results/la_5k_$location/$folder

  # make folder for reclassification tables  
  mkdir rc.tables  

  # symlink to current of the mapset
  # prefix symlinked mapset with c, for current
  ln -s $gisdb/current/s$folder $gisdb/$location/c$folder

  # figure out which year we are treating

  year=$(echo $location | awk 'BEGIN{FS=OFS="_"}{print $2,$3}')

  # setting thresholds according to the year of the SRES
  # each sres correspongs to a value in the [spid].stencil.b 
  # 10 = current + chull (300km) = null adaption
  # 8 = current + chull (300km) + 6 km = 2030 realistic
  # 6 = current + chull (300km) + 8 km = 2050 realistic
  # 4 = current + chull (300km) + 30 km = 2030 optimistic
  # 2 = current + chull (300km) + 40 km = 2050 optimistic

  null_gain="101"
  null_loss="102"
  null_richness="101 103"

  if [ $year = "2020_2049" ] 
  then
    realistic_gain="81 101"
    realistic_loss="82 102"
    realistic_richness="81 83 101 103"
    optimistic_gain="41 61 81 101"
    optimistic_loss="42 62 82 102"
    optimistic_richness="41 43 61 63 81 83 101 103"
  elif [ $year = "2040_2069" ]
  then
    realistic_gain="61 81 101"
    realistic_loss="62 82 102"
    realistic_richness="61 63 81 83 101 103"
    optimistic_gain="21 41 61 81 101"
    optimistic_loss="22 42 62 82 102"
    optimistic_richness="21 23 41 43 61 63 81 83 101 103"
  fi

  # Create reclass tables for gain, loss and richness
  echo -e "$realistic_gain = 1 \n * = 0 " > rc.tables/realistic_gain.rc 
  echo -e "$realistic_loss = 1 \n * = 0 " > rc.tables/realistic_loss.rc 
  echo -e "$realistic_richness = 1 \n * = 0 " > rc.tables/realistic_richness.rc 
  echo -e "$optimistic_gain = 1 \n * = 0 " > rc.tables/optimistic_gain.rc 
  echo -e "$optimistic_loss = 1 \n * = 0 " > rc.tables/optimistic_loss.rc 
  echo -e "$optimistic_richness = 1 \n * = 0 " > rc.tables/optimistic_richness.rc 
  echo -e "$null_gain = 1 \n * = 0 " > rc.tables/null_gain.rc 
  echo -e "$null_loss = 1 \n * = 0 " > rc.tables/null_loss.rc 
  echo -e "$null_richness = 1 \n * = 0 " > rc.tables/null_richness.rc 

  r.mapcalc "rrichness.tmp=0"
  r.mapcalc "rgain.tmp=0"
  r.mapcalc "rloss.tmp=0"
  r.mapcalc "orichness.tmp=0"
  r.mapcalc "ogain.tmp=0"
  r.mapcalc "oloss.tmp=0"
  r.mapcalc "nrichness.tmp=0"
  r.mapcalc "ngain.tmp=0"
  r.mapcalc "nloss.tmp=0"


  # cycle through each tif
  for tif in *.tif
  do
    # species id
    base=$(echo $tif | sed 's/.tif//')


    # getting threshold value (already rescaled to 1 - 256)
    threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")

    # register raste rin GRASS
    r.in.gdal in=$base.tif out=s$base -o 

    # create reclassification table (0 absence, 2 presence, 2 is used so it can be used in the turn over calculation 
    echo -e "0 thru $threshold = 0 \n$threshold thru 300 = 1 " > rc.tables/$base.rc 

    # reclass
    r.reclass in=s$base out=s$base.p rules=rc.tables/$base.rc --o
  
    # Add presence of the model to the stencil
    r.mapcalc "s$base.to=s$base.p + s$base.stencil@c$folder"

    # Reclass for gain, loss and richness
    echo -e "$realistic_gain = 1 \n * = 0 " > rc.tables/realistic_gain.rc 
    echo -e "$realistic_loss = 1 \n * = 0 " > rc.tables/realistic_loss.rc 
    echo -e "$realistic_richness = 1 \n * = 0 " > rc.tables/realistic_richness.rc 
    echo -e "$optimistic_gain = 1 \n * = 0 " > rc.tables/optimistic_gain.rc 
    echo -e "$optimistic_loss = 1 \n * = 0 " > rc.tables/optimistic_loss.rc 
    echo -e "$optimistic_richness = 1 \n * = 0 " > rc.tables/optimistic_richness.rc 
    echo -e "$null_gain = 1 \n * = 0 " > rc.tables/null_gain.rc 
    echo -e "$null_loss = 1 \n * = 0 " > rc.tables/null_loss.rc 
    echo -e "$null_richness = 1 \n * = 0 " > rc.tables/null_richness.rc 

    r.reclass in=s$base.to out=s$base.to.rgain rules=rc.tables/realistic_gain.rc 
    r.reclass in=s$base.to out=s$base.to.rloss rules=rc.tables/realistic_loss.rc 
    r.reclass in=s$base.to out=s$base.to.rrichness rules=rc.tables/realistic_richness.rc 
    r.reclass in=s$base.to out=s$base.to.ogain rules=rc.tables/optimistic_gain.rc 
    r.reclass in=s$base.to out=s$base.to.oloss rules=rc.tables/optimistic_loss.rc 
    r.reclass in=s$base.to out=s$base.to.orichness rules=rc.tables/optimistic_richness.rc 
    r.reclass in=s$base.to out=s$base.to.ngain rules=rc.tables/null_gain.rc 
    r.reclass in=s$base.to out=s$base.to.nloss rules=rc.tables/null_loss.rc 
    r.reclass in=s$base.to out=s$base.to.nrichness rules=rc.tables/null_richness.rc 

    # add to total
    r.mapcalc "rgain=rgain.tmp + s$base.to.rgain"
    g.copy rast=rgain,rgain.tmp --o

    r.mapcalc "rloss=rloss.tmp + s$base.to.rloss"
    g.copy rast=rloss,rloss.tmp --o

    r.mapcalc "rrichness=rrichness.tmp + s$base.to.rrichness"
    g.copy rast=rrichness,rrichness.tmp --o

    r.mapcalc "ogain=ogain.tmp + s$base.to.ogain"
    g.copy rast=ogain,ogain.tmp --o

    r.mapcalc "oloss=oloss.tmp + s$base.to.oloss"
    g.copy rast=oloss,oloss.tmp --o

    r.mapcalc "orichness=orichness.tmp + s$base.to.orichness"
    g.copy rast=orichness,orichness.tmp --o

    r.mapcalc "ngain=ngain.tmp + s$base.to.ngain"
    g.copy rast=ngain,ngain.tmp --o

    r.mapcalc "nloss=nloss.tmp + s$base.to.nloss"
    g.copy rast=nloss,nloss.tmp --o

    r.mapcalc "nrichness=nrichness.tmp + s$base.to.nrichness"
    g.copy rast=nrichness,nrichness.tmp --o

    # clean
    g.remove rast=s$base.p
    g.remove rast=s$base
    g.mremove rast="s$base.to*" -f
    g.mremove rast="s$base.to*" -f
  done

  # remove temp rasters
  g.mremove rast="*.tmp" -f
 
  rm -r rc.tables

cd ..
