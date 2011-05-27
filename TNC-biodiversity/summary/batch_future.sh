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

  null=10

  if [ $year = "2020_2049" ] 
  then
    realistic=8
    optimistic=4
  elif [ $year = "2040_2069" ]
  then
    realistic=6
    optimistic=2
  fi


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
  
    # Add distribution of the model to the stencil
    r.mapcalc "s$base.to=s$base.p + s$base.stencil.p@c$folder"

    # clean
    g.remove rast=s$base.p
    g.remove rast=s$base
  done
 
  rm -r rc.tables

cd ..
