# Batch job, to be started in current directory
  # which folder should I work in
  folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=' | sed 's/s//')
  
# go to folder
  cd /data/TNC/results/la_5k_c_2000_current/$folder

  # make folder were vrts are stored
  mkdir vrts

  # make folder for reclassification tables  
  mkdir rc.tables  

  # Cycle through all adapttation scenarios, buffer width in [m]
  # 300000 - null
  # 306000 - 2030 realistic   60 years a 100m (1975 - 2035)
  # 308000 - 2030 optimistic  60 years a 500m
  # 330000 - 2050 realistic   80 years a 100m
  # 340000 - 2050 optimistic  80 years a 500m


  # cycle through each tif
  for tif in *.tif
  do
    # species id
    base=$(echo $tif | sed 's/.tif//')

    # build gdal virtual grid
    gdalbuildvrt vrts/$base.vrt $base.tif

    # getting threshold value (already rescaled to 1 - 256)
    threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")

    for bufDis in 300000 306000 308000 330000 340000
    do

      if [ ! -d "vrts.chull.$bufDis" ]
      then
        mkdir "vrts.chull.$bufDis"
      fi

      # cut to chull according to buffer distance
      # transform to global mercator, perform the buffer of convex hulls and transform back
      gdalwarp -cutline "PG:dbname=gisdb user=model1" -csql "select ST_Transform(ST_Buffer(ST_Transform(geom,3395),$bufDis),4326) from convexhulls where speciesid='$base'" -q vrts/$base.vrt vrts.chull.$bufDis/$base.vrt

      # register raste rin GRASS
      r.in.gdal in=vrts.chull.$bufDis/$base.vrt out=s$base.$bufDis -o --o

      # create reclassification table (0 absence, 2 presence, 2 is used so it can be used in the turn over calculation 
      echo -e "0 thru $threshold = 0 \n$threshold thru 300 = 2 " > rc.tables/$base.p.rc 

      # create reclassification table (0 absence, 2 presence, 2 is used so it can be used in the turn over calculation 
      echo -e "0 = 0 \n1 thru 300 = 10 " > rc.tables/$base.b.rc 

      # reclass
      r.reclass in=s$base.$bufDis out=s$base.$bufDis.p rules=rc.tables/$base.p.rc --o

      # reclass
      r.reclass in=s$base.$bufDis out=s$base.$bufDis.b rules=rc.tables/$base.b.rc --o

    done

    r.mapcalc "s$base.stencil.p=s$base.300000.p + s$base.306000.p + s$base.308000.p + s$base.330000.p + s$base.340000.p"
    r.mapcalc "s$base.stencil.b=s$base.300000.b + s$base.306000.b + s$base.308000.b + s$base.330000.b + s$base.340000.b"
    
    # remove original maps
    g.mremove rast="s$base.3*" -f
    g.mremove rast="s$base.3*" -f

  done

  rm -r vrts* 
  rm -r rc.tables

cd ..
