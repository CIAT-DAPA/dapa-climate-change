# Batch job, to be started in current directory
  # which folder should I work in
  folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=')
  
# go to folder
  cd /data/TNC/results/la_5k_c_2000_current/$folder

  # make folder were vrts are stored
  mkdir vrts

  # make folder for reclassification tables  
  mkdir rc.tables  

  # make folder for convexhulls
  mkdir vrts.chull

  # Cycle through all adapttation scenarios, buffer width in [m]
  # 300000 - null
  # 305000 - 2030 realistic
  # 308000 - 2030 optimistic
  # 325000 - 2050 realistic
  # 340000 - 2050 optimistic 

  for bufDis in 300000 305000 308000 325000 340000
  do

    # mapset for cut unclassified grids
    g.mapset -c mapset=s$folder.$bufDis --quiet
    g.region latinamerica@PERMANENT

    # mapset for cut classified grids
    g.mapset -c mapset=s$folder.th.$bufDis --quiet
    g.region latinamerica@PERMANENT

    # cycle through each tif
    for tif in *.tif
    do
      # species id
      base=$(echo $tif | sed 's/.tif//')

      # build gdal virtual grid
      gdalbuildvrt vrts/$base.vrt $base.tif

      # cut to chull according to buffer distance
      gdalwarp -cutline "PG:dbname=gisdb user=model1" -csql "select ST_Transform(ST_Buffer(ST_Transform(geom,3395),$bufDis),4326) from convexhulls where speciesid='$base'" -q vrts/$base.vrt vrts.chull/$base.vrt

      # get the threshold value (may bis can be done somewhere else)
      threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")

      # register raste rin GRASS
      g.mapset mapset=s$folder.$bufDis --quiet
      r.external in=vrts.chull/$base.vrt out=s$base -o

      # create reclassification table (0 absence, 2 presence, 2 is used so it can be used in the turn over calculation 
      echo -e "0 thru $threshold = 0 \n$threshold thru 300 = 2 " > rc.tables/$base.rc

      
      g.mapset mapset=s$folder.th.$bufDis --quiet
      r.reclass in=s$base@s$folder.$bufDis out=s$base rules=rc.tables/$base.rc

    done
  done
rm -r vrts vrts.chull rc.tables
cd ..
