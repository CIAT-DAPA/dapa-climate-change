# Batch job, to be started in current directory
  # which folder should I work in
  folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=' | cut -c2-)
  location=$(g.gisenv -n | grep 'LOCATION_NAME' | cut -f2 -d'=')
  gisdb=$(g.gisenv -n | grep 'GISDBASE' | cut -f2 -d'=')

# go to folder
  cd /data1/TNC/results/$location/$folder

  # make folder for reclassification tables  
  mkdir rc.tables  

  # symlink to current of the mapset
  # prefix symlinked mapset with c, for current
  ln -s $gisdb/lam_5k_c_2000_current/s$folder $gisdb/$location/c$folder

  # figure out which year we are treating

  #year=$(echo $location | awk 'BEGIN{FS=OFS="_"}{print $4,$5}')

  # setting thresholds according to the year of the SRES
  # each sres correspongs to a value in the [spid].stencil.b 
  # 10 = current + chull (300km) = null adaption
  # 8 = current + chull (300km) + 6 km = 2030 realistic
  # 6 = current + chull (300km) + 8 km = 2050 realistic
  # 4 = current + chull (300km) + 30 km = 2030 optimistic
  # 2 = current + chull (300km) + 40 km = 2050 optimistic

  #if [ $year = "2020_2049" ] 
  #then
  #  realistic_richness="11 12"
  #  realistic_gain="12"
  #  optimistic_richness="11 12 13 14"
  #  optimistic_gain="12 13 14"
  #elif [ $year = "2040_2069" ]
  #then
  #  realistic_richness="11 12 13"
  #  realistic_gain="12 13"
  #  optimistic_richness="11 12 13 14 15"
  #  optimistic_gain="12 13 14 15"
  #fi

  # Create reclass tables for gain, loss and richness
  #echo -e "$realistic_gain = 1 \n * = 0 " > rc.tables/realistic_gain.rc 
  #echo -e "$realistic_richness = 1 \n * = 0 " > rc.tables/realistic_richness.rc 
  #echo -e "$optimistic_gain = 1 \n * = 0 " > rc.tables/optimistic_gain.rc 
  #echo -e "$optimistic_richness = 1 \n * = 0 " > rc.tables/optimistic_richness.rc 

  # null, full and loss dont change
  #echo -e "11 = 1 \n * = 0 " > rc.tables/null_richness.rc  # null has no gain
  #echo -e "10 11 12 13 14 15 = 1 \n * = 0 " > rc.tables/full_richness.rc 
  #echo -e "10 12 13 14 15 = 1 \n * = 0 " > rc.tables/full_gain.rc 
  #echo -e "1 = 1 \n * = 0 " > rc.tables/loss.rc 

  #r.mapcalc "rrichness.tmp=0"
  #r.mapcalc "rgain.tmp=0"
  #r.mapcalc "orichness.tmp=0"
  #r.mapcalc "ogain.tmp=0"
  #r.mapcalc "nrichness.tmp=0"
  #r.mapcalc "fgain.tmp=0"
  #r.mapcalc "frichness.tmp=0"
  #r.mapcalc "loss.tmp=0"


  # cycle through each tif
  for tif in *.tif
  do
    # species id
    base=$(echo $tif | sed 's/.tif//')


    # getting threshold value (already rescaled to 1 - 256)
    # threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")
    threshold=$(mysql --skip-column-names -umodel1 -pmaxent -hflora.ciat.cgiar.org -e"use tnc; select thresholdrs from species where species_id=$base;")

    # register raster in GRASS
    r.in.gdal in=$base.tif out=s$base.pr -o --o

    # create reclassification table (0 absence, 10 presence) 
    echo -e "0 thru $threshold = 0 \n$threshold thru 255 = 1 " > rc.tables/$base.rc 

    # reclass and calculate thresholded map
    r.reclass in=s$base.pr out=s$base.b rules=rc.tables/$base.rc --o
	r.mapcalc "s$base.th=s$base.b"
	g.remove rast=s$base.b -f

    # now we have the distribution stuff, so we need to calculate for each species:
	#
	# s$base.pa.um = presence(1)/absence(0) raster for unlimited migration (this equals the s$base.th map)
	# s$base.pa.nm = presence(1)/absence(0) raster for null migration (areas where future==1 and where current==1)
	# s$base.gn.um = 1 for pixels that are gained, 0 for all other pixels (for unlimited migration only)
	# s$base.ls.um = 1 for pixels that are lost, 0 for all other pixels (unlimited migration)
	# s$base.ls.nm = 1 for pixels that are lost, 0 for all other pixels (null migration)
	#
	#r.mapcalc "s$base.start=s$base.p + s$base.buf@c$folder"  
	
	# calculate presence/absence for unlimited migration
	r.mapcalc "s$base.pa.um=s$base.th"
	
	# calculate presence/absence for null migration
	r.mapcalc "s$base.pa.nm=if(s$base.pa.um==1 & s$base.pa@c$folder==1,1,0)"
	
	# calculate other migration scenarios as needed
	# 6000  - 2030 realistic   60 years at 100m (1975 - 2035)
	# 30000 - 2030 optimistic  60 years at 500m (1975 - 2035)
	# 8000  - 2050 realistic   80 years at 100m (1975 - 2055)
	# 40000 - 2050 optimistic  80 years at 500m (1975 - 2055)
	# only 8000 and 40000 will be used (as we are dealing with 2050 here only)
	
	# first with 8000, realistic (.re)
	r.mapcalc "$base.t1=if(s$base.pa.nm==0,null(),s$base.pa.nm)"
	r.buffer input=$base.t1 output=$base.t2 distances=8000 units=meters --overwrite
	r.mapcalc "$base.t3=if($base.t2>=0,1,0)"
	r.null map=$base.t3 null=0
	r.mapcalc "s$base.pa.re=if($base.t3==1 & s$base.pa.um==1,1,0)"
	g.mremove rast=$base.t3,$base.t2,$base.t1 -f
	
	# now optimistic with 40000 (.op)
	r.mapcalc "$base.t1=if(s$base.pa.nm==0,null(),s$base.pa.nm)"
	r.buffer input=$base.t1 output=$base.t2 distances=40000 units=meters --overwrite
	r.mapcalc "$base.t3=if($base.t2>=0,1,0)"
	r.null map=$base.t3 null=0
	r.mapcalc "s$base.pa.op=if($base.t3==1 & s$base.pa.um==1,1,0)"
	g.mremove rast=$base.t3,$base.t2,$base.t1 -f
	
	# calculate gain (for migration scenarios only), areas that are not suitable (0) in current but are in future (1)
	# IF curr==0 & fut==1, gain=1, else gain=0 ........ this happens in a pixel when a species is gained
	r.mapcalc "s$base.gn.um=if(s$base.pa.um==1 & s$base.pa@c$folder==0,1,0)"
    r.mapcalc "s$base.gn.re=if(s$base.pa.re==1 & s$base.pa@c$folder==0,1,0)"
	r.mapcalc "s$base.gn.op=if(s$base.pa.op==1 & s$base.pa@c$folder==0,1,0)"
	
	# calculate loss (for both scenarios), areas that are suitable (1) in current but are not in future (0)
	# IF curr==1 & fut==0, loss=1, else loss=0 ........ this happens in a pixel when a species is lost
	r.mapcalc "s$base.ls.um=if(s$base.pa.um==0 & s$base.pa@c$folder==1,1,0)"
	r.mapcalc "s$base.ls.re=if(s$base.pa.re==0 & s$base.pa@c$folder==1,1,0)"
	r.mapcalc "s$base.ls.op=if(s$base.pa.op==0 & s$base.pa@c$folder==1,1,0)"
	r.mapcalc "s$base.ls.nm=if(s$base.pa.nm==0 & s$base.pa@c$folder==1,1,0)"
	
	# rasters produced here are the ones listed below, per species:
	# 
	# s$base.pr = probabilities scaled to 0-255
	# s$base.th = thresholded probabilities (using ROC threshold)
	# s$base.pa.um = presence (1) and absence (0) of species, unlimited migration (equals s$base.th)
	# s$base.pa.re = presence (1) and absence (0) of species, realistic migration (8000 meters to 2050)
	# s$base.pa.op = presence (1) and absence (0) of species, optimistic migration (40000 meters to 2050)
	# s$base.pa.nm = presence (1) and absence (0) of species, null migration
	# s$base.gn.um = gain (1) else not gained (0), only for unlimited migration
	# s$base.gn.re = gain (1) else not gained (0), only for realistic migration
	# s$base.gn.op = gain (1) else not gained (0), only for optimistic migration
	# s$base.ls.um = loss (1) else not lost (0), unlimited migration
	# s$base.ls.re = loss (1) else not lost (0), realistic migration
	# s$base.ls.op = loss (1) else not lost (0), optimistic migration
	# s$base.ls.nm = loss (1) else not lost (0), null migration
	# 
	
  done

  rm -r rc.tables

cd ..
