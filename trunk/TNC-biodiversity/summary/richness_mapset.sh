# Batch job, to be started in current directory
  # which folder should I work in
  folder=$(g.gisenv -n | grep 'MAPSET' | cut -f2 -d'=' | cut -c2-)
  location=$(g.gisenv -n | grep 'LOCATION_NAME' | cut -f2 -d'=')
  gisdb=$(g.gisenv -n | grep 'GISDBASE' | cut -f2 -d'=')
  
  if [ $location == "lam_5k_c_2000_current" ]
  then
	loc=0
	r.mapcalc "richness=0"
  else
	loc=1
	r.mapcalc "richness.um=0"
	r.mapcalc "richness.nm=0"
	r.mapcalc "richness.re=0"
	r.mapcalc "richness.op=0"
	r.mapcalc "gain.um=0"
	r.mapcalc "gain.re=0"
	r.mapcalc "gain.op=0"
	r.mapcalc "loss.um=0"
	r.mapcalc "loss.nm=0"
	r.mapcalc "loss.re=0"
	r.mapcalc "loss.op=0"
  fi
  
  cd /data1/TNC/results/grass/$location/s$folder/cell
  
  # cycle through each species layer
  COUNT=1
  for lyr in *.pr
  do
    # species id
    base=$(echo $lyr | sed 's/.pr//' | sed 's/s//')

    # calculate cumulative richness (and others) grid
	if [ $loc -eq 0 ]
	then
		r.mapcalc "richness.tmp=richness+s$base.pa"
		g.remove rast=richness -f
		r.mapcalc "richness=richness.tmp"
		g.remove rast=richness.tmp
	else
		#richness
		#unlimited
		r.mapcalc "richness.um.tmp=richness.um+s$base.pa.um"
		g.remove rast=richness.um -f
		r.mapcalc "richness.um=richness.um.tmp"
		g.remove rast=richness.um.tmp
		
		#null
		r.mapcalc "richness.nm.tmp=richness.nm+s$base.pa.nm"
		g.remove rast=richness.nm -f
		r.mapcalc "richness.nm=richness.nm.tmp"
		g.remove rast=richness.nm.tmp
		
		#realistic
		r.mapcalc "richness.re.tmp=richness.re+s$base.pa.re"
		g.remove rast=richness.re -f
		r.mapcalc "richness.re=richness.re.tmp"
		g.remove rast=richness.re.tmp
		
		#optimistic
		r.mapcalc "richness.op.tmp=richness.op+s$base.pa.op"
		g.remove rast=richness.op -f
		r.mapcalc "richness.op=richness.op.tmp"
		g.remove rast=richness.op.tmp
		
		#gain
		#unlimited
		r.mapcalc "gain.um.tmp=gain.um+s$base.gn.um"
		g.remove rast=gain.um -f
		r.mapcalc "gain.um=gain.um.tmp"
		g.remove rast=gain.um.tmp
		
		#realistic
		r.mapcalc "gain.re.tmp=gain.re+s$base.gn.re"
		g.remove rast=gain.re -f
		r.mapcalc "gain.re=gain.re.tmp"
		g.remove rast=gain.re.tmp
		
		#optimistic
		r.mapcalc "gain.op.tmp=gain.op+s$base.gn.op"
		g.remove rast=gain.op -f
		r.mapcalc "gain.op=gain.op.tmp"
		g.remove rast=gain.op.tmp
		
		#loss
		#unlimited
		r.mapcalc "loss.um.tmp=loss.um+s$base.ls.um"
		g.remove rast=loss.um -f
		r.mapcalc "loss.um=loss.um.tmp"
		g.remove rast=loss.um.tmp
		
		#null
		r.mapcalc "loss.nm.tmp=loss.nm+s$base.ls.nm"
		g.remove rast=loss.nm -f
		r.mapcalc "loss.nm=loss.nm.tmp"
		g.remove rast=loss.nm.tmp
		
		#realistic
		r.mapcalc "loss.re.tmp=loss.re+s$base.ls.re"
		g.remove rast=loss.re -f
		r.mapcalc "loss.re=loss.re.tmp"
		g.remove rast=loss.re.tmp
		
		#optimistic
		r.mapcalc "loss.op.tmp=loss.op+s$base.ls.op"
		g.remove rast=loss.op -f
		r.mapcalc "loss.op=loss.op.tmp"
		g.remove rast=loss.op.tmp
	fi
	
	COUNT=$(echo $COUNT+1 | bc)
  done

  rm -r rc.tables

cd ..
