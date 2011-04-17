for sres in A1B 
do
  for gcm in `ls ~/geodata3/climate_change/IPCC_CMIP3/SRES_$sres/downscaled/Global_10min/`
  do
    for year in 2020_2049 
    do
       for var in tmean tmin tmax prec
       do
          for month in $(seq 1 12)
          do
            cp -R /home/johannes/geodata3/climate_change/IPCC_CMIP3/SRES_${sres}/downscaled/Global_5min/${gcm}/${year}/${var}_${month} ~/Desktop/ccafs.data/${sres}_${year}_${gcm}_${var}_${month}
            #echo $sres\_$year\_$gcm.zip > ~/geodata2/TNC_global_plants/data/proj/$sres\_$year\_$gcm.zip
         done 
      done
    done
  done
done

# In GRASS
g.region res=00:10
for i in `ls`
do
   r.in.gdal in=$i out=$i
done

g.region res=1
for i in `g.mlist type=rast pattern="*dtr*"`
do
   r.mapcalc "tmp=$i/10"
   r.mapcalc $i=tmp
done

# calculte dtr
for sres in A1B 
do
  for gcm in `ls ~/geodata3/climate_change/IPCC_CMIP3/SRES_$sres/downscaled/Global_10min/`
  do
    for year in 2020_2049 
    do
       for month in $(seq 1 12)
       do
          r.mapcalc "${sres}_${year}_${gcm}_dtr_${month} = ${sres}_${year}_${gcm}_tmax_${month} - ${sres}_${year}_${gcm}_tmin_${month}"
      done
    done
  done
done

g.mremove rast="*_tmin*"
g.mremove rast="*_tmax*"

# export as asciis
g.region res=1
for sres in A1B 
do
  for gcm in `cat ../gcms.txt`
  do
    for year in 2020_2049 
    do
       for var in tmean dtr prec
       do
          for month in $(seq 1 12)
          do
            r.out.gdal in=${sres}_${year}_${gcm}_${var}_${month} out=`echo ${sres}_${year}_${gcm}_${var}_${month} | tr [:upper:] [:lower:]`.tif
         done 
      done
    done
  done
done

for i in *.asc
do
   mv $i `echo $i | sed 's/2020_2049/2030/'`
done


# import 
for i in *.bil
do
   r.in.bin byte=2 anull=-9999 north=90 south=-60 east=180 west=-180 rows=900 cols=2160 in=$i out=$i -s --o
done 

for i in `g.mlist type=rast pattern="t*.bil"`
do
   r.mapcalc "current_`echo $i | cut -f1 -d.`=$i/10"
   g.remove rast=$i
done

for i in $(seq 1 12)
do
   g.rename rast=prec$i.bil,current_prec$i
   g.remove rast=prec$i.bil
done
   

for i in $(seq 1 12)
do
   r.mapcalc "current_dtr$i=current_tmax$i-current_tmin$i"
done


# export current
:wg.region res=1
for i in $(seq 1 12)
do
   r.out.arc in=current_prec$i out=current_prec_$i.asc
   r.out.arc in=current_dtr$i out=current_dtr_$i.asc
   r.out.arc in=current_tmean$i out=current_tmean_$i.asc
done

