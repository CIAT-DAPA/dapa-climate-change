for sres in A1B A2
do
  for gcm in `ls /mnt/GIS-HD722/climate_change/IPCC_CMIP3/SRES_$sres/downscaled/Global_2_5min/`
  do
    for year in 2020_2049 2040_2069
    do
       cp /mnt/GIS-HD722/climate_change/IPCC_CMIP3/SRES_${sres}/downscaled/Global_5min/${gcm}/${year}/_asciis/bio_asc.zip ./${sres}_${year}_${gcm}.zip
    done
  done
done


