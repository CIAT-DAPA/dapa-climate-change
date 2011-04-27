for sres in A1B A2
do
  for gcm in `ls ~/geodata3/climate_change/IPCC_CMIP3/SRES_$sres/downscaled/Global_5min/`
  do
    for year in 2020_2049 2040_2069
    do
       cp /home/johannes/geodata3/climate_change/IPCC_CMIP3/SRES_${sres}/downscaled/Global_5min/${gcm}/${year}/_asciis/bio_asc.zip ~/geodata2/TNC_global_plants/data/proj/${sres}_${year}_${gcm}.zip
       
    
    done
  done
done

for i in *.zip
do
  name=$(echo $i | sed 's/.zip//')
  mkdir $name
  unzip $i -d $name 
done

ls *.zip -1 | sed 's/.zip//' | sed 's/_/,/' | sed 's/_/,/2' > ~/models.csv


