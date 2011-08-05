#!/usr/bin/bash

# exprot gcms from GRASS

g.region n=35 s=-60 w=-118 e=-33 res=00:02:30


for i in A1B_2020 A1B_2040 A2_2020 A2_2040
do
  for b in {1..19}
  do
    r.out.arc in="${i}_bio${b}_mean" out="bio${b}.asc"
  done
  zip ${i}_mean.zip bio*.asc
  rm bio*.asc
done

for b in {1..19}
do
  gdal_translate -of 'AAIGrid' -projwin -118 35 -33 -60 bio/bio_$b/hdf.adf bio$b.asc
done
zip current.zip bio*.asc
rm bio*.asc
rm bio*.prj
