#!/bin/bash

# to be run within GRASS seesion

# import to grass
for i in *.zip
do
  unzip -o $i
  dir=$(echo $i | cut -f1 -d.)
  for j in bio*
  do
    r.in.gdal in=$j out=${dir}_${j} -o
    rm $j
  done
  rm $dir
done

# create averages
for i in A1B_2020 A1B_2040 A2_2020 A_2040
do
  r.series input="`g.mlist pattern='${i}*' sep=,`" output=${i}_mean,${i}_min,${i}_max,${i}_stdev method=average,minimum,maximum,stddev
done
