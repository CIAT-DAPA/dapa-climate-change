#!/bin/bash

# check the import of data to grass (run this after running import_grass_*.sh
while read folder
do
	echo "doing folder $folder"
	MODEL=lam_5k_$1
	
	cd /data1/TNC/results/$MODEL/$folder
	
	for tif in *.tif
	do
		base=$(echo $tif | sed 's/.tif//')
		
		if [ $MODEL == "lam_5k_c_2000_current" ]
		then
			for FILE in s$base.bf s$base.pa s$base.th s$base.ch s$base.pr
			do
				COUNT=0
				if [ ! -f /data1/TNC/resutls/grass/$MODEL/$folder/cell/$file ]
				then
					COUNT=$(echo $COUNT+1 | bc)
					echo "$MODEL/$folder/$FILE...ok" >> /data1/TNC/lib/checkImport.lst
				else
					echo "$MODEL/$folder/$FILE...missing" >> /data1/TNC/lib/checkImport.lst
				fi
			done
		else
			for FILE in s$base.th s$base.pr s$base.pa.um s$base.pa.re s$base.pa.op s$base.pa.nm s$base.gn.um s$base.gn.re s$base.gn.op s$base.ls.um s$base.ls.re s$base.ls.op s$base.ls.nm
			do
				COUNT=0
				if [ ! -f /data1/TNC/resutls/grass/$MODEL/$folder/cell/$file ]
				then
					COUNT=$(echo $COUNT+1 | bc)
					echo "$MODEL/$folder/$FILE...ok" >> /data1/TNC/lib/success.lst
				else
					echo "$MODEL/$folder/$FILE...missing" >> /data1/TNC/lib/errors.lst
				fi
			done
		fi
	done
done
