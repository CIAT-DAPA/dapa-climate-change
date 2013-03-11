import os, sys, glob, string
# python rename_folder.py S:\data\portals\analogues-gcm-data\ExtractWorld_tiles_tiffs\sres_a1b\global_2_5min
dirbase = sys.argv[1]

modelist = sorted(os.listdir(dirbase))
for model in modelist:
	variablelist = sorted(os.listdir(dirbase + "\\" + model + "\\2020_2049"))
	for variable in variablelist:
		inname = dirbase + "\\" + model + "\\2020_2049\\" + variable
		outname = dirbase + "\\" + model + "\\2020_2049\\" + variable.split("_")[0] + "_tif" 
		if not variable.split("_")[1] == "tif": 
			print inname
			os.rename(inname, outname)
	