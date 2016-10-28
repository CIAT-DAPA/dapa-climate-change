# Author: Jaime Tarapues

import os, sys, glob, string, shutil, errno

# python rename_files.py S:\observed\gridded_products\worldclim\Global_30s_v2
 
dirbase = sys.argv[1]

os.chdir(dirbase)
lista = glob.glob("*.tif")
for w in lista:

	print "renombrando: ", w
	var = os.path.basename(w).split("_")[2]
	mth = os.path.basename(w).split("_")[3][:-4]
	
	os.rename(w, var + "_" + str(int(mth)) + ".tif")

