# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Purpose: Purpose: Cut by mask worldclim data and Extraction by mask of points
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

# python D:\_scripts\dapa\IPCC-CMIP5\Cut_ensemble_gcm.py T:\gcm\cmip5\downscaled\ensemble S:\admin_boundaries\grid_files D:\Request\Request_peter rcp26


# Arguments
dirbase = sys.argv[1]
admdir = sys.argv[2]
dirout = sys.argv[3]
rcp = sys.argv[4]


# Clean screen
os.system('cls')

gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK WORLDCLIM  "
print "~~~~~~~~~~~~~~~~~~~~~~"


gp.workspace = dirbase + "\\"  + rcp + "\\Global_30s\\2020_2049" 
# gp.workspace = "D:\Request\Request_miguel\ensemble\rcp45\global_30s\2020_2049\col_adm" 


# Get a list of grids in the workspace

# variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
# variablelist = ["bio_1","bio_12"]
variablelist = ['tmin_1', 'tmin_2', 'tmin_3', 'tmin_4', 'tmin_5', 'tmin_6', 'tmin_7', 'tmin_8', 'tmin_9', 'tmin_10', 'tmin_11', 'tmin_12']

# admin = ["col_adm","per_adm","slv_adm","cri_adm","mex_adm","grd_adm"]
admin = ["tza_adm"]


worldclim = "S:\observed\gridded_products\worldclim\Global_30s"

			
# for variable in variablelist:
	# if variable == "bio":
		# num = 19
	# else:
		# num = 12
	# for month in range (1, num + 1, 1):
		# if variable == "cons_mths":
			# raster = gp.workspace + "\\" + variable
		# elif month == 1 or month == 12:
			# raster = gp.workspace + "\\" + variable + "_" + str(month)
			
		# gp.AddMessage("    Extracting " + os.path.basename(raster))
# bio1 = gp.workspace + "\\bio_1"
# bio12 = gp.workspace + "\\bio_12"


for variable in variablelist:
	for ad in admin:
		maskadmin = admdir + "\\" + ad+ "\\"+ ad.split("_")[0] + str(0)
		diroutraster = dirout + "\\ensemble\\" + rcp+ "\\global_30s\\2040_2069\\" + ad
		dirouanoma = dirout + "\\ensemble\\" + rcp+ "\\global_30s\\2040_2069\\"+ ad + "\\anomalies"
		dirworldclim = dirout + "\\worldclim\\global_30s"

		if not os.path.exists(diroutraster):
			os.system('mkdir ' + diroutraster)		
			
		if not os.path.exists(dirouanoma):
			os.system('mkdir ' + dirouanoma)
			
		if not os.path.exists(dirworldclim):
			os.system('mkdir ' + dirworldclim)			
		
		bio = gp.workspace + "\\" + variable
		OutRaster = diroutraster + "\\" + os.path.basename(bio)
		
		if not gp.Exists(OutRaster):
			print OutRaster
			gp.ExtractByMask_sa(bio, maskadmin, OutRaster)
		print "...ya existe enmsemble!"
		
		
		

		base = worldclim+ "\\" + variable
		outworldclim =dirworldclim+ "\\" + variable +'_wc'
		if not gp.Exists(outworldclim):
			print outworldclim
			gp.ExtractByMask_sa(base, maskadmin, outworldclim)
		print "...ya existe worldclim!"	
		
		enseAnoma = dirouanoma + "\\" + variable
		InExpression = '"'  +  OutRaster + '" - "' + base + '"' 
		# InExpression = "Int(Floor(" + OutRaster + " + " + base +" ))"
		# InExpression = "Int(Floor(" + OutRaster + " + 0.5))"
	
		
		# if not os.path.exists(enseAnoma):
		# print enseAnoma
		# if not gp.Exists(enseAnoma):
			# print InExpression, enseAnoma
			# gp.SingleOutputMapAlgebra_sa(InExpression, enseAnoma)	

diroutraster = dirout + "\\ensemble\\" + rcp+ "\\global_30s\\2040_2069\\" + admin[0]
dirouanoma = dirout + "\\ensemble\\" + rcp+ "\\global_30s\\2040_2069\\"+ admin[0] + "\\anomalies"
dirworldclim = dirout + "\\worldclim\\global_30s"			

for month in range(1, 12+1, 1):
	gp.workspace = diroutraster
	inrastertmax = diroutraster + "\\tmin_" + str(month)
	inrastertmin = dirworldclim + "\\tmin_" + str(month)+'_wc'
	# inexpresion = "Float("+"Float( " +  inrastertmax + " / 10 ) - Float( " + inrastertmin + " / 10 )"+")"
	# inexpresion = "( " +  inrastertmax + " / 10 ) + 0.5" + " - " + "( " + inrastertmin + " / 10 ) - 0.5"
	inexpresion = inrastertmax + " - " + inrastertmin 
	outAnomala= dirouanoma + "\\tmin_" + str(month)
	print inexpresion
	if not gp.Exists(outAnomala):
		gp.SingleOutputMapAlgebra_sa(inexpresion, outAnomala)				
		# gp.Float_sa(outAnomala, outAnomala+"_mod")

			
		
gp.AddMessage("\n \t ====> DONE!! <====")  