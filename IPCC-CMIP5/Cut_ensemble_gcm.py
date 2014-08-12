# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: August 08th, 2014
# Purpose: Purpose: Cut by mask worldclim data and Extraction by mask of points
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

# python F:\jetarapues\Request\Request_peter\Cut_ensemble_gcm.py T:\gcm\cmip5\downscaled\ensemble S:\admin_boundaries\grid_files\tza_adm\tza0 F:\jetarapues\Request\Request_peter rcp26 2040_2069
# python F:\jetarapues\Request\Request_peter\Cut_ensemble_gcm.py T:\gcm\cmip5\downscaled\ensemble F:\jetarapues\Request\Request_biofuturo\mask\col_narino F:\jetarapues\Request\Request_biofuturo rcp26 2020_2049


# Arguments
dirbase = sys.argv[1]
admdir = sys.argv[2]
dirout = sys.argv[3]
rcp = sys.argv[4]
period = sys.argv[5]


# Clean screen
os.system('cls')

gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK WORLDCLIM  "
print "~~~~~~~~~~~~~~~~~~~~~~"


endir = dirbase + "\\"  + rcp + "\\Global_30s\\"+period


variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]

worldclim = "S:\observed\gridded_products\worldclim\Global_30s"

for var in variablelist:
	if var == "bio":
		num = 19
	else:
		num = 12
	for month in range (1, num + 1, 1):
		if var == "cons_mths":
			variable = var
		else:
			variable = var + "_" + str(month)
			
			diroutraster = dirout + "\\ensemble\\" + rcp+ "\\global_30s\\"+period

			if not os.path.exists(diroutraster):
				os.system('mkdir ' + diroutraster)		
			
			bio = endir + "\\" + variable
			OutRaster = diroutraster + "\\" + os.path.basename(bio)
			
			if not gp.Exists(OutRaster):
				print OutRaster
				gp.ExtractByMask_sa(bio, admdir, OutRaster)
			else:	
				print "...ya existe enmsemble!",variable
			
			# Para cortar datos worldclim
			dirworldclim = dirout + "\\worldclim\\global_30s"		
			if not os.path.exists(dirworldclim):
				os.system('mkdir ' + dirworldclim)			
			base = worldclim+ "\\" + variable
			outworldclim =dirworldclim+ "\\" + variable +'_wc'
			if not gp.Exists(outworldclim):
				print outworldclim
				gp.ExtractByMask_sa(base, admdir, outworldclim)
			else:
				print "...ya existe worldclim!", variable

			# Para calcular anomalias
			dirouanoma = dirout + "\\anomalies\\"+period		
			if not os.path.exists(dirouanoma):
				os.system('mkdir ' + dirouanoma)			
			gp.workspace = diroutraster
			inrastertmax = diroutraster + "\\"+variable
			inrastertmin = dirworldclim + "\\"+variable+'_wc'
			inexpresion = inrastertmax + " - " + inrastertmin 
			outAnomala= dirouanoma + "\\"+variable
			
			if not gp.Exists(outAnomala):
				print outAnomala
				gp.SingleOutputMapAlgebra_sa(inexpresion, outAnomala)	
			else:
				print "...ya existe anomalia!", variable				
	print '...done',var
						
gp.AddMessage("\n \t ====> DONE!! <====")  