# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: August 08th, 2014
# Purpose: Purpose: Cut by mask worldclim data and Extraction by mask of points
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

# python D:\_scripts\dapa\IPCC-CMIP3\Cut_ensemble_gcm.py S:\gcm\cmip3\downscaled\ensemble S:\admin_boundaries\grid_files\moz_adm\moz0 D:\Request\Request_cnavarro sres_a1b 2020_2049 YES


# Arguments
dirbase = sys.argv[1]
admdir = sys.argv[2]
dirout = sys.argv[3]
rcp = sys.argv[4]
period = sys.argv[5]
switch = sys.argv[6]


# Clean screen
os.system('cls')

gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK WORLDCLIM  "
print "~~~~~~~~~~~~~~~~~~~~~~"


endir = dirbase + "\\"  + rcp + "\\Global_30s\\"+period


variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
# variablelist = ["prec"]

worldclim = "S:\observed\gridded_products\worldclim\Global_30s"

for var in variablelist:
	if var == "bio":
		num = 19
	elif var == "cons_mths":
		num = 1	
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
				
			gp.workspace = diroutraster 	
			if not os.path.exists(diroutraster + "\\ascii"):
				os.system('mkdir ' + diroutraster + "\\ascii")	
			OutAscii = diroutraster + "\\ascii\\" + variable + ".asc"	
			if not gp.Exists(OutAscii):	
				gp.RasterToASCII_conversion(bio, OutAscii)
				gp.AddMessage( "\t"+ " " +  variable+ " " + "converted" )
				
			InZip = diroutraster + "\\ascii\\" + var + "_asc.zip"
			os.system('7za a ' + InZip + " " + OutAscii)
			os.remove(OutAscii)				
			if os.path.exists(OutAscii[:-3]+"prj"):
				os.remove(OutAscii[:-3]+"prj")
			if gp.Exists(OutRaster) and switch == "YES":
				gp.delete_management(OutRaster)


				
			# Para cortar datos worldclim
			# dirworldclim = dirout + "\\worldclim\\global_30s"		
			# if not os.path.exists(dirworldclim):
				# os.system('mkdir ' + dirworldclim)			
			# base = worldclim+ "\\" + variable
			# outworldclim =dirworldclim+ "\\" + variable +'_wc'
			# if not gp.Exists(outworldclim):
				# print outworldclim
				# gp.ExtractByMask_sa(base, admdir, outworldclim)
			# else:
				# print "...ya existe worldclim!", variable

			##### Para calcular anomalias
			# dirouanoma = dirout + "\\anomalies\\"+period		
			# if not os.path.exists(dirouanoma):
				# os.system('mkdir ' + dirouanoma)			
			# gp.workspace = diroutraster
			# inrastertmax = diroutraster + "\\"+variable
			# inrastertmin = dirworldclim + "\\"+variable+'_wc'
			# inexpresion = inrastertmax + " - " + inrastertmin 
			# outAnomala= dirouanoma + "\\"+variable
			
			# if not gp.Exists(outAnomala):
				# print outAnomala
				# gp.SingleOutputMapAlgebra_sa(inexpresion, outAnomala)	
			# else:
				# print "...ya existe anomalia!", variable				
	print '...done',var
						
gp.AddMessage("\n \t ====> DONE!! <====")  