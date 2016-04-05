# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Join tiles
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 04_join_tiles.py X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\baseline\tropico\average X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\baseline\tropico\average\_grid"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")
os.system('cls')

print "\n~~~~ CutByCoordinates ~~~~\n"

gp.toolbox = "SA"
varlist = "prec", "tmax", "tmin"


for var in varlist:

	for mth in range(1, 12 + 1, 1):
		
		print var + "_" + str(mth)

		if not gp.Exists(dirout + "\\" + var + "_" + str(mth)):
			
			print "\tProcessing Tile 1"
			extent_t1 =  "-99.00833 -40.00833 -19.99999 -7.499996"
			# gp.Extent = extent_t1
			gp.workspace = dirbase + "\\tile-1"		
			if not gp.Exists(dirout + "\\" + var + "_" + str(mth) + "_t1"):
				gp.clip_management(var + "_" + str(mth), extent_t1, dirout + "\\" + var + "_" + str(mth) + "_t1")

			print "\tProcessing Tile 2"
			extent_t2 =  "-99.00833 -7.499994066 -19.99999 25.00834"
			# gp.Extent = extent_t2
			gp.workspace = dirbase + "\\tile-2"
			if not gp.Exists(dirout + "\\" + var + "_" + str(mth) + "_t2"):
				gp.clip_management(var + "_" + str(mth), extent_t2, dirout + "\\" + var + "_" + str(mth) + "_t2")
			
			print "\tProcessing Mosaic"			
			gp.workspace = dirout
			gp.MosaicToNewRaster_management(dirout + "\\" + var + "_" + str(mth) + "_t1;" + dirout + "\\" + var + "_" + str(mth) + "_t2", dirout, var + "_" + str(mth), "#", "16_BIT_SIGNED", "#", "1", "MEAN", "FIRST")

			# gp.delete_management(var + "_" + str(mth) + "_t1")
			# gp.delete_management(var + "_" + str(mth) + "_t2")
