# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Convierte asciis a grids en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Ascii2Grid.py D:\climate_change\RCM_Data\SRES_A1B\HadCM3Q3\daily_grids\1959\SurPress"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

gp.workspace = dirbase 

# Get a list of grids in the workspace of each folder
print "\t ..listing grids into " + gp.workspace
ascList = glob.glob(gp.workspace + "\\*.asc")
for asc in ascList:
	print "Converting " + os.path.basename(asc)
	# os.system("gdal_translate -of AIG " + asc + " " + os.path.basename(asc)[:-4]
	gp.ASCIIToRaster_conversion(asc, os.path.basename(asc)[:-4], "FLOAT")
	
print "\t ..done!!"