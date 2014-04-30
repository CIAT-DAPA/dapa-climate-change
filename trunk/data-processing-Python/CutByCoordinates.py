# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Extrae por mascara en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python CutByCoordinates.py S:\observed\gridded_products\worldclim\Global_30s S:\observed\gridded_products\worldclim\Lat_30s"
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

gp.workspace = dirbase
gp.toolbox = "SA"
gp.Extent = "-120 -56 -30 33"

varlist = "prec", "tmax", "tmin"

for var in varlist:
	for mth in range(1, 12 + 1, 1):
		print var + "_" + str(mth)
		OutAscii = dirout + "\\" + var + "_" + str(mth) + ".asc"
		gp.RasterToASCII_conversion(gp.workspace + "\\" + var + "_" + str(mth), OutAscii)

print "\t ..done!!"