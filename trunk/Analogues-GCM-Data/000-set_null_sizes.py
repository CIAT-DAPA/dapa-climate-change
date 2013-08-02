# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Grid2Ascii.py <dirbase> <outdir> <variable>"
	print "   - ie: python set_null_sizes.py D:\cenavarro\admin-boundaries\GRID-files-continents-5min D:\cenavarro\Analogues_GCM_data\Test_sizes"
	sys.exit(1)

dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
# variable = sys.argv[3]

os.system('cls')
countrylist =  sorted(os.listdir(dirbase))
print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

gp.CheckOutExtension("Spatial")
print "\n---> Processing: " + dirbase

for country in countrylist:
	gp.workspace = dirbase + "\\" + country
	raster = gp.workspace + "\\" + country.split("_")[0] + "0"
	InExpression = 'setnull("' + raster + '")'
	print InExpression
	OutRaster = dirout + "\\" + country + "_10min"
	gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
	gp.RasterToASCII_conversion(OutRaster, dirout + "\\" + country + "_10min.asc")

print "Done!!!!"

	