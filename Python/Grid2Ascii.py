# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Grid2Ascii.py <dirbase> <outdir> <variable>"
	print "   - ie: python Grid2Ascii.py D:\climate_change\Baseline\WC_20km D:\climate_change\Baseline\WC_Dataset_20km"
	sys.exit(1)

dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
# variable = sys.argv[3]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

gp.CheckOutExtension("Spatial")
gp.workspace = dirbase 
print "\n---> Processing: " + dirbase

gp.workspace = dirbase 
rasters = gp.ListRasters("tm*", "GRID")
for raster in rasters:
	print raster
	
	OutAscii = dirout + "\\" + raster + ".asc"
	if not gp.Exists(OutAscii):
		OutRaster = dirout + "\\" + raster
		# InExpression = (raster + " * 0.1")
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)   
		gp.RasterToASCII_conversion(raster, OutAscii)

print "Done!!!!"

	