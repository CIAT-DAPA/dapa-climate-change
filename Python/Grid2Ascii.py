# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Grid2Ascii.py <dirbase> <outdir> <variable>"
	print "   - ie: python Grid2Ascii.py M:\climate_change\IPCC_CMIP3\SRES_A2\downscaled\Global_30s\mpi_echam5\2060_2089 D:\Workspace\Data_Request\GCM_Christina_2 bio"
	sys.exit(1)

dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
variable = sys.argv[3]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

gp.workspace = dirbase 
print "\n---> Processing: " + dirbase

rasters = gp.ListRasters(variable + "_*", "GRID")
for raster in rasters:
	print raster
	
	OutRaster = dirout + "\\" + raster
	gp.clip_management(raster," -30 30 50 90 ",OutRaster)
	
	OutAscii = dirout + "\\" + raster + ".asc"
	gp.RasterToASCII_conversion(OutRaster, OutAscii)
	InZip = dirout + "\\" + variable + ".asc.gz"
	#os.system('7za a ' + InZip + " " + OutAscii)
	gp.delete_management(OutRaster)
	#os.remove(OutAscii)
	
print "Done!!!!"

	