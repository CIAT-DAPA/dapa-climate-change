# ------------------------------------------------------------------------------------------------
# Description: Convert PRECIS 30yr averages Grids to ESRI-ASCIIs
# Author: Carlos Navarro
# Date: 21/06/11
# ------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Grid2Ascii.py <dirbase> <outdir> <variable>"
	print "   - ie: python 30yr_Grid2Ascii.py L:\climate_change\RCM_Data A1B D:\climate_change\RCM_Data"
	sys.exit(1)

dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario))
periodlist = "1961_1990", "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"

for model in modellist:
	for period in periodlist:
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + model + "\\30yrAverages\\" + period
		diroutAsciis = dirout + "\\SRES_" + scenario + "\\" + model + "\\30yrAverages_asciis\\" + period
		
		print "\n---> Processing: " + gp.workspace
		rasters = gp.ListRasters("*", "GRID")
		for raster in rasters:
			print raster
			
			if not os.path.exists(diroutAsciis):
				os.system('mkdir ' + diroutAsciis)
			
			OutAscii = diroutAsciis + "\\" + raster + ".asc"
			if not os.path.exists(OutAscii):
				gp.RasterToASCII_conversion(raster, OutAscii)
				variable = os.path.basename(raster).split("_")[0]
				InZip = diroutAsciis + "\\" + variable + "_asc.zip"
				print "Compressing " + InZip
				os.system('7za a ' + InZip + " " + OutAscii)
				os.remove(OutAscii)
			
print "Done!!!!"

	