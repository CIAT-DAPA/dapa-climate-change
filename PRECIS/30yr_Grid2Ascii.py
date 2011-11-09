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
	print "   - ie: python 30yr_Grid2Ascii.py D:\climate_change\RCM_Data Baseline D:\Workspace\PRECIS_colombia_baseline"
	sys.exit(1)

dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

modellist = sorted(os.listdir(dirbase + "\\" + scenario))
# periodlist = "1961_1990", "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
period = "1961_1990"

for model in modellist:
	# for period in periodlist:
	gp.workspace = dirbase + "\\" + scenario + "\\" + model + "\\30yrAverages\\" + period
	diroutAsciis = dirout + "\\rcm_" + model
	if not os.path.exists(diroutAsciis):
		os.system('mkdir ' + diroutAsciis)
		
	print "\n---> Processing: " + gp.workspace
	for month in range(1, 12+1, 1):
	# rasters = gp.ListRasters("*", "GRID")
	# for raster in rasters:
		# print raster
		if month < 10:
			raster = gp.workspace + "\\prec_0" + str(month)
			OutAscii = diroutAsciis + "\\" + os.path.basename(raster) + ".asc"
			if not os.path.exists(OutAscii):
				gp.RasterToASCII_conversion(raster, OutAscii)
				# variable = os.path.basename(raster).split("_")[0]
				# InZip = diroutAsciis + "\\" + variable + "_asc.zip"
				# print "Compressing " + InZip
				# os.system('7za a ' + InZip + " " + OutAscii)
				# os.remove(OutAscii)
			
			raster = gp.workspace + "\\tmean1_5_0" + str(month)
			OutAscii = diroutAsciis + "\\tmean_0" + str(month) + ".asc"
			if not os.path.exists(OutAscii):
				gp.RasterToASCII_conversion(raster, OutAscii)

			
		if month > 9:
			raster = gp.workspace + "\\prec_" + str(month)
			OutAscii = diroutAsciis + "\\" + os.path.basename(raster) + ".asc"
			if not os.path.exists(OutAscii):
				gp.RasterToASCII_conversion(raster, OutAscii)
				# variable = os.path.basename(raster).split("_")[0]
				# InZip = diroutAsciis + "\\" + variable + "_asc.zip"
				# print "Compressing " + InZip
				# os.system('7za a ' + InZip + " " + OutAscii)
				# os.remove(OutAscii)
			
			raster = gp.workspace + "\\tmean1_5_" + str(month)
			OutAscii = diroutAsciis +  "\\tmean_" + str(month) + ".asc"
			if not os.path.exists(OutAscii):
				gp.RasterToASCII_conversion(raster, OutAscii)
			
print "Done!!!!"

	