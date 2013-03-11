# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: July 26th, 2012
# Purpose: Converts ESRI grid to asciis and tiff in tiles or not. 
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

# #Syntax
# if len(sys.argv) < 1:
	# os.system('cls')
	# print "\n Too few args"
	# print "   - ie: python grid2asciitiff_world.py G:\jtarapues\Global\tile_global G:\jtarapues\tile_global Global a2 2020_2049 2_5min YES"
	# sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
country = sys.argv[3]
sres = sys.argv[4]
period = sys.argv[5]
resolution = sys.argv[6]
tiled = sys.argv[7]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"


print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "  CONVERT TO ASCII AND TIFF	 "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

varlist = "bio", "dtr", "prec", "tmean"

modellist =  sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution))
for model in modellist:
	
	if tiled == "NO":
		###This part is under construction
		if model == "current":
			gp.workspace = dirbase + "\\" + country + "\\" + model 
			
	else:

		for var in varlist:
			
			gp.workspace = dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var
			
			print "\n --> Processing: " + gp.workspace, country,sres,model,period,var,"\n"
			
			diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_asciis"
			if not os.path.exists(diroutAscii):
				os.system('mkdir ' + diroutAscii)
			
			diroutTiff = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tiffs"
			if not os.path.exists(diroutTiff):
				os.system('mkdir ' + diroutTiff)
			
			if var == "bio":
				
				rasters = gp.ListRasters("bio*", "GRID")
				for raster in rasters:
					print raster
					
					if model == "current":
						OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
						OutTiff = diroutTiff + "\\" + model + "_" + raster + "_1.tif"
					else: 
						OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
						OutTiff = diroutTiff + "\\" + model + "_" + raster + "_1.tif"
					
					print "\tConverting " + raster
					if not gp.Exists(OutAscii):
						gp.RasterToASCII_conversion(raster, OutAscii)
					if not gp.Exists(OutTiff):
						os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet " + gp.workspace + "\\" + raster + " " + OutTiff)
						# gp.delete_management(raster)

				trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
				for trashfile in trashList:
					os.remove(trashfile)
				
				os.system("rmdir /s /q " + gp.workspace)
				
			else:
				rasters = gp.ListRasters("*", "GRID")
				for raster in rasters:
					
					if model == "current":
						OutAscii = diroutAscii + "\\" + model + "_" + raster + ".asc"
						OutTiff = diroutTiff + "\\" + model + "_" + raster + ".tif"
					else:
						OutAscii = diroutAscii + "\\" + sres + "_" + period + "_" + model + "_" + raster + ".asc"
						OutTiff = diroutTiff + "\\" + sres + "_" + period + "_" + model + "_" + raster + ".tif"
					
					print "\tConverting " + raster
					if not gp.Exists(OutAscii):
						gp.RasterToASCII_conversion(raster, OutAscii)
					if not gp.Exists(OutTiff):
						os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet " + gp.workspace + "\\" + raster + " " + OutTiff)
						gp.delete_management(raster)

				trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
				for trashfile in trashList:
					os.remove(trashfile)
				
				os.system("rmdir /s /q " + gp.workspace)
					
print " Grid to Ascii and Tiff done!!!"    

