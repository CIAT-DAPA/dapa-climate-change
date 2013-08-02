# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: July 26th, 2012
# Purpose: Converts ESRI grid to asciis and tiff in tiles 
# ----------------------------------------------------------------------------------

def mainfunction(dirbase, dirout, country, sres, period, resolution, tiled):
	
	import arcgisscripting, os, sys, string, glob
	gp = arcgisscripting.create(9.3)
	
	# gp.CheckOutExtension("Spatial")
	# gp.toolbox="management"
	
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	print "  CONVERT TO ASCII AND TIFF	 "
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

	varlist = "bio", "dtr", "prec", "tmean"
	# modellist =  sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution))
	
	# for model in modellist:
	model = "current"
	for var in varlist:
		
		gp.workspace = dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var
		
		print "\n --> Processing: " + country,sres,model,period,var,"\n"
		
		# diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_asciis"
		# if not os.path.exists(diroutAscii):
			# os.system('mkdir ' + diroutAscii)
		
		diroutTiff = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"
		if not os.path.exists(diroutTiff):
			os.system('mkdir ' + diroutTiff)
		
		rasters = gp.ListRasters("*", "GRID")
		for raster in rasters:
			
			# OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
			OutTiff = diroutTiff + "\\" + sres + "_" + period + "_" + model + "_" + raster + ".tif"
			
			
			# if not gp.Exists(OutAscii):
				# gp.RasterToASCII_conversion(raster, OutAscii)
			if not os.path.exists(OutTiff):
				print "\tConverting " + raster
				os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet " + gp.workspace + "\\" + raster + " " + OutTiff)
				# gp.delete_management(raster)
			else:
				print "\tConverted " + raster
		# trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
		# for trashfile in trashList:
			# os.remove(trashfile)
		
		os.system("rmdir /s /q " + gp.workspace)

				
print " Process Grid to tif done!!!"    

