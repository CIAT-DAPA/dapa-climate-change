# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: July 26th, 2012
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 006-grid2asciitiff_countries.py D:\cenavarro\Analogues_GCM_data\ExtractByCountry D:\cenavarro\Analogues_GCM_data\TilesByCountry a1b 2020_2049 10min"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
period = sys.argv[4]
resolution = sys.argv[5]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

# countrylist = "af", "as", "eu", "na", "oc", "sa"
countrylist = "cod", "kna", "mne", "nru", "plw", "pse", "srb", "ssd", "tls", "tto", "tuv", "ala", "alb", "and", "are", "arm", "ata", "atg", "aut", "aze", "bdi", "bel", "ben", "bgr", "bhr", "bih", "blz", "bmu", "brb", "brn", "btn", "che", "com",\
			"cpv", "cri", "cyp", "cze", "dji", "dma", "dom", "est", "fro", "geo", "glp", "gmb", "gnb", "grd", "gtm", "guf", "hnd", "hti", "hun", "irl", "isr", "jam",\
			"jor", "kwt", "lbn", "lbr", "lca", "lie", "lka", "lso", "ltu", "lux", "lva", "mac", "mco", "mda", "mkd", "mnp", "mtq", "nld", "pan", "pri", "qat", "reu",\
			"rwa", "sle", "slv", "smr", "stp", "sur", "svk", "svn", "swz", "syc", "tgo", "ton", "twn", "vct", "wsm"
varlist = "bio", "dtr", "prec", "tmean"
			
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "  CONVERT TO ASCII AND TIFF	 "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

for country in countrylist:

	if not os.path.exists(dirout + "\\SRES_" + sres + "\\" + country + "_grid2tiff_countries_done.txt"):
	
		modellist =  sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution))
		
		for model in modellist:
			
			if not model == "current":
				
				gp.workspace = dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution + "\\" + model + "\\" + period
				
				for var in varlist:
					
					print "\n --> Processing: " + country,sres,model,period,var,"\n"
					
					# diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_asciis"
					# if not os.path.exists(diroutAscii):
						# os.system('mkdir ' + diroutAscii)
					
					diroutTiff = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"
					if not os.path.exists(diroutTiff):
						os.system('mkdir ' + diroutTiff)
					
					rasters = gp.ListRasters(var + "*", "GRID")
					for raster in rasters:
						
						# OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
						OutTiff = diroutTiff + "\\" + sres + "_" + period + "_" + model + "_" + raster + "_0.tif"
						
						print "\tConverting " + raster
						# if not gp.Exists(OutAscii):
							# gp.RasterToASCII_conversion(raster, OutAscii)
						if not os.path.exists(OutTiff):
							os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet " + gp.workspace + "\\" + raster + " " + OutTiff)
							# gp.delete_management(raster)

					# trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
					# for trashfile in trashList:
						# os.remove(trashfile)
					
		## Compressing country files
		print " \nCompressing country folder"
		os.system("7za a -mmt12 " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution + ".zip " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution)
		os.system("rmdir /s /q " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution)
		print " \nCompression done!"
		
		checkTXT = open(dirout + "\\SRES_" + sres + "\\" + country + "_grid2tiff_countries_done.txt", "w")
		checkTXT.close()
	
		print " Process Grid to tif done!!!"

	else:
		print " Process Grid to tif done!!!"
		
				
print " Process Grid to tif done!!!"    

