# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: July 26th, 2012
# Purpose: Converts ESRI grid to asciis and tiff in tiles or not. 
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 007-grid2asciitiff_tiles_baseline.py D:\cenavarro\Analogues_GCM_data\TilesByCountry D:\cenavarro\Analogues_GCM_data\TilesByCountry baseline 1960_1990 10min YES"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
period = sys.argv[4]
resolution = sys.argv[5]
tiled = sys.argv[6]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "  CONVERT TO ASCII AND TIFF	 "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

varlist = "bio", "dtr", "prec", "tmean"
model = "current"

countrylist = "af", "eu", "na", "oc", "sa" #"as", 
# countrytilelist = "afg", "ago", "arg", "aus", "bfa", "bgd", "bhs", "blr", "bol", "bra", "bwa", "caf", "can", "chl", "chn", "civ", "cmr", "cog", "col",\
				# "cub", "deu", "dza", "ecu", "egy", "eri", "esh", "esp", "eth", "fin", "fji", "fra", "gab", "gbr", "gha", "gin", "gnq", "grc", "grl",\
				# "guy", "hrv", "idn", "ind", "irn", "irq", "isl", "ita", "jpn", "kaz", "ken", "kgz", "khm", "kir", "kor", "lao", "lby", "mar", "mdg",\
				# "mdv", "mex", "mli", "mlt", "mmr", "mng", "moz", "mrt", "mus", "mwi", "mys", "nam", "ncl", "ner", "nga", "nor", "npl",  "omn",\
				# "pak", "per", "phl", "png", "pol", "prk", "prt", "pry", "pyf", "rom",  "sau", "sdn", "sen", "sgp", "sjm", "slb", "som", "swe",\
				# "syr", "tca", "tcd", "tha", "tjk", "tkm", "tun", "tur", "tza", "uga", "ukr", "ury",  "uzb", "ven", "vnm", "vut", "yem", "zaf",\
				# "zmb", "zwe", "nzl", "rus", "usa"
				
# countrylist = "ala", "alb", "and", "are", "arm", "ata", "atg", "aut", "aze", "bdi", "bel", "ben", "bgr", "bhr", "bih", "blz", "bmu", "brb", "brn", "btn", "che", "com",\
			# "cpv", "cri", "cyp", "cze", "dji", "dma", "dom", "est", "fro", "geo", "glp", "gmb", "gnb", "grd", "gtm", "guf", "hnd", "hti", "hun", "irl", "isr", "jam",\
			# "jor", "kwt", "lbn", "lbr", "lca", "lie", "lka", "lso", "ltu", "lux", "lva", "mac", "mco", "mda", "mkd", "mnp", "mtq", "nld", "pan", "pri", "qat", "reu",\
			# "rwa", "sle", "slv", "smr", "stp", "sur", "svk", "svn", "swz", "syc", "tgo", "ton", "twn", "vct", "wsm"
# countrylist = "cod", "kna", "mne", "nru", "plw", "pse", "srb", "ssd", "tls", "tto", "tuv"
			
if tiled == "NO":

	for country in countrylist:
		
		if not gp.Exists(dirout + "\\" + sres + "\\" + country + "_" + resolution + "_grid2tiff_countries_done.txt"):
			
			gp.workspace = dirbase + "\\Baseline\\" + country + "_" + resolution + "\\" + model
			for var in varlist:
				
				print "\n --> Processing: " + country,sres,model,period,var,"\n"
				
				# diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_asciis"
				# if not os.path.exists(diroutAscii):
					# os.system('mkdir ' + diroutAscii)
				
				diroutTiff = dirout + "\\" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"
				
				if not os.path.exists(diroutTiff):
					os.system('mkdir ' + diroutTiff)
				
				rasters = gp.ListRasters(var + "*", "GRID")
				for raster in rasters:
					
					# OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
					OutTiff = diroutTiff + "\\" + sres + "_" + period + "_" + model + "_" + raster + "_0.tif"
					
					print "\tConverting " + raster
					# if not gp.Exists(OutAscii):
						# gp.RasterToASCII_conversion(raster, OutAscii)
					
					if not gp.Exists(OutTiff):
						os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet -a_nodata -9999 " + gp.workspace + "\\" + raster + " " + OutTiff)
						
				# os.system("rmdir /s /q " + gp.workspace)
		
			checkTXT = open(dirout + "\\" + sres + "\\" + country + "_" + resolution + "_grid2tiff_countries_done.txt", "w")
			checkTXT.close()
	

	print " Process Grid to tif done!!!"
	
else:

	# outFile = open(dirout + "\\error.txt", "a")
	# outFile.write("sres" + "\t" + "country" + "\t" + "raster"+ "\n")
	# outFile.close()
	countrytilelist = sorted(os.listdir(dirbase + "\\" + sres))
	for countryname in countrytilelist:
		country = countryname.split("_")[0]
		resolution = countryname.split(country)[1][1:]
		if not gp.Exists(dirout + "\\" + sres + "\\" + countryname + "_grid2tiff_countries_done.txt"):
			
			for var in varlist:
				
				gp.workspace = dirout + "\\" + sres + "\\" + countryname + "\\" + model + "\\" + var
				print gp.workspace 
				print "\n --> Processing: " + countryname,sres,model,period,var,"\n"
				
				# diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_asciis"
				# if not os.path.exists(diroutAscii):
					# os.system('mkdir ' + diroutAscii)
				
				diroutTiff = dirout + "\\" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"
				if not os.path.exists(diroutTiff):
					os.system('mkdir ' + diroutTiff)
				
				rasters = gp.ListRasters("*", "GRID")
				for raster in rasters:
					
					# OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
					OutTiff = diroutTiff + "\\" + sres + "_" + period + "_" + model + "_" + raster + ".tif"
										
					print "\tConverting " + raster
					# if not gp.Exists(OutAscii):
						# gp.RasterToASCII_conversion(raster, OutAscii)
					
					if not os.path.exists(OutTiff):
						os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet " + gp.workspace + "\\" + raster + " " + OutTiff)
						if not os.path.exists(OutTiff):
							# outFile = open(dirout + "\\error.txt", "a")
							# outFile.write(sres + "\t" + country + "\t" + raster + "\n")
							# outFile.close()
							
							checkTXT = open(dirout + "\\" + sres + "\\" + countryname + "_grid2tiff_countries_done.txt", "w")
						gp.delete_management(raster)
						
				os.system("rmdir /s /q " + gp.workspace)
		
			checkTXT = open(dirout + "\\" + sres + "\\" + country + "_grid2tiff_countries_done.txt", "w")
			checkTXT.close()

	print " Process Grid to tif done!!!"