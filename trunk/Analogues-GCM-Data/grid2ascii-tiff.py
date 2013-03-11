# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: July 26th, 2012
# Purpose: Converts ESRI grid to asciis and tiff in tiles or not. 
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python grid2ascii-tiff.py F:\Analogues_GCM_data\ExtractByCountry F:\Analogues_GCM_data\ExtractByCountry_asciis all a1b 2020_2049 30s NO"
	sys.exit(1)

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

def mainfunction(dirout, dirout, country, sres, period, resolution, tiled):
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	print "  CONVERT TO ASCII AND TIFF	 "
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

	varlist = "bio", "dtr", "prec", "tmean"
	if country == "all":
		
		countrylist = "ala", "alb", "and", "are", "arm", "ata", "atg", "aut", "aze", "bdi", "bel", "ben", "bgr", "bhr", "bih", "blz", "bmu", "brb", "brn", "btn", "che", "com",\
					"cpv", "cri", "cyp", "cze", "dji", "dma", "dom", "est", "fro", "geo", "glp", "gmb", "gnb", "grd", "gtm", "guf", "hnd", "hti", "hun", "irl", "isr", "jam",\
					"jor", "kwt", "lbn", "lbr", "lca", "lie", "lka", "lso", "ltu", "lux", "lva", "mac", "mco", "mda", "mkd", "mnp", "mtq", "nld", "pan", "pri", "qat", "reu",\
					"rwa", "sle", "slv", "smr", "stp", "sur", "svk", "svn", "swz", "syc", "tgo", "ton", "twn", "vct", "wsm"
		# countrylist =  sorted(os.listdir(dirbase))
		for country in countrylist:
			
			if not gp.Exists(dirout + "\\SRES_" + sres + "\\" + country + "_grid2ascii-tiff_countries_done.txt"):
				modellist =  sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution))
				for model in modellist:
					
					if tiled == "NO":
						
						gp.workspace = dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + resolution + "\\" + model + "\\" + period
						for var in varlist:
							
							print "\n --> Processing: " + country,sres,model,period,var,"\n"
							
							diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_asciis"
							if not os.path.exists(diroutAscii):
								os.system('mkdir ' + diroutAscii)
							
							diroutTiff = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_tiffs"
							if not os.path.exists(diroutTiff):
								os.system('mkdir ' + diroutTiff)
							
							if var == "bio":
								rasters = gp.ListRasters("bio*", "GRID")
								for raster in rasters:
									
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
								
								# os.system("rmdir /s /q " + gp.workspace)
								
							else:
								rasters = gp.ListRasters(var + "*", "GRID")
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
										# gp.delete_management(raster)

								trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
								for trashfile in trashList:
									os.remove(trashfile)
								
								# os.system("rmdir /s /q " + gp.workspace)
								

			
				## Compressing country files
				print " \nCompressing country folder"
				os.system("7za a -mmt8 " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + ".zip " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution)
				os.system("rmdir /s /q " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution)
				print " \nCompression done!"
				
				print "\n",country," converted!"
				checkTXT = open(dirout + "\\SRES_" + sres + "\\" + country + "_grid2ascii-tiff_countries_done.txt", "w")
				checkTXT.close()
			else:
				print "\n",country," converted!"
		
		print "\nGrid to Ascii and Tiff done!!!"  
			
	else:

		modellist =  sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution))
		for model in modellist:
			
			if tiled == "NO":
				###This part is under construction
				if model == "current":
					gp.workspace = dirbase + "\\" + country + "\\" + model 
					
			else:

				for var in varlist:
					
					gp.workspace = dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var
					
					print "\n --> Processing: " + country,sres,model,period,var,"\n"
					
					diroutAscii = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_asciis"
					if not os.path.exists(diroutAscii):
						os.system('mkdir ' + diroutAscii)
					
					diroutTiff = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var + "_tiffs"
					if not os.path.exists(diroutTiff):
						os.system('mkdir ' + diroutTiff)
					
					if var == "bio":
						rasters = gp.ListRasters("*", "GRID")
						for raster in rasters:
							
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
								gp.delete_management(raster)

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

