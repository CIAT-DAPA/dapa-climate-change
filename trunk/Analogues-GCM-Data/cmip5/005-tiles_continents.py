# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Create tile from GCM data
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string, glob, grid2asciitiff_tiles
from arcpy import env

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 005-tiles_continents.py U:\portals\ccafs-analogues\grid_files rcp26 U:\portals\ccafs-analogues\TilesByCountry 2_5min 2020_2049"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
rcp = sys.argv[2]
dirout = sys.argv[3]
resolution = sys.argv[4]
period = sys.argv[5]

os.system('cls')
countrytilelist = "af", "as", "eu", "na", "oc", "sa"

if resolution == "2_5min":
	countryDic = {"af": "4 4 ", "as": "6 6 ", "eu": "3 3 ", "na": "5 5 ", "oc": "3 3 ", "sa": "4 4 "}
elif resolution == "5min":
	countryDic = {"af": "2 2 ", "as": "3 3 ", "eu": "2 2 ", "na": "3 3 ", "oc": "2 2 ", "sa": "2 2 "}
elif resolution == "10min":
	countryDic = {"af": "1 1 ", "as": "2 2 ", "eu": "1 1 ", "na": "1 1 ", "oc": "1 1 ", "sa": "1 1 "}
				
for country in countrytilelist:
	# country = "af"
	out = dirout + "\\" + rcp +"\\"+ country + "_" + str(resolution)
	checkFile = out + "_tiles_countinents_done.txt"
	if not os.path.exists(checkFile):
		
		input = dirbase + "\\" + rcp +"\\"+ country + "_" + str(resolution)
		modellist = sorted(os.listdir(input))
		
		print "~~~~~~~~~~~~~~~~~~~"
		print "  TILES COUNTRIES  "
		print "~~~~~~~~~~~~~~~~~~~"
		
		for model in modellist:
			if model == "current":
				arcpy.env.workspace = input + "\\" + model
				diroutGrids = dirout + "\\baseline\\" + country + "_" + str(resolution) + "\\" + model
			else:
				arcpy.env.workspace = input + "\\" + model + "\\" + str(period)
				diroutGrids = out + "\\" + model + "\\" + str(period)
				
			print "\nProcessing",country,model,period,"\n"

			rasterList = arcpy.ListRasters("*", "GRID")
			for raster in rasterList:

				if os.path.basename(raster).split("_")[0] == "bio" or os.path.basename(raster).split("_")[0] == "prec" or os.path.basename(raster).split("_")[0] == "tmean" or os.path.basename(raster).split("_")[0] == "dtr":
					
					diroutGridsVar = diroutGrids + "\\" + os.path.basename(raster).split("_")[0]+"_tif"
					if not os.path.exists(diroutGridsVar):
						os.system('mkdir ' + diroutGridsVar)
					tileTif= rcp+"_"+period+"_"+model+"_"+raster + "_" + str(int(str(countryDic [country]).split(" ")[0]) * int(str(countryDic [country]).split(" ")[1]) - 1)+".tif"
					if not arcpy.Exists(diroutGridsVar + "\\" + rcp+"_"+period+"_"+model+"_"+raster + "_" + str(int(str(countryDic [country]).split(" ")[0]) * int(str(countryDic [country]).split(" ")[1]) - 1)+".tif"):
						
						# trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
						# for trashfile in trashList:
							# os.remove(trashfile)
						
						print "\tspliting .. ",raster
						
						# rasterdeleteList = sorted(glob.glob(diroutGridsVar + "\\" + os.path.basename(raster) + "_*"))
						# for rasterdelete in rasterdeleteList:
							# arcpy.Delete_management(rasterdelete)
						
						# arcpy.SplitRaster_management(raster, diroutGridsVar, rcp+"_"+period+"_"+model+"_"+raster + "_", "NUMBER_OF_TILES", "TIFF", "#", "4 4", "#", "0", "DEGREES", "#", "#")
						arcpy.SplitRaster_management(raster,diroutGridsVar,raster + "_","NUMBER_OF_TILES","TIFF","NEAREST","4 4","2048 2048","0","PIXELS","#","#")
						# arcpy.SplitRaster_management(raster, diroutGridsVar, raster + "_", "NUMBER_OF_TILES", "GRID", "#", str(countryDic [country]), "#", "0", "PIXELS", "#", "#")
						print "\t" + raster,"tiled"					
						
						# trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
						# for trashfile in trashList:
							# os.remove(trashfile)
						
					else:
						print "\t" + raster,"tiled"
				
		#Compressing and delete input files
		# os.system("7za a -mmt8 " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + ".zip " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution))
		# os.system("rmdir /s /q " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution))
		
		#Create check file
		checkTXT = open(checkFile, "w")
		checkTXT.close()

	else:
		
		print "\n\t",country," Tiled!"

	# if not os.path.exists(dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "_grid2tiff_countries_done.txt"):
		
		##Scenarios
		# grid2asciitiff_tiles.mainfunction(dirout, dirout, country, sres, period, resolution, "YES")
		
		## Compressing country files
		# print " \nCompressing country folder"
		# os.system("7za a -mmt12 " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + ".zip " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution)
		# os.system("rmdir /s /q " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution)
		# print " \nCompression done!"
		
		# checkTXT = open(dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "_grid2tiff_countries_done.txt", "w")
		# checkTXT.close()

		# print "\n",country," Tiles converted!"

	# else:
		# print "\n",country," Tiles converted!"

print "\n Process done!"
