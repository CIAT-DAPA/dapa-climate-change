# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Create tile from GCM data
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string, glob
from arcpy import env

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python fix_tiles_countries.py D:\cenavarro\Analogues_GCM_data\ExtractByCountry a2 D:\cenavarro\Analogues_GCM_data\TilesByCountry 2020_2049 D:\cenavarro\Analogues_GCM_data\TilesByCountry\a2_check.txt"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
sres = sys.argv[2]
dirout = sys.argv[3]
# resolution = sys.argv[4]
period = sys.argv[4]
txtfile = sys.argv[5]

os.system('cls')

txtfile = open(txtfile)

for line in txtfile:
	
	country = line.split("\t")[0]
	model = line.split("\t")[1]
	resolution = line.split("\n")[0].split("\t")[2]
		
	if not os.path.exists(dirout + "\\SRES_" + sres + "\\fix_" + country + "_" + model + ".txt"):
	
		if resolution == "30s":
			countryDic = {"cod": "6 6 ", "fji": "1 1 ", "kna": "1 1 ", "mne": "1 1 ", "nru": "1 1 ", "nzl": "5 5 ", "plw": "1 1 ", "pse": "1 1 ", "srb": "1 1 ", "ssd": "3 3 ", "tls": "1 1 ", "tto": "1 1 ", "tuv": "1 1 ", "pan": "2 2 "}
		elif resolution == "2_5min":
			countryDic = {"arg": "2 2 ", "aus": "3 3 ", "bra": "3 3 ", "can": "4 4 ", "chl": "3 3 ", "chn": "3 3 ", "grl": "3 3 ", "idn": "2 2 ", "ind": "2 2 ", "jpn": "1 1 ", "kaz": "2 2 ", "rus": "7 7 ", "usa": "8 8 ", "af": "4 4 ", "as": "6 6 ", "eu": "3 3 ", "na": "5 5 ", "oc": "3 3 ", "sa": "4 4 "}
		elif resolution == "5min":
			countryDic = {"af": "2 2 ", "as": "3 3 ", "eu": "2 2 ", "na": "3 3 ", "oc": "2 2 ", "sa": "2 2 "}
		elif resolution == "10min":
			countryDic = {"af": "1 1 ", "as": "2 2 ", "eu": "1 1 ", "na": "1 1 ", "oc": "1 1 ", "sa": "1 1 "}
		
		print "~~~~~~~~~~~~~~~~~~~"
		print "  TILES COUNTRIES  "
		print "~~~~~~~~~~~~~~~~~~~"
		
		inZip = dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + ".zip"
		if not os.path.exists(dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + "\\" + model):
			os.system("7za x -aos -o" + dirbase + "\\SRES_" + sres + "\\downscaled" + " " + inZip  + " " + country + "_" + str(resolution) + "\\" + model)
				
		arcpy.env.workspace = dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + "\\" + model + "\\" + period
		diroutGrids = dirout + "\\sres_" + sres + "\\" + country + "_" + str(resolution) + "\\" + model
		print arcpy.env.workspace
		if os.path.exists(diroutGrids):
			os.system("rmdir /s /q " + diroutGrids + "\\" + period)
			
		print "\nProcessing",country,model,period,resolution,"\n"
		rasterList = arcpy.ListRasters("*", "GRID")
		
		for raster in rasterList:
			
			if os.path.basename(raster).split("_")[0] == "bio" or os.path.basename(raster).split("_")[0] == "prec" or os.path.basename(raster).split("_")[0] == "tmean" or os.path.basename(raster).split("_")[0] == "dtr":
				
				diroutGridsVar = diroutGrids + "\\" + os.path.basename(raster).split("_")[0]
				if not os.path.exists(diroutGridsVar):
					os.system('mkdir ' + diroutGridsVar)

				if not arcpy.Exists(diroutGridsVar + "\\" + raster + "_" + str(int(str(countryDic [country]).split(" ")[0]) * int(str(countryDic [country]).split(" ")[1]) - 1)):
					
					trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
					for trashfile in trashList:
						os.remove(trashfile)
					
					print "\tspliting .. ",raster
					
					rasterdeleteList = sorted(glob.glob(diroutGridsVar + "\\" + os.path.basename(raster) + "_*"))
					for rasterdelete in rasterdeleteList:
						arcpy.Delete_management(rasterdelete)
					
					arcpy.SplitRaster_management(raster, diroutGridsVar, raster + "_", "NUMBER_OF_TILES", "GRID", "#", str(countryDic [country]), "#", "0", "PIXELS", "#", "#")
					print "\t" + raster,"tiled"					
					
					trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
					for trashfile in trashList:
						os.remove(trashfile)
					
				else:
					print "\t" + raster,"tiled"
				
		

		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		print "  CONVERT TO ASCII AND TIFF	 "
		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

		varlist = "bio", "dtr", "prec", "tmean"
		for var in varlist:
			
			arcpy.env.workspace = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + var
			
			print "\n --> Processing: " + country,sres,model,period,var,resolution,"\n"
				
			diroutTiff = dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"
			if not os.path.exists(diroutTiff):
				os.system('mkdir ' + diroutTiff)
			
			rasters = arcpy.ListRasters("*", "GRID")
			
			for raster in rasters:
				
				OutTiff = diroutTiff + "\\" + sres + "_" + period + "_" + model + "_" + raster + ".tif"
				
				if not os.path.exists(OutTiff):
					print "\tConverting " + raster
					os.system("gdal_translate -of GTiff -ot Int32 -co COMPRESS=lzw -quiet " + arcpy.env.workspace + "\\" + raster + " " + OutTiff)

				else:
					print "\tConverted " + raster
			
			os.system("rmdir /s /q " + arcpy.env.workspace)

		print "\n",country," Tiles converted!"
		# os.remove(dirbase + "\\SRES_" + sres + "\\" + country + "_" + str(resolution) + ".zip")
		# os.system("7za a " + dirbase + "\\SRES_" + sres + "\\" + country + "_" + str(resolution) + ".zip " + dirbase + "\\SRES_" + sres + "\\" + country + "_" + resolution)
		#os.system("rmdir /s /q " + dirbase + "\\SRES_" + sres + "\\" + country + "_" + str(resolution))
		
		checkTXT = open(dirout + "\\SRES_" + sres + "\\fix_" + country + "_" + model + ".txt", "w")
		checkTXT.close()
		
		
print "\n Process done!"
