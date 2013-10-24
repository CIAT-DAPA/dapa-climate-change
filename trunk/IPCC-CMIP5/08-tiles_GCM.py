# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: October 23th, 2013
# Purpouse: Split in tiles CMIP5 delta method downscaled data
# email: c.e.navarro@cgiar.org
# ---------------------------------------------------------------------------------

import arcpy, os, sys, string, glob
from arcpy import env

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 08-tiles_GCM.py T:\gcm\cmip5\downscaled D:\CIAT\Workspace\cmip5_tiles_process S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled rcp26 10min r1i1p1"
	sys.exit(1)

dirBase = sys.argv[1]
dirTmp = sys.argv[2]
dirOut = sys.argv[3]
rcp = sys.argv[4]
res = sys.argv[5]
ens = sys.argv[6]

os.system('cls')

print "\t/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ "
print "\t/\/\/\/\/SPLIT IN TILES CMIP5/\/\/\/ "
print "\t/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ \n"

modellist = sorted(os.listdir(dirBase + "\\" + rcp + "\\global_" + str(res)))

rcpDc = {"rcp26": "rcp2_6", "rcp45": "rcp4_5", "rcp60": "rcp6_0", "rcp85": "rcp8_5"}
periodDc = {"2020_2049": "2030s", "2040_2069": "2050s", "2060_2089": "2070s", "2070_2099": "2080s"}
zoneDc = {"0":"C1", "1":"C2", "2":"C3", "3":"C4", "4":"C5", "5":"C6", "6":"B1", "7":"B2", "8":"B3", "9":"B4", "10":"B5", "11":"B6", "12":"A1", "13":"A2", "14":"A3", "15":"A4", "16":"A5", "17":"A6"}

print "Available models: " + str(modellist)

for model in sorted(modellist):

    for period in sorted(periodDc):

		dirCopy = dirOut + "\\" + rcpDc[rcp] + "\\" + periodDc[period] + "\\" +  model + "\\" + res		
		checkfile = dirCopy + "_tiles_done.txt"
		if not os.path.exists(checkfile):
			
			##### Slit in tiles
			print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, "\n"
			
			#Set dirTmp
			dirProc = dirTmp + "\\" + rcp + "_" + model + "_" + period
			if not os.path.exists(dirProc):
				os.system('mkdir ' + dirProc)
			
			#Get a list of raster in workspace
			arcpy.env.workspace = dirBase + "\\" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + ens + "\\" + period
			rasterList = sorted(arcpy.ListRasters("", "GRID"))
			for raster in rasterList:
				
				# if not os.path.basename(raster).split("_")[0] == "cons" :				
				# Split in tiles function
				arcpy.SplitRaster_management(raster, dirProc, raster + "_", "NUMBER_OF_TILES",  "GRID", "#", "6 3", "#", "0", "DEGREES", "#", "#")
				print "\t", os.path.basename(raster), " splited"
				
			
			##### Convert to Asciis
			print "\n .> Convert to Asciis: ", rcp, model, str(res), ens, period, "\n"
			
			#Get a list of raster in processing dir
			arcpy.env.workspace = dirProc			
			rasterList = sorted(arcpy.ListRasters("", "GRID"))
			for raster in rasterList:
				
				# Convert to ESRI-Ascii
				var = os.path.basename(raster).split("_")[0]
				month = os.path.basename(raster).split("_")[1]
				zone = os.path.basename(raster).split("_")[-1]
					
				dirAsc = dirProc + "\\" + var + "_" + zoneDc[zone]
				if not os.path.exists(dirAsc):
					os.system('mkdir ' + dirAsc)
				
				outAsc = dirAsc + "\\" + var + "_" + month + ".asc"
				try:
					# os.system("gdal_translate -of AAIGrid -ot Int16 -quiet " + arcpy.env.workspace + "\\" + raster + " " + outAsc)
					arcpy.RasterToASCII_conversion(raster, outAsc)
				except:
					arcpy.CalculateStatistics_management(raster)
					arcpy.RasterToASCII_conversion(raster, outAsc)
					
				arcpy.Delete_management(raster)
				
				print "\t", os.path.basename(raster), " converted"
				
			
			##### Compress by Zones
			print "\n .> Compress by Zones: ", rcp, model, str(res), ens, period, "\n"

			if os.path.exists(dirProc + "\\info"):
				os.system("rmdir /s /q " + dirProc + "\\info")
			
			varZones = sorted(os.listdir(dirProc))
			for varZone in varZones:
				
				inZip = dirProc + "\\" + model + "_" + rcpDc[rcp] + "_" + periodDc[period] + "_" + varZone.split("_")[0] + "_" + res + "_" + ens + "_" + varZone.split("_")[1] + "_asc.zip"
				os.system('7za a -tzip ' + inZip + " " + dirProc + "\\" + varZone)
				
				os.system("rmdir /s /q " + dirProc + "\\" + varZone)
				
				print varZone, " compressed!"
				
				
			##### Copying to output dir
			print "\n .> Copying to output dir: ", rcp, model, str(res), ens, period, "\n"
			
			os.system("robocopy " + dirProc + " " + dirCopy + " /z /e")
			os.system("rmdir /s /q " + dirProc)

			checkTxt = open(checkfile, "w")
			checkTxt.close()
			
			print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, " done! \n"

		else:
			
			print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, " done! \n"

print " Split in Tiles CMIP5 Process done!!!" 