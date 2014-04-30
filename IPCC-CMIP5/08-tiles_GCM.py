# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: October 23th, 2013
# Purpouse: Split in tiles CMIP5 delta method downscaled data
# email: c.e.navarro@cgiar.org
# ---------------------------------------------------------------------------------

import arcpy, os, sys, string, glob, shutil
from arcpy import env

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 08-tiles_GCM.py T:\gcm\cmip5\downscaled D:\jetarapues\cmip5_process\cmip5_tiles_process T:\gcm\cmip5\ipcc_5ar_ciat_tiled rcp45 30s r1i1p1 ALL"
	sys.exit(1)

dirBase = sys.argv[1]
dirTmp = sys.argv[2]
dirOut = sys.argv[3]
rcp = sys.argv[4]
res = sys.argv[5]
ens = sys.argv[6]
varlist = sys.argv[7]

os.system('cls')

print "\t/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ "
print "\t/\/\/\/\/SPLIT IN TILES CMIP5/\/\/\/ "
print "\t/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ \n"

modellist = sorted(os.listdir(dirBase + "\\" + rcp + "\\global_" + str(res)))

if varlist == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")


rcpDc = {"rcp26": "rcp2_6", "rcp45": "rcp4_5", "rcp60": "rcp6_0", "rcp85": "rcp8_5"}
periodDc = {"2020_2049": "2030s", "2040_2069": "2050s", "2060_2089": "2070s", "2070_2099": "2080s"}
zoneDc = {"0":"c1", "1":"c2", "2":"c3", "3":"c4", "4":"c5", "5":"c6", "6":"b1", "7":"b2", "8":"b3", "9":"b4", "10":"b5", "11":"b6", "12":"a1", "13":"a2", "14":"a3", "15":"a4", "16":"a5", "17":"a6"}

print "Available models: " + str(modellist)

for model in sorted(modellist):

    for period in sorted(periodDc):

		dirCopy = dirOut + "\\" + rcpDc[rcp] + "\\" + periodDc[period] + "\\" +  model + "\\" + res		
		checkfile = dirCopy + "_tiles_done.txt"
		if not os.path.exists(checkfile):
			
			##### Slit in tiles
			# print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, "\n"
			
			#Set dirTmp
			dirProc = dirTmp + "\\" + rcp + "_" + model + "_" + period
			dirgrids = dirTmp + "\\" + rcp + "_" + model + "_" + period + "\grids"
			if not os.path.exists(dirProc):
				os.system('mkdir ' + dirProc)
			if not os.path.exists(dirgrids):
				os.system('mkdir ' + dirgrids)
				
			print "\n\tprocessing", model,period + "\n"
			
			########## ckeck if raster exist!
			# arcpy.env.workspace = dirgrids
			# for variable in variablelist:
				# if variable == "bio":
					# num = 19
				# else:
					# num = 12
				# for month in range (1, num + 1, 1):
					# for i in range(1,18):
						# if variable == "cons_mths":
							# raster = arcpy.env.workspace + "\\" + variable + "_" + str(i)
						# else:
							# raster = arcpy.env.workspace + "\\" + variable + "_" + str(month) + "_" + str(i)
						# if not arcpy.Exists(raster):
							# print "no existe", os.path.basename(raster), model, period
							# raster = dirBase + "\\" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + ens + "\\" + period + "\\" + os.path.basename(raster).split("_")[0]+"_"+os.path.basename(raster).split("_")[1]
							# arcpy.SplitRaster_management(raster, dirgrids, raster + "_", "NUMBER_OF_TILES",  "GRID", "#", "6 3", "#", "0", "DEGREES", "#", "#")
							# print "\t", os.path.basename(raster)+"_"+str(i), "fix splited" 							
				

			########### ckeck if raster exist!
			
			# arcpy.env.workspace = dirgrids	
			# text = dirgrids+"\\errores.txt"	
			# if os.path.exists(text):
				# os.system("del /s /q " + text)
				# print "\tdelete file exist:",text	
			# rasterList = sorted(arcpy.ListRasters("", "GRID"))
			# for raster in rasterList:
				# try:
					######### arcpy.CalculateStatistics_management(raster)
					# arcpy.GetRasterProperties_management(raster, "MINIMUM")
				# except:
					# print "DELETE FILE  ", raster

					# txt =  open(dirgrids+"\\errores.txt", "a")	
					# txt.write(raster+"\n")
					# txt.close()
					# shutil.rmtree(dirgrids+"\\"+raster)
			# arcpy.env.workspace = dirBase + "\\" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + ens + "\\" + period		
			# if os.path.exists(dirgrids+"\\errores.txt"):
				# txt =  open(dirgrids+"\\errores.txt", "r")
				# for grds in txt:
					# raster = grds.split("_")[0]+"_"+grds.split("_")[1]
					# arcpy.SplitRaster_management(raster, dirgrids, raster + "_", "NUMBER_OF_TILES",  "GRID", "#", "6 3", "#", "0", "DEGREES", "#", "#")
					# print "\t", os.path.basename(raster), "fix splited" 
				
				
			###### Get a list of raster in workspace
			arcpy.env.workspace = dirBase + "\\" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + ens + "\\" + period
			rasterList = sorted(arcpy.ListRasters("", "GRID"))
			for raster in rasterList:
				if not arcpy.Exists(dirgrids+"\\"+raster+"_17"):
					arcpy.SplitRaster_management(raster, dirgrids, raster + "_", "NUMBER_OF_TILES",  "GRID", "#", "6 3", "#", "0", "DEGREES", "#", "#")
					print "\t ", os.path.basename(raster), " splited"# + "_"+str(i)
				else:
					print "\t ", os.path.basename(raster), " splited"
			####### Convert to Asciis
			# print "\n .> Convert to Asciis: ", rcp, model, str(res), ens, period, "\n"
			
			#########Get a list of raster in processing dir
			arcpy.env.workspace = dirgrids		
			rasterList = sorted(arcpy.ListRasters("", "GRID"))
			for raster in rasterList:
				
				############# Convert to ESRI-Ascii
				var = os.path.basename(raster).split("_")[0]
				month = os.path.basename(raster).split("_")[1]
				zone = os.path.basename(raster).split("_")[-1]
					
				dirAsc = dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ascii\\" + var + "_" + zoneDc[zone]
				if not os.path.exists(dirAsc):
					os.system('mkdir ' + dirAsc)
				
				outAsc = dirAsc + "\\" + var + "_" + month + ".asc"
				prjAsc = dirAsc + "\\" + var + "_" + month + ".prj"
				# print outAsc
				if not os.path.exists(outAsc): 
					try:
						##########os.system("gdal_translate -of AAIGrid -ot Int16 -quiet " + arcpy.env.workspace + "\\" + raster + " " + outAsc)
						arcpy.RasterToASCII_conversion(raster, outAsc)
						
					except:
						arcpy.CalculateStatistics_management(raster)
						arcpy.RasterToASCII_conversion(raster, outAsc)
					print "\t", os.path.basename(raster), " converted to ascii"
					# arcpy.Delete_management(raster)
				else:
					print "\t", os.path.basename(raster), " converted"
					
				if os.path.exists(prjAsc):
					os.system("del /s /q " + prjAsc)					
				
			#################### Compress by Zones
			print "\n .> Compress by Zones: ", rcp, model, str(res), ens, period, "\n"

			######### if os.path.exists(dirProc + "\\info"):
				###### os.system("rmdir /s /q " + dirProc + "\\info")
			if not os.path.exists(dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ZipVarZonas"):
				os.system('mkdir ' + dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ZipVarZonas")

				
			varZones = sorted(os.listdir(dirProc+ "\\ascii"))
			for varZone in varZones:
				######### if varZone.split("_")[0] == "cons":
					###########inZip = dirProc + "\\" + model + "_" + rcpDc[rcp] + "_" + periodDc[period] + "_" + varZone.split("_")[0] + "_" + res + "_" + ens + "_" + varZone.split("_")[1] + "_asc.zip"
				######## else:
				inZip = dirProc + "\\ZipVarZonas\\" + model + "_" + rcpDc[rcp] + "_" + periodDc[period] + "_" + varZone.split("_")[0] + "_" + res + "_" + ens + "_" + varZone.split("_")[1] + "_asc.zip"
				if not os.path.exists(inZip):
					os.system('7za a -tzip ' + inZip + " " + dirProc + "\\ascii\\" + varZone)
					
					os.system("rmdir /s /q " + dirProc + "\\" + varZone)
					
					print varZone, " compressed!"
				
				########## else:
					######### Copying to output dir
					if not os.path.exists(dirCopy + "\\"+os.path.basename(inZip) ):
						print "\n .> Copying to output dir: ", rcp, model, str(res), ens, period, "\n"
						os.system("robocopy " + dirProc + "\\ZipVarZonas" + " " + dirCopy + " "+os.path.basename(inZip)+ " /z /e /MOV")
			
			# os.system("rmdir /s /q " + dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ascii")
			os.system("rmdir /s /q " + dirProc)
			checkTxt = open(checkfile, "w")
			checkTxt.close()
			
			print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, " done! \n"

		else:
			
			print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, " done! \n"

print " Split in Tiles CMIP5 Process done!!!" 