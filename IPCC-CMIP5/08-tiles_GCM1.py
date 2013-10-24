# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: October 23th, 2013

import arcpy, os, sys, string, glob
from arcpy import env

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 08-tiles_GCM.py T:\gcm\cmip5\downscaled G:\cenavarro\cmip5_tiles_process S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled rcp26 30s r1i1p1"
	sys.exit(1)

dirBase = sys.argv[1]
dirTmp = sys.argv[2]
dirOut = sys.argv[3]
rcp = sys.argv[4]
res = sys.argv[5]
ens = sys.argv[6]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    SPLIT IN TILES CMIP5    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

modellist = sorted(os.listdir(dirBase + "\\" + rcp + "\\global_" + str(res)))

rcpDc = {"rcp26": "2_6", "rcp45": "4_5", "rcp60": "6_0", "rcp85": "8_5"}
periodDc = {"2020_2049": "2030s", "2040_2069": "2050s", "2060_2089": "2070s", "2070_2099": "2080s"}
zoneDc = {"0":"C1", "1":"C2", "2":"C3", "3":"C4", "4":"C5", "5":"C6", "6":"B1", "7":"B2", "8":"B3", "9":"B4", "10":"B5", "11":"B6", "12":"C1", "13":"C2", "14":"C3", "15":"C4", "16":"C5", "17":"C6"}

print "Available models: " + str(modellist)

for model in sorted(modellist):

    for period in sorted(periodDc):
		
		checkfile = dirOut + "\\" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + period + "_TilesProcess_Done.txt"
		if not os.path.exists(checkfile):
            
			arcpy.env.workspace = dirBase + "\\sres_" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + ens + "\\" + period
			
			print "\n.> Slit in tiles: ", rcp, model, str(res), ens, period,  + "\n"

			#Set dirTmp
			dirProc = dirTmp + "\\" + rcp + "_" + model + "_" + period
			if not os.path.exists(dirProc):
				os.system('mkdir ' + dirProc)
								
			#Get a list of raster in workspace
			rasterList = sorted(arcpy.ListRasters("", "GRID"))
			for raster in rasterList:
				
				# Split in tiles function
				arcpy.SplitRaster_management(raster, dirProc, raster + "_", "NUMBER_OF_TILES",  "GRID", "#", "6 3", "#", "0", "DEGREES ", "#", "#")
				print os.path.basename(raster), " splited"
			
			print "\n.> Convert to Asciis: ", rcp, model, str(res), ens, period,  + "\n"
			
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
				arcpy.RasterToASCII_conversion(raster, outAsc)
				
				%outfolder%\%modname%_%rcpmod%_%tsmod%_%vartype%_%resol%_r1i1p1_no_tile_asc.zip %outfolder%\%gridname%.asc
				
				
				
				inZip = dirProc + "\\" + model + "_" + rcp + "_" + str(periodDc [period]) + "_" + os.path.basename(OutRaster).split("_")[0] + "_Zone" + str(lat) + str(lon)  + "_asc.zip"
				
				
				if str(lat) + str(lon) == "C6": 
					if raster == "bio_9" or raster == "cons_mths" or raster == "prec_9" or raster == "tmin_9" or raster == "tmax_9" or raster == "tmean_9" :
						checkVAR = open(checkvar, "w")
						checkVAR.close()
			
			print "\n\t Copying results.."
			
			trashlist = sorted(glob.glob(dirOuttiles + "\\*.prj"))
			for trash in trashlist:
				os.remove(trash)
			
			os.system("robocopy " + dirOuttiles + " " + dirOut + "\\sres_" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + period + "\\_tiles /z /e")
			os.system("rmdir /s /q " + dirOutraster)

			checkTXT = open(checkfile, "w")
			checkTXT.close()
			print "\n---> Cut in tiles: ", rcp, model, period + " done! \n"

		else:
			
			print "\n---> Cut in tiles: ", rcp, model, period + " done! \n"

print "Process done!!!" 