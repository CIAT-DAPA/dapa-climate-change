# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: March 30th, 2011
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Tiles_GCM.py S:\data\gcm\cmip3\downscaled G:\jtarapues\tiles_downscaled_process G:\jtarapues\downscaled_tiled b1 30s"
	sys.exit(1)

dirbase = sys.argv[1]
dirtemp = sys.argv[2]
dirout = sys.argv[3]
scenario = sys.argv[4]
resolution = sys.argv[5]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    SPLIT IN TILES     "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

modellist = sorted(os.listdir(dirbase + "\\sres_" + scenario + "\\Global_" + str(resolution)))
periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
latDc = {"A": 30, "B": -30, "C": -90}
lonDc = {"1": -180, "2": -120, "3": -60, "4": 0, "5": 60, "6": 120}

print "Available models: " + str(modellist)

for model in sorted(modellist):

    for period in sorted(periodDc):
		
		checkfile = dirout + "\\sres_" + scenario + "\\global_" + str(resolution) + "\\" + model + "\\" + period + "_TilesProcess_Done.txt"
		
		if not os.path.exists(checkfile):
            
			gp.workspace = dirbase + "\\sres_" + scenario + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
			
			print "\n---> Cut in tiles: ",scenario, model, period + "\n"

			#Set dirtemp
			diroutraster = dirtemp + "\\sres_" + scenario + "\\" + model + "_" + period
			dirouttiles = diroutraster + "\\_tiles"

			if not os.path.exists(dirouttiles):
				os.system('mkdir ' + dirouttiles)
								
			#Get a list of raster in workspace
			rasters = sorted(gp.ListRasters("", "GRID"))
			for raster in rasters:
				for lat in sorted(latDc):
					for lon in sorted(lonDc):
						
						checkvar = dirouttiles + "\\_VAR_" + os.path.basename(raster).split("_")[0] + ".txt"
						if not os.path.exists(checkvar):
							
							print "\n\t Processing " + raster 
							print "\t\t Cutting.. "
							OutRaster = diroutraster + "\\" + raster
							xmin = str(lonDc [lon])
							ymin = str(latDc [lat])
							xmax = int(xmin) + 60
							ymax = int(ymin) + 60
							
							gp.clip_management(raster," " + str(xmin) + " " + str(ymin) + " " + str(xmax) + " " + str(ymax) + " ",OutRaster)

							print "\t\t Converting formats.. " 
							OutAscii = dirouttiles + "\\" + os.path.basename(OutRaster) + ".asc"
							gp.RasterToASCII_conversion(OutRaster, OutAscii)
							gp.delete_management(OutRaster)
							
							print "\t\t Compressing.. " 
							InZip = dirouttiles + "\\" + model + "_" + scenario + "_" + str(periodDc [period]) + "_" + os.path.basename(OutRaster).split("_")[0] + "_Zone" + str(lat) + str(lon)  + "_asc.zip"
							os.system('7za a ' + InZip + " " + OutAscii)
							os.remove(OutAscii)
							
							if str(lat) + str(lon) == "C6": 
								if raster == "bio_9" or raster == "cons_mths" or raster == "prec_9" or raster == "tmin_9" or raster == "tmax_9" or raster == "tmean_9" :
									checkVAR = open(checkvar, "w")
									checkVAR.close()
			
			print "\n\t Copying results.."
			
			trashlist = sorted(glob.glob(dirouttiles + "\\*.prj"))
			for trash in trashlist:
				os.remove(trash)
			
			os.system("robocopy " + dirouttiles + " " + dirout + "\\sres_" + scenario + "\\global_" + str(resolution) + "\\" + model + "\\" + period + "\\_tiles /z /e")
			os.system("rmdir /s /q " + diroutraster)

			checkTXT = open(checkfile, "w")
			checkTXT.close()
			print "\n---> Cut in tiles: ", scenario, model, period + " done! \n"

		else:
			
			print "\n---> Cut in tiles: ", scenario, model, period + " done! \n"

print "Process done!!!" 