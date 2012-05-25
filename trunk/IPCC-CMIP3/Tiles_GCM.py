# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: March 30th, 2011
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Tiles_GCM.py M:\climate_change\IPCC_CMIP3 D:\climate_change\IPCC_CMIP3 N:\climate_change\IPCC_CMIP3 A1B 30s downscaled"
	print "   Syntax	: <Tiles_GCM.py>, <dirbase>, <dirtemp>, <scenario>, <dirout>, <scenario>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirtemp 	: Where is made calculations"
	print "   dirout	: Out folder"
	print "   scenario  : A1B, A2 or B1"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

dirbase = sys.argv[1]
dirtemp = sys.argv[2]
dirout = sys.argv[3]
scenario = sys.argv[4]
resolution = sys.argv[5]
type = sys.argv[6]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    SPLIT IN TILES     "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

#periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
latDc = {"A": 30, "B": -30, "C": -90}
lonDc = {"1": -180, "2": -120, "3": -60, "4": 0, "5": 60, "6": 120}

print "Available models: " + str(modellist)

for model in sorted(modellist)[10:]:

    # diroutmodel = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model
    # if not os.path.exists(diroutmodel):
        # os.system('mkdir ' + diroutmodel)

    for period in sorted(periodDc):

        gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period

        if not os.path.exists(dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "_TilesProcess_Done.txt"):
            
            print "\n---> Processing: " + "SRES_" + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"
            
            #Set dirtemp
            diroutraster = dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
            dirouttiles = diroutraster + "\\_tiles"
            if not os.path.exists(dirouttiles):
                os.system('mkdir ' + dirouttiles)
                            
            if not os.path.exists(diroutraster):
                os.system('mkdir ' + diroutraster)
                
            #Get a list of raster in workspace
            rasters = sorted(gp.ListRasters("", "GRID"))
            for raster in rasters:

                for lat in sorted(latDc):
                
                    for lon in sorted(lonDc):
						
						if not os.path.exists(dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\_tiles\\_VAR_" + os.path.basename(raster).split("_")[0] + ".txt"):
							
							print "\n    Processing " + raster 
							print "    ----> Extracting "
							OutRaster = diroutraster + "\\" + raster
							xmin = str(lonDc [lon])
							ymin = str(latDc [lat])
							xmax = int(xmin) + 60
							ymax = int(ymin) + 60
							
							gp.clip_management(raster," " + str(xmin) + " " + str(ymin) + " " + str(xmax) + " " + str(ymax) + " ",OutRaster)

							print "    ----> Converting " 
							OutAscii = dirouttiles + "\\" + os.path.basename(OutRaster) + ".asc"							
							gp.RasterToASCII_conversion(OutRaster, OutAscii)
							gp.delete_management(OutRaster)
							
							print "    ----> Compressing "
							InZip = dirouttiles + "\\" + model + "_" + scenario + "_" + str(periodDc [period]) + "_" + os.path.basename(OutRaster).split("_")[0] + "_Zone" + str(lat) + str(lon)  + "_asc.zip"
							os.system('7za a ' + InZip + " " + OutAscii)
							os.remove(OutAscii)
							
							if str(lat) + str(lon) == "C6": 
								if raster == "bio_9" or raster == "cons_mths" or raster == "prec_9" or raster == "tmin_9" or raster == "tmax_9" or raster == "tmean_9" :
									checkVAR = open(dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\_tiles\\_VAR_" + os.path.basename(raster).split("_")[0] + ".txt", "w")
									checkVAR.close()
                        
            print "Done!!"
            
            checkTXT = open(dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "_TilesProcess_Done.txt", "w")
            checkTXT.close()
                
        else:
            print "\nThe model " + model + " " + period + " is already processed"
            print "Processing the next period \n"

    # try:
        # print "\n    ----> Copying out tiles by models... \n"
        # # diroutcopy = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period 
        # # if not os.path.exists(diroutcopy):
            # # os.system('mkdir ' + diroutcopy)
        # shutil.copytree(dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model, dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model)
        # print "\n    ----> Copy done... \n"

    # except: 
        # print "Error copying output ascii folder of " + str(model)
        # sys.exit(4)

    # print "\n    ----> Removing temporal folders ... \n"
    # shutil.rmtree(dirtemp + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model)

print "Process done!!!" 