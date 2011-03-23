# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_MaskGCM.py O:\climate_change\IPCC_CMIP3 A1B F:\climate_change\_Masks\SouthAmerica.shp F:\climate_change\IPCC_CMIP3\SouthAmerica_Extract 2_5min downscaled"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <scenario>, <mask>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
scenario = sys.argv[2]
mask = sys.argv[3]
dirout = sys.argv[4]
resolution = sys.argv[5]
type = sys.argv[6]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
print "Available models: " + str(modellist)

for model in modellist:
    for period in periodlist:
        gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
        if os.path.exists(gp.workspace) and not os.path.exists(dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\_extract_" + period + "_done"):
            print "\n---> Processing: " + "SRES_" + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"
            diroutraster = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
            diroutascii = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\_asciis"

            if not os.path.exists(diroutraster):
                os.system('mkdir ' + diroutraster)
                
                #Get a list of raster in workspace
                rasters = sorted(gp.ListRasters("", "GRID"))
                for raster in rasters:
                    print "    Extracting " + raster
                    OutRaster = diroutraster + "\\" + raster
                    gp.clip_management(raster,"-100 -60 -30 23 ",OutRaster)

                    if not os.path.exists(diroutascii):
                        os.system('mkdir ' + diroutascii)

                    OutAscii = diroutascii + "\\" + os.path.basename(OutRaster) + ".asc"
                    print "    Converting " + OutAscii
                    gp.RasterToASCII_conversion(OutRaster, OutAscii)
                    InZip = diroutascii + "\\" + os.path.basename(OutRaster).split("_")[0] + "_asc.zip"
                    os.system('7za a ' + InZip + " " + OutAscii)
                    os.remove(OutAscii)
                    gp.delete_management(OutRaster)

                print "Done!!"
                
                checkTXT = open(dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\_extract_" + period + "_done", "w")
                checkTXT.close()
        else:
            print "The model " + model + " " + period + " is already processed"
            print "Processing the next period \n"
print "done!!!" 