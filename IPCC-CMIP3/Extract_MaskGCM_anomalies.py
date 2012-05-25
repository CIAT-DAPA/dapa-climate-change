# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_MaskGCM.py L:\climate_change\IPCC_CMIP3 A2 D:\Masks\ColPlains\ColPlains.shp D:\Workspace\MS_llanos\_extractLlanos 30s anomalies"
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
periodlist = "2020_2049", "2040_2069" #"1961_1990", "2070_2099", "2050_2079", "2060_2089", "2030_2059",
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type))
print "Available models: " + str(modellist)

# for model in modellist:
for model in modellist:
    for period in periodlist:
    
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period

		if os.path.exists(gp.workspace) and not os.path.exists(dirout + "\\SRES_" + scenario + "\\" + model + "_extract_" + period + "_done.txt"):
			print "\n---> Processing: " + "SRES_" + scenario + " " + type + " " + model + " " + period + "\n"
			diroutraster = dirout + "\\SRES_" + scenario + "\\" + model + "\\" + period
			diroutascii = dirout + "\\SRES_" + scenario + "\\" + model + "\\" + period

			if not os.path.exists(diroutraster):
				os.system('mkdir ' + diroutraster)
				# describefile = dirout + "\\_SRES_" + str(scenario) + "_" +  str(resolution) + "_" + str(type)  + ".txt"
				# if os.path.isfile(describefile):
					# outFile = open(describefile, "a")
				# else:
					# outFile = open(describefile, "w")

				# outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")
				
				#Get a list of raster in workspace
			rasters = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters:
				print "    Extracting " + raster
				OutRaster = diroutraster + "\\" + raster
				# X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
				if not gp.Exists(OutRaster):
					# gp.clip_management(raster,"66 4 100 38 ",OutRaster)
					gp.ExtractByMask_sa(gp.workspace + "\\" + raster, mask, OutRaster)

					# if not os.path.exists(diroutascii):
						# os.system('mkdir ' + diroutascii)

			# gp.workspace = diroutraster
			# rasters1 = sorted(gp.ListRasters("*", "GRID"))
			# for raster in rasters1:
				# OutAscii = diroutascii + "\\" + os.path.basename(raster) + ".asc"
				# print "    Converting " + OutAscii
				# gp.RasterToASCII_conversion(raster, OutAscii)
				# InZip = diroutascii + "\\" + os.path.basename(raster).split("_")[0] + "_asc.zip"
				# os.system('7za a ' + InZip + " " + OutAscii)
				# os.remove(OutAscii)
				# gp.delete_management(raster)

			# pjrList = sorted(glob.glob(diroutascii + "\\*.pjr"))
			# for pjr in pjrList:
				# os.remove(pjr)
				
				
					# MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
					# MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
					# MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
					# STD = gp.GetRasterProperties_management(OutRaster, "STD")
					# CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
					# outFile = open(describefile, "a")
					# outFile.write(scenario + "\t" + model + "\t" + period + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

			print "Done!!"
			
			checkTXT = open(dirout + "\\SRES_" + scenario + "\\" + model + "_extract_" + period + "_done.txt", "w")
			checkTXT.close()
		else:
			print "The model " + model + " " + period + " is already processed"
			print "Processing the next period \n"
print "done!!!" 