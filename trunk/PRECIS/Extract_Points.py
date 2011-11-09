# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_Points.py D:\climate_change\RCM_Data A1B D:\Workspace\Julian\Points_GCM_PRECIS\points.shp D:\Workspace\Julian\Points_GCM_PRECIS"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <scenario>, <mask>, <dirout>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
scenario = sys.argv[2]
mask = sys.argv[3]
dirout = sys.argv[4]

# resolution = sys.argv[5]
# type = sys.argv[6]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
periodlist = "2010_2039" , "2040_2069", # "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = "ECHAM5", "HadCM3Q0"
# modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\anomalies\\")
variablelist = "prec", "tmax1_5", "tmin1_5"
print "Available models: " + str(modellist)

for model in modellist:
	for period in periodlist:
	
		print "\n---> Processing: " + "SRES_" + scenario + " " + model + " " + period + "\n"
		diroutpoints = dirout + "\\_extract_" + "SRES_" + scenario 
		if not os.path.exists(diroutpoints):
			os.system('mkdir ' + diroutpoints)	
		
		if not os.path.exists(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt"):
			for variable in variablelist:
				for month in range (1, 12 + 1, 1):
					
					gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + model + "\\anomalies\\" + period
					raster = gp.workspace + "\\" + variable + "_" + str(month) #sorted(gp.ListRasters(variable + "*", "GRID"))
					
					# for raster in rasters:
					#OutRaster = diroutraster + "\\" + raster
					#gp.clip_management(raster,"-100 -60 -30 23 ",OutRaster)
					InPointsFC = mask 
					OutPointsFC = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"

					if not os.path.exists(OutPointsFC):
						print "\tExtracting .. " + os.path.basename(raster)
						#Check out Spatial Analyst extension license
						gp.CheckOutExtension("Spatial")

						#Process: Cell Statistics...
						gp.Sample_sa(raster, InPointsFC, OutPointsFC, "")
					else:
						print "\t" + os.path.basename(raster) + " extracted"
			
			print "\n"
			for variable in variablelist:
				for month in range (1, 12 + 1, 1):
					dbf = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + variable + "_" + str(month) + ".dbf"

					if not os.path.basename(dbf)[-10:] == "prec_1.dbf":
						print "\tJoining .. " + os.path.basename(dbf)
						InData = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_prec_1.dbf"
						fields = os.path.basename(dbf)[:-4].split("_")[-3:]
						if fields[1] == "prec":
							gp.joinfield (InData, "mask", dbf, "mask", fields[1] + "_" + fields[2])
						else:
							gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2])
						os.remove(dbf)
			
			xmlList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.xml"))
			for xml in xmlList:
				os.remove(xml)
			
			checkTXT = open(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt", "w")
			checkTXT.close()
print "done!!!" 