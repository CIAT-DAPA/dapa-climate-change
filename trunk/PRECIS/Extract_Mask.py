# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: June 13th, 2011
# Purpose: Extraction by mask PRECIS surfaces
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_Mask.py L:\climate_change\RCM_Data\SRES_A1B D:\Masks\ColPlains\ColPlains.shp D:\climate_change\RCM_Data\_extract_ColPlains\SRES_A1B"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <mask>, <dirout>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]

gp.CheckOutExtension("Spatial")
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK PRECIS "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
periodlist = "1961_1990", "2020_2049", "2040_2069" #"2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase))
variablelist = "prec", "tmean1_5", "tmmax1_5", "tmmin1_5"
print "Available models: " + str(modellist)

for model in modellist:
	for period in periodlist:
		gp.workspace = dirbase + "\\" + model + "\\30yrAverages\\" + period
		if os.path.exists(gp.workspace):
			print "\n---> Processing: " + model + " " + period + "\n"
			for variable in variablelist:
			
				diroutdescribe = dirout + "\\_describes"  
				if not os.path.exists(diroutdescribe):
					os.system('mkdir ' + diroutdescribe)
				if os.path.isfile(diroutdescribe + "\\" + variable + "_" + model + ".txt"):
					outFile = open(diroutdescribe + "\\" + variable + "_" + model + ".txt", "a")
				else:
					outFile = open(diroutdescribe + "\\" + variable + "_" + model + ".txt", "w")
				outFile.write("PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")

				rasters = gp.ListRasters(str(variable) + "*", "GRID")
				for raster in rasters:
					print raster
					diroutraster = dirout + "\\" + model + "\\30yrAverages\\" + period
					if not os.path.exists(diroutraster):
						os.system('mkdir ' + diroutraster)
					OutRaster = diroutraster + "\\" + raster
					gp.ExtractByMask_sa(raster, mask, OutRaster)
					
					MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
					MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
					MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
					STD = gp.GetRasterProperties_management(OutRaster, "STD")
					CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
					outFile = open(diroutdescribe + "\\" + variable + "_" + model + ".txt", "a")
					outFile.write(str(period) + "\t" + OutRaster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

print "done!!!" 