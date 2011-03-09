# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: August 24th 2010
# Pourpose: Describe properties of Grids Downscaled, Disaggregated or Interpolated datasets, y exporta un grafico
# ----------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Describe_GCMDownscaled.py O:\climate_change\IPCC_CMIP3\ F:\climate_change\IPCC_CMIP3\ 2_5min disaggregated YES"
	print "   Syntax	: <code.py>, <dirbase>, <scenario>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "   dirout	: Out folder of txt describe archive"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: disaggregated, interpolated or downscaled"	
	print "   graph   : YES to export a graphic of raster dataset"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
#scenario = sys.argv[2]
dirout = sys.argv[2]
resolution = sys.argv[3]
type = sys.argv[4]
graph = sys.argv[5]

#Clear screen
os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Describe " + type + " " + str(resolution) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of scenarios and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
scenariolist = "A1B", "A2", "B1"

#Creation out describe txt file

diroutDescribe = dirout + "\\_describes"
if not os.path.exists(diroutDescribe):
    os.system('mkdir ' + diroutDescribe)

txtDescribe = diroutDescribe + "\\SRES_" + scenario + "_" + type + "_" + str(resolution) + ".txt"
if os.path.isfile(txtDescribe):
    outFile = open(txtDescribe, "a")
else:
    outFile = open(txtDescribe, "w")
outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")

for scenario in scenariolist:

	modellist = sorted(os.listdir(dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
	
	for model in modellist:

		for period in periodlist:

			if not os.path.exists(diroutDescribe + "\\"	+ "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period):
				#Set workspace
				gp.workspace = dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
				print "\n---> Processing: " + dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\n"

				#Get a list of raster into the workspace
				rasters = sorted(gp.ListRasters("", "GRID"))
				for raster in rasters:
					print "  Describing " + raster
					MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
					MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
					MEA = gp.GetRasterProperties_management(raster, "MEAN")
					STD = gp.GetRasterProperties_management(raster, "STD")
					CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
					#Write txt describe file
					outFile = open(txtDescribe, "a")
					outFile.write(scenario + "\t" + model + "\t" + period + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

					if graph == "YES":
						#Creating Graph of rater dataset

						print "  Creating graph for " + raster
						diroutGraph = diroutDescribe + "\\"	+ "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
						if not os.path.exists(diroutGraph):
							os.system('mkdir ' + diroutGraph)

						graphOut =  diroutGraph + "\\" + raster + ".jpeg"
						os.system("gdal_translate -of JPEG -q -outsize 10% 10% " + gp.workspace + "\\" + raster + " " + graphOut )

			else:
				print "\n---> Procesed: " + dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period

outFile.close()

print "Process done!!!"    
