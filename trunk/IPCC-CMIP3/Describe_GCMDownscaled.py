# ------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: August 24th 2010
# Pourpose: Describe properties of Grids Downscaled, Disaggregated or Interpolated datasets
# ------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Describe_GCMDownscaled.py N:\climate_change\IPCC_CMIP3\ B1 D:\IPCC_CMIP3_process 2_5min Disaggregated"
	print "   Syntax	: <code.py>, <dirbase>, <scenario>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "   dirout	: Out folder of txt describe archive"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]
resolution = sys.argv[4]
type = sys.argv[5]

#Clear screen
os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Describe " + type + " " + str(resolution) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of periods and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))

#Creation out describe txt file
if os.path.isfile(dirout + "\\" + type + "_SRES_" +  scenario + "_" + str(resolution) + ".txt"):
    outFile = open(dirout + "\\" + type + "_SRES_" +  scenario + "_" + str(resolution) + ".txt", "a")
else:
    outFile = open(dirout + "\\" + type + "_SRES_" +  scenario + "_" + str(resolution) + ".txt", "w")
outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")


for model in modellist:

    for period in periodlist:
	
		#Set workspace
        gp.workspace = dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
        print "\n---> Processing: " + dirbase + "SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period

		#Get a list of raster into the workspace
        rasters = sorted(gp.ListRasters("", "GRID"))
        for raster in rasters:
            print raster
            MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
            MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
            MEA = gp.GetRasterProperties_management(raster, "MEAN")
            STD = gp.GetRasterProperties_management(raster, "STD")
            CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
			
			#Write txt describe file
            outFile = open(dirout + "\\" + type + "_SRES_" +  scenario + "_" + str(resolution) + ".txt", "a")
            outFile.write(scenario + "\t" + model + "\t" + period + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

outFile.close()

print "done!!!"    
