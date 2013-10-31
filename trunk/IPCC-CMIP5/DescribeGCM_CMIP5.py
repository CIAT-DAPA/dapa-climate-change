# ---------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: August 24th, 2010
# Purpose: Describe properties of Grids Downscaled, Disaggregated, anomalies or Interpolated datasets
# ---------------------------------------------------------------------------------------------------

# python DescribeGCM_CMIP5.py T:\data\gcm\cmip5\interpolations rcp26 18

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: DescribeGCM_CMIP3.py <dirbase> <describe file> <sres> <resolution> <period>"
	print "   - ex: python DescribeGCM_CMIP3.py T:\data\gcm\cmip5\interpolations rcp26"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   descfile	: Output txt file"
	print "   rcp		: IPCC Emission Escenario"
	print "   res		: The possibilities are 2_5 min 5min 10min 30s"
	print "   period	: Future 30yr interval"	
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
rcp = sys.argv[2]
mod = sys.argv[3]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")


#Get lists of models and periods
periodlist = "2020_2049", "2040_2069", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase + "\\" + rcp))
print "\nAvailable models: " + str(modellist)
descfile = dirbase +"\\"+ rcp+"_describe.txt"
if not os.path.isfile(descfile):
	outFile = open(descfile, "w")
	outFile.write("RCP" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" 
				+ "\t" + "TOP" + "\t" + "LEFT" + "\t" + "RIGHT" + "\t" + "BOTTOM" + "\t" + "CELLSIZEX" + "\t" + "CELLSIZEY" + "\t" 
				+ "VALUETYPE" + "\t" + "COLUMNCOUNT" + "\t" + "ROWCOUNT" + "\n") #+ "\t" + "BANDCOUNTUSER" + "\n")
	outFile.close()

# Looping around periods
for period in periodlist:

	# Looping around models 
	for model in modellist[int(mod):]:

		print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		print "     Describe " + " " + rcp + " "  + str(model) + " "  + str(period) 
		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
		
		#Set workspace
		gp.workspace = dirbase + "\\" + rcp + "\\" + model + "\\r1i1p1\\" + period

		#Get a list of raster into the workspace
		rasters = sorted(gp.ListRasters("", "GRID"))
		
		# Looping around rasters 
		for raster in rasters:

			# Parameters
			MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
			MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
			MEA = gp.GetRasterProperties_management(raster, "MEAN")
			STD = gp.GetRasterProperties_management(raster, "STD")
			TOP = gp.GetRasterProperties_management(raster, "TOP")
			LEF = gp.GetRasterProperties_management(raster, "LEFT")
			RIG = gp.GetRasterProperties_management(raster, "RIGHT")
			BOT = gp.GetRasterProperties_management(raster, "BOTTOM")
			CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
			CEY = gp.GetRasterProperties_management(raster, "CELLSIZEY")
			VAL = gp.GetRasterProperties_management(raster, "VALUETYPE")
			COL = gp.GetRasterProperties_management(raster, "COLUMNCOUNT")
			ROW = gp.GetRasterProperties_management(raster, "ROWCOUNT")
			# BAN = gp.GetRasterProperties_management(raster, "BANDCOUNTUSER")
			
			# Writting grid characteristics
			outFile = open(descfile, "a")
			outFile.write(rcp + "\t" + model + "\t" + period + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" 
						+ TOP.getoutput(0) + "\t" + LEF.getoutput(0) + "\t" + RIG.getoutput(0) + "\t" + BOT.getoutput(0) + "\t" + CEX.getoutput(0) + "\t" + CEY.getoutput(0)
						+ VAL.getoutput(0) + "\t" + COL.getoutput(0) + "\t" + ROW.getoutput(0) + "\n") # "\t" + BAN.getoutput(0) + "\n")
			print "\t", raster, period, model, "described"

			outFile.close()

print "\n \t Process done!!"  
