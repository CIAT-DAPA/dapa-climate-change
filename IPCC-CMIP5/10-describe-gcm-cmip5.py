# ---------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: January 20th, 2014
# Purpose: Describe properties of Grids Downscaled, Disaggregated, anomalies or Interpolated cmip5 datasets
# ---------------------------------------------------------------------------------------------------------

# python 10-describe-gcm-cmip5.py \\dapadfs\data_cluster_2\gcm\cmip5\downscaled rcp26 30s

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax: 10-describe-gcm-cmip5.py <dirbase> <rcp> <resolution>"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
rcp = sys.argv[2]
res = sys.argv[3]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

#Get lists of models and periods
periodlist = ["2020_2049", "2040_2069", "2060_2089", "2070_2099"]
modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + res))

print "\nAvailable models: " + str(modellist)
descfile = dirbase +"\\"+ rcp + "_" + res + "_describe.txt"

if not os.path.isfile(descfile):
	outFile = open(descfile, "w")
	outFile.write("RCP" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" 
				+ "\t" + "TOP" + "\t" + "LEFT" + "\t" + "RIGHT" + "\t" + "BOTTOM" + "\t" + "CELLSIZEX" + "\t" + "CELLSIZEY" + "\t" 
				+ "VALUETYPE" + "\t" + "COLUMNCOUNT" + "\t" + "ROWCOUNT" + "\n") #+ "\t" + "BANDCOUNTUSER" + "\n")
	outFile.close()

# Looping around periods

# Looping around models 
for model in modellist:

	for period in periodlist:
	
		print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		print "     Describe " + " " + rcp + " "  + str(model) + " "  + str(period) 
		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
		
		#Set workspace
		gp.workspace = dirbase + "\\" + rcp + "\\global_" + res + "\\" + model + "\\r1i1p1\\" + period

		#Get a list of raster into the workspace
		rasters = sorted(gp.ListRasters("*", "GRID"))
		
		# Looping around rasters 
		for raster in rasters:
			try:
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
							+ COL.getoutput(0) + "\t" + ROW.getoutput(0) + "\n") # "\t" + VAL.getoutput(0) + "\t" + BAN.getoutput(0)  + "\n")
				print "\t", raster, period, model, "described"

				outFile.close()
			except:
				if gp.getmessage(2) == "ERROR 001100: Failed because no statistics are available.":
					# Parameters
					TOP = gp.GetRasterProperties_management(raster, "TOP")
					LEF = gp.GetRasterProperties_management(raster, "LEFT")
					RIG = gp.GetRasterProperties_management(raster, "RIGHT")
					BOT = gp.GetRasterProperties_management(raster, "BOTTOM")
					CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
					CEY = gp.GetRasterProperties_management(raster, "CELLSIZEY")
					VAL = gp.GetRasterProperties_management(raster, "VALUETYPE")
					COL = gp.GetRasterProperties_management(raster, "COLUMNCOUNT")
					ROW = gp.GetRasterProperties_management(raster, "ROWCOUNT")			
					
					print "Error statistics"
					# Writting grid characteristics
					outFile = open(descfile, "a")
					outFile.write(rcp + "\t" + model + "\t" + period + "\t" + raster + "\t" + "NA"+ "\t" + "NA" + "\t" + "NA" + "\t" + "NA" + "\t" 
								+ TOP.getoutput(0) + "\t" + LEF.getoutput(0) + "\t" + RIG.getoutput(0) + "\t" + BOT.getoutput(0) + "\t" + CEX.getoutput(0) + "\t" + CEY.getoutput(0)
								+ COL.getoutput(0) + "\t" + ROW.getoutput(0) + "\t" + "No_statistics" + "\n") # "\t" + VAL.getoutput(0) + "\t" + BAN.getoutput(0)  + "\n")
					print "\t", raster, period, model, "described"

					outFile.close()					
print "\n \t Process done!!"  
