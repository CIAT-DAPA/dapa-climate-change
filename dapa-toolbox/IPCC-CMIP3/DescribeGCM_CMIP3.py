# ---------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: August 24th, 2010
# Purpose: Describe properties of Grids Downscaled, Disaggregated, anomalies or Interpolated datasets
# ---------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

# Syntax 
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <DescribeGCM_CMIP3.py> <dirbase> <descfile> <sres> <resolution> <models> <periods> <variable>"
	print "   - ex: python DescribeGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace\output a1b 30s cnrm_cm3,bccr_bcm2_0 2010_2039 ALL"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   descfile	: Is the folder where you want to create description file to outputs Rasters"
	print "   sres		: IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "	  models	: Global Climate Models. If you want to choose some models, only separated with commas without spaces. Use 'ALL' to choose all available models"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces. E.g.: 2010_2039,2020_2049,2030_2059. Use 'ALL' to process all the periods"
	print "   variable	: Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write 'PREC'. Use 'ALL' to convert all data in the workspace"
	sys.exit(1)

# Set variables 
dirbase = sys.argv[1]
descfile = sys.argv[2]
sres = sys.argv[3]
resolution = sys.argv[4]
models = sys.argv[5]
periods = sys.argv[6]
variable = sys.argv[7]

# Clean screen
os.system('cls')

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Describe " + " " + sres + " " + str(resolution) + str(periods) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"


#Get lists of models
if models == "ALL":
	modellist = sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\Global_" + str(resolution) ))
else: 
	modellist = models.split(",")
	
# Get lists of periods
if periods == "ALL":
	periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
else:	
	periodlist = periods.split(",")

gp.AddMessage("Models: " + str(modellist))
gp.AddMessage( "Periods: " + str(periodlist) )		
	
# output file with txt extension
if not os.path.exists(descfile):
	os.system('mkdir ' + descfile)
	
descfile = 	descfile + '\\Describe_GCM_'+sres+'_'+resolution +'.txt'

if not os.path.isfile(descfile):
	outFile = open(descfile, "w")
	outFile.write("GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" 
				+ "\t" + "TOP" + "\t" + "LEFT" + "\t" + "RIGHT" + "\t" + "BOTTOM" + "\t" + "CELLSIZEX" + "\t" + "CELLSIZEY" + "\t" 
				+ "VALUETYPE" + "\t" + "COLUMNCOUNT" + "\t" + "ROWCOUNT" + "\t" + "BANDCOUNTUSER" + "\n")
	outFile.close()

# Looping around periods
for periods in periodlist:
		
	# Looping around models 
	for model in modellist:

		#Set workspace
		gp.workspace = dirbase + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + periods

		if variable == "ALL":
			var = "*"
		else:	
			var = variable + "*"
		
		#Get a list of raster into the workspace
		rasters = sorted(gp.ListRasters(var, "GRID"))
		
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
			#BAN = gp.GetRasterProperties_management(raster, "BANDCOUNTUSER")
			
			# Writting grid characteristics
			outFile = open(descfile, "a")
			outFile.write(sres + "\t" + model + "\t" + periods + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" 
						+ TOP.getoutput(0) + "\t" + LEF.getoutput(0) + "\t" + RIG.getoutput(0) + "\t" + BOT.getoutput(0) + "\t" + CEX.getoutput(0) + "\t" + CEY.getoutput(0)
						+ VAL.getoutput(0) + "\t" + COL.getoutput(0) + "\t" + ROW.getoutput(0)+ "\n") #+ "\t" + BAN.getoutput(0) + "\n")
			gp.AddMessage( "\t" + " " + raster + " " + periods + " " + model + " " + "described" )

			outFile.close()

gp.AddMessage("\n \t ====> DONE!! <====")  
