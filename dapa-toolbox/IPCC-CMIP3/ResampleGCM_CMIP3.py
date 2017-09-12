# -------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: 14/12/2011
# Purpose: Resample anomalies, disaggregated, interpolated or downscaled surfaces
# -------------------------------------------------------------------------------
#python F:\jtarapues\Request\_scripts\dapa\IPCC-CMIP3\ResampleGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled F:\jtarapues\Request\PRUEBAS_SCRIPTs\pr2 B1 5min 10min ALL ALL prec NEAREST
import arcgisscripting, os, sys, string, re
gp = arcgisscripting.create(9.3)

# Syntax
if len(sys.argv) < 10:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <ResampleGCM_CMIP3.py> <dirbase> <dirout> <sres> <resolution> <resol_resample> <models> <periods> <wildcard> <method> "
	print "	  - ie: python ResampleGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace\output B1 5min 10min ALL ALL prec NEAREST "
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   sres		: IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "   resol_resample: Is a numeric value indicating the output resolution in arc-minutes. The possibilities are 30s 2_5min 5min 10min"
	print "	  models	: Global Climate Models. If you want to choose some models, only separated with commas without spaces. Use 'ALL' to choose all available models"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces. E.g.: 2010_2039,2020_2049,2030_2059. Use 'ALL' to process all the periods"
	print "	  wildcard  : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	print "   method	: NEAREST Nearest neighbor assignment This is the default"
	print "			      BILINEAR Bilinear interpolation"
	print "				  CUBIC Cubic convolution"
	print "				  MAJORITY Majority resampling"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
resolution = sys.argv[4]
resol_resamp = sys.argv[5]
models = sys.argv[6]
periods = sys.argv[7]
wildcard = sys.argv[8]
method = sys.argv[9]


# Clean screen
os.system('cls')

resol_resample= (float(re.findall(r'[0-9]+', resol_resamp)[0])/60)

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Resample GCM " + " Sres " + sres + " " + str(resolution) + " Resol. Resample:" + str(resol_resamp)
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"



# Get lists of sress and models
if models == "ALL":
	modellist = sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\Global_" + str(resolution) ))
else: 
	modellist = models.split(",")

# Get lists of sress and models
if periods == "ALL":
	periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
else:	
	periodlist = periods.split(",")

gp.AddMessage("Models: " + str(modellist))
gp.AddMessage( "Periods: " + str(periodlist) )

# Looping around periods
for period in periodlist:

	# for model in modellist:
	for model in modellist:
		
		# Set workspace by each model
		gp.workspace = dirbase + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		
		# Define check file
		checkFile = dirout + "\\sres_" + sres + "\\" + model + "-" + period + "-resample-done.txt"
		if not os.path.exists(checkFile):
			
			gp.AddMessage( "\n Processing: " + sres + " " + period + " " + model + "\n" )
			
			# Define and create output directory
			diroutGrd = dirout + "\\sres_" + sres + "\\Global_" + str(resol_resamp) + "\\" + model + "\\" + period
			if not os.path.exists(diroutGrd):
				os.system('mkdir ' + diroutGrd)

			# Get a list of raster in workspace
			if wildcard == "ALL":
				rasters = sorted(gp.ListRasters("*", "GRID"))
			else:	
				rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))			
	
			for raster in rasters:
				
				# Define out ESRI-Grid
				outGrd = diroutGrd + "\\" + raster
				
				if not gp.Exists(outGrd):
					
					# Cut function
					gp.Resample_management(gp.workspace + "\\" + raster, outGrd, str(resol_resample), method)	
					gp.AddMessage( "\t" + " " +  raster + " " +  period + " " +  model + " " +  "Resampled" )
									
			checkFile = open(checkFile, "w")
			checkFile.close()
			print "\n " + sres + " " + period + " " + model + " processed\n"
		else:
			print "\n " + sres + " " + period + " " + model + " processed\n"

gp.AddMessage("\n \t ====> DONE!! <====")  