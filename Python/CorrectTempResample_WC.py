# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Resample Downscaling Disaggregated
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Resample_Disaggregated.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python CorrectTempResample_WC.py D:\climate_change\IPCC_CMIP3\Nepal_Extract A1B D:\climate_change\IPCC_CMIP3\NepalResample_Extract\WorldClim_2_5km D:\climate_change\IPCC_CMIP3\tmp"
	sys.exit(1)

dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
# resolution = sys.argv[4]
# method = sys.argv[5]
dirtemp = sys.argv[4]
if not os.path.exists(dirtemp):
    os.system('mkdir ' + dirtemp)

os.system('cls')

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "  CORRECT AND RESAMPLE  "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# Get a list of periods and models
# periodlist = "2020_2049", "2010_2039"#"2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = os.listdir(dirbase + "\\SRES_" + scenario + "\\downscaled\\Global_30s\\")
print modellist
# Correct resolution
res = "0.0208333345"


# for model in modellist:
    
	# period = "2020_2049"
	
	# # Create Dir Resample folders
diroutResample = dirout 
if not os.path.exists(diroutResample):
	os.system('mkdir ' + diroutResample)

	# Set workspace
gp.workspace = "D:\climate_change\IPCC_CMIP3\Nepal_Extract\WorldClim"
# print "\n---> Processing: " + model + "  " + period

for month in range (1, 12 + 1, 1):
	
	OutDtrRes = diroutResample + "\\dtr_" + str(month)  
	OutDelta = dirtemp + "\\delta_" + str(month)
	OutDtr = dirtemp + "\\dtr_" + str(month)
	OutTmeanRes = diroutResample + "\\tmean_" + str(month)  
	OutTmean = dirtemp + "\\tmean_" + str(month)
	OutPrecRes = diroutResample + "\\prec_" + str(month)  
	
	if not gp.Exists(OutDtrRes):
		print "dtr_" + str(month)
		InExpression = gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
		gp.SingleOutputMapAlgebra_sa(InExpression, OutDelta)
		InExpression = OutDelta + " * 0.1"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutDtr)
		gp.Resample_management(OutDtr, OutDtrRes , res, "NEAREST")			
		gp.delete_management(OutDelta)
		gp.delete_management(OutDtr)
		
	if not gp.Exists(OutTmeanRes):
		print "tmean_" + str(month)
		InExpression = gp.workspace + "\\tmean_" + str(month) + " * 0.1"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutTmean)          
		gp.Resample_management(OutTmean, OutTmeanRes , res, "NEAREST")	
		gp.delete_management(OutTmean)
	
	if not gp.Exists(OutPrecRes):
		print "prec_" + str(month)
		gp.Resample_management(gp.workspace + "\\prec_" + str(month), OutPrecRes , res, "NEAREST")		

for month in range (1, 19 + 1, 1):	
	raster = "bio_" + str(month)
	if os.path.basename(raster) == "bio_1"  or os.path.basename(raster) == "bio_2" or os.path.basename(raster) == "bio_4" or os.path.basename(raster) == "bio_5" or os.path.basename(raster) == "bio_6" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_8" or os.path.basename(raster) == "bio_9" or os.path.basename(raster) == "bio_10" or os.path.basename(raster) == "bio_11":
		if not gp.Exists(diroutResample + "\\" + raster):
			print raster
			InExpression = raster + " * 0.1"
			gp.SingleOutputMapAlgebra_sa(InExpression, dirtemp + "\\" + raster)          
			gp.Resample_management(dirtemp + "\\" + raster, diroutResample + "\\" + raster , res, "NEAREST")	
			gp.delete_management(dirtemp + "\\" + raster)
	else: 
		if not gp.Exists(diroutResample + "\\" + raster):
			print raster
			gp.Resample_management(raster, diroutResample + "\\" + raster , res, "NEAREST")

				
# for model in modellist:
    
	# period = "2020_2049"
	
	# Create Dir Resample folders
gp.workspace = dirout
diroutAscii = "D:\climate_change\IPCC_CMIP3\Current_Nepal_2_5km"
if not os.path.exists(diroutAscii):
	os.system('mkdir ' + diroutAscii)

rasters = gp.ListRasters("*", "GRID")

for raster in rasters:
	if os.path.basename(raster) == "bio_1" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_12":
		OutAscii = diroutAscii + "\\current_" + raster + "_1.asc"
	else:
		OutAscii = diroutAscii + "\\current_" + raster + ".asc"
	print "Converting " + os.path.basename(OutAscii)
	gp.RasterToASCII_conversion(raster, OutAscii)

print "done!!!"    
