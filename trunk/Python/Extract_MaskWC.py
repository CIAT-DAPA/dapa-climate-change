# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_MaskWC.py D:\climate_change\IPCC_CMIP3 A1B D:\Workspace\tmp D:\Workspace\extract_countries\by_regions 30min downscaled D:\Workspace\extract_countries\_masks"
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
dirtemp = sys.argv[3]
if not os.path.exists(dirtemp):
    os.system('mkdir ' + dirtemp)
dirout = sys.argv[4]
resolution = sys.argv[5]
type = sys.argv[6]
dirmask = sys.argv[7]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

countrylist = "rus","afr", "asi", "auz", "eur", "lat", "nam",  "glo"#"ind", "eth", "bgd", "mli", "sen", "ner", "bfa", "gha", "nep", "uga", "ken", "tza",
countryDc = {"ind": "67 6 99 37 ", "bgd": "87 19 94 28 ", "mli": "-13 8 6 27 ", "sen": "-19 11 -10 18 ", "ner": "-1 10 17 25 ",
			 "bfa": "-6.5 8 3 17 ", "gha": "-4 3 2 13 ", "nep": "79 24 89 32 "} #"eth": "32 0 49 17 ", "uga": "28 -3 36 6 ", "ken": "33 -7 43 7 ", "tza": "28 -14 41 1 ", }
		 
period = "2020_2049"
# modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
res = "0.5"

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

# print "Available models: " + str(modellist)
country = "rus"
# for country in countrylist:
model = "WorldClim" 
# for model in modellist:
	# gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
gp.workspace = "D:\climate_change\WorldClim"
if os.path.exists(gp.workspace):# and not os.path.exists(dirout + "\\_extract_" + country + "_done.txt"):
	print "\n---> Processing: " + country, model, period
	diroutraster = dirout + "\\" + country + "_extract\\" + model

	if not os.path.exists(diroutraster):
		os.system('mkdir ' + diroutraster)

	#Get a list of raster in workspace
	rasters = sorted(gp.ListRasters("*", "GRID"))
	for raster in rasters:
		print "    Extracting " + raster
		OutRaster = diroutraster + "\\" + raster
		if not gp.Exists(OutRaster):
			gp.Extent = dirmask + "\\" + country
			gp.SnapRaster = dirmask + "\\" + country
			gp.Mask = dirmask + "\\" + country
			# gp.clip_management(raster, str(countryDc[country]), OutRaster)
			gp.ExtractByMask_sa(raster, dirmask + "\\" + country, OutRaster)
			
	print "Done!!"


else:
	print "The model " + model + " " + period + " is already processed"
	print "Processing the next period \n"

checkTXT = open(dirout + "\\_extract_" + country + "_done.txt", "w")
checkTXT.close()	

print "All countries extracted!!!" 




print "~~~~~~~~~~~~~~~~~~~~~~"
print "      RESAMPLE  		 "
print "~~~~~~~~~~~~~~~~~~~~~~"

# for model in modellist:
	# if not model == "bccr_bcm2_0":
model = "WorldClim" 
country = "rus"
# for country in countrylist:
	
# diroutResample = dirout + "\\" + country + "_extract_4km\\" + model

# if not os.path.exists(diroutResample):
	# os.system('mkdir ' + diroutResample)

# Set workspace
gp.workspace = dirout + "\\" + country + "_extract\\" + model
print "\n---> Processing: " + country, model, period

for month in range (1, 12 + 1, 1):
	
	# OutDtrRes = diroutResample + "\\dtr_" + str(month)  
	OutDelta = dirtemp + "\\delta_" + str(month)
	OutDtr = gp.workspace + "\\dtr_" + str(month)
	# OutTmeanRes = diroutResample + "\\tmean_" + str(month)  
	OutTmean = dirtemp + "\\tmean_" + str(month)
	# OutPrecRes = diroutResample + "\\prec_" + str(month)  
	
	if not gp.Exists(OutDtr):
		print "dtr_" + str(month)
		InExpression = gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
		gp.SingleOutputMapAlgebra_sa(InExpression, OutDelta)
		InExpression = OutDelta + " * 0.1"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutDtr)
		# gp.Resample_management(OutDtr, OutDtrRes , res, "NEAREST")			
		gp.delete_management(OutDelta)
		# gp.delete_management(OutDtr)
		
	if not gp.Exists(OutTmean):
		print "tmean_" + str(month)
		InExpression = gp.workspace + "\\tmean_" + str(month) + " * 0.1"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutTmean)          
		# gp.Resample_management(OutTmean, OutTmeanRes , res, "NEAREST")	
		gp.delete_management(gp.workspace + "\\tmean_" + str(month))
		gp.CopyRaster_management(OutTmean, gp.workspace + "\\tmean_" + str(month))
		gp.delete_management(OutTmean)
	
	# if not gp.Exists(OutPrecRes):
		# print "prec_" + str(month)
		# gp.Resample_management(gp.workspace + "\\prec_" + str(month), OutPrecRes , res, "NEAREST")		

for month in range (1, 19 + 1, 1):	
	raster = "bio_" + str(month)
	if os.path.basename(raster) == "bio_1"  or os.path.basename(raster) == "bio_2" or os.path.basename(raster) == "bio_5" or os.path.basename(raster) == "bio_6" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_8" or os.path.basename(raster) == "bio_9" or os.path.basename(raster) == "bio_10" or os.path.basename(raster) == "bio_11":
		# if not gp.Exists(diroutResample + "\\" + raster):
		print raster
		InExpression = raster + " * 0.1"
		gp.SingleOutputMapAlgebra_sa(InExpression, dirtemp + "\\" + raster)          
		# gp.Resample_management(dirtemp + "\\" + raster, diroutResample + "\\" + raster , res, "NEAREST")	
		gp.delete_management(gp.workspace + "\\" + raster)
		gp.CopyRaster_management(dirtemp + "\\" + raster, gp.workspace + "\\" + raster)
		gp.delete_management(dirtemp + "\\" + raster)
	# else: 
		# if not gp.Exists(diroutResample + "\\" + raster):
			# print raster
			# gp.Resample_management(raster, diroutResample + "\\" + raster , res, "NEAREST")

print "All countries resampled!!!" 

print "~~~~~~~~~~~~~~~~~~~~~~"
print "  CONVERT TO ASCII	 "
print "~~~~~~~~~~~~~~~~~~~~~~"
model = "WorldClim" 
country = "rus"
# for model in modellist:

# for country in countrylist:

gp.workspace = dirout + "\\" + country + "_extract\\" + model
print "\n---> Processing: " + country, model, period

diroutAscii = dirout + "\\" + country + "_a1b_2030\\"
if not os.path.exists(diroutAscii):
	os.system('mkdir ' + diroutAscii)

rasters = gp.ListRasters("*", "GRID")

for raster in rasters:
	if os.path.basename(raster)[0:3] == "bio":
		OutAscii = diroutAscii + "\\current_" + raster + "_1.asc"
	else:
		OutAscii = diroutAscii + "\\current_" + raster + ".asc"
	print "Converting " + os.path.basename(OutAscii)
	gp.RasterToASCII_conversion(raster, OutAscii)

print "done!!!"    