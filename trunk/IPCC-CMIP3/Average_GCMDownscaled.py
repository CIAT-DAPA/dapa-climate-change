# ------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: June 16th 2011
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Average_GCMDownscaled.py M:\climate_change\IPCC_CMIP3 A1B D:\climate_change\IPCC_CMIP3 30s downscaled D:\Masks\COL_adm\COL_adm0.dbf"
	print "   Syntax	: <code.py>, <dirbase>, <scenario>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirout	: Out folder of txt describe archive"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: disaggregated, interpolated or downscaled"	
	sys.exit(1)


dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]
resolution = sys.argv[4]
type = sys.argv[5]
mask = sys.argv[6]

#Clear screen
os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + type + " " + str(resolution) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of scenarios and models
scenariolist = "A1B", "A2", "B1"
periodlist = "2020_2049", "2040_2069", "2070_2099"#  "2010_2039", "2030_2059", "2050_2079", "2060_2089", 
# wildlist = "bio" , "cons"

# for scenario in scenariolist:

modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))

for period in periodlist:

# for wild in wildlist:
	gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\bccr_bcm2_0\\2020_2049"
	variablelist = gp.ListRasters(wild + "*", "GRID")
	
	diroutMean = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Ensemble\\" + period
	if not os.path.exists(diroutMean):
		os.system('mkdir ' + diroutMean)
	diroutascii = diroutMean + "\\_asciis"
	if not os.path.exists(diroutascii):
		os.system('mkdir ' + diroutascii)
	# diroutSTD = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Multimodel_STD\\" + period
	# if not os.path.exists(diroutSTD):
		# os.system('mkdir ' + diroutSTD)

	for variable in variablelist:		
		lista = ""
		
		for model in modellist:
		
			raster = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\" + variable
			print "   .. adding\t" + model + "\t\t" + os.path.basename(raster)
			lista = lista + ";" + raster
		
		LISTA = "\"" + lista[1:] + "\""		
		
		OutRasterMean =  diroutMean + "\\" + variable
		# gp.toolbox = "SA"
		# X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
		# gp.Extent = "-83 -5 -65 17"
		gp.CellStatistics_sa(LISTA, OutRasterMean, "MEAN")

		OutAscii = diroutascii + "\\" + os.path.basename(OutRasterMean) + ".asc"
		print "    Converting " + OutAscii
		gp.RasterToASCII_conversion(OutRasterMean, OutAscii)
		InZip = diroutascii + "\\" + os.path.basename(OutRasterMean).split("_")[0] + "_asc.zip"
		os.system('7za a ' + InZip + " " + OutAscii)
		os.remove(OutAscii)
		
		# diroutcut = diroutMean + "\\_cut" 
		# if not os.path.exists(diroutcut):
			# os.system('mkdir ' + diroutcut)				
		# OutRasterMeanCut = diroutcut + "\\" + os.path.basename(OutRasterMean)
		
		# if not gp.Exists(OutRasterMeanCut):
			# print "\n\t   ..averaging " + variable + ""
			# if not gp.Exists(OutRasterMean):
				# gp.CellStatistics_sa(LISTA, OutRasterMean, "MEAN")
			# gp.ExtractByMask_sa(OutRasterMean, mask, OutRasterMeanCut)
			# gp.delete_management(OutRasterMean)
			# print "\n\t   ..average " + variable + " done"
		# else:
			# print "\n\t   ..average " + variable + " done"
		
		
		
		# OutRasterSTD =  diroutSTD + "\\" + variable
		# if not gp.Exists(OutRasterSTD):
			# print "\n\t   ..calculing STD " + variable + "\n"
			# gp.CellStatistics_sa(LISTA, OutRasterSTD, "STD")
		# else:
			# print "\n\t   ..calculation STD " + variable + " done\n"

# gp.workspace = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Multimodel_Mean\\" + period
# print "\n---> Processing: " + country, model, period

# gp.toolbox="management"
# for month in range (1, 12 + 1, 1):
	
	# OutDtrRes = gp.workspace + "\\dtr_" + str(month)  
	# OutDelta = diroutResample + "\\delta_" + str(month)
	# OutDtr = gp.workspace + "\\dtr_" + str(month)
	# OutTmeanRes = diroutResample + "\\tmean_" + str(month)  
	# OutTmean = gp.workspace + "\\tmean_" + str(month)
	# OutPrecRes = diroutResample + "\\prec_" + str(month)  
	
	# if not gp.Exists(OutDtrRes):
		# print "dtr_" + str(month)
		# InExpression = gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutDelta)
		# InExpression = OutDelta + " * 0.1"
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutDtr)
		# # gp.Resample_management(OutDtr, OutDtrRes , res, "NEAREST")			
		# gp.delete_management(OutDelta)
		# # gp.delete_management(OutDtr)
		
	# if not gp.Exists(OutTmeanRes):
		# print "tmean_" + str(month)
		# InExpression = gp.workspace + "\\tmean_" + str(month) + " * 0.1"
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutTmeanRes)
		# gp.delete_management(OutTmean)
		# gp.CopyRaster_management(OutTmeanRes, OutTmean)	
		
	
	# if not gp.Exists(OutPrecRes):
		# print "prec_" + str(month)
		# gp.Resample_management(gp.workspace + "\\prec_" + str(month), OutPrecRes , res, "NEAREST")		

# for month in range (1, 19 + 1, 1):	
	# raster = "bio_" + str(month)
	# if os.path.basename(raster) == "bio_1"  or os.path.basename(raster) == "bio_2" or os.path.basename(raster) == "bio_4" or os.path.basename(raster) == "bio_5" or os.path.basename(raster) == "bio_6" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_8" or os.path.basename(raster) == "bio_9" or os.path.basename(raster) == "bio_10" or os.path.basename(raster) == "bio_11":
		# if not gp.Exists(diroutResample + "\\" + raster):
			# print raster
			# InExpression = raster + " * 0.1"
			# gp.SingleOutputMapAlgebra_sa(InExpression, diroutResample + "\\" + raster)          
			# gp.delete_management(gp.workspace + "\\" + raster)
			# gp.CopyRaster_management(diroutResample + "\\" + raster, gp.workspace + "\\" + raster)	
			
	# else: 
		# if not gp.Exists(diroutResample + "\\" + raster):
			# print raster
			# gp.Resample_management(raster, diroutResample + "\\" + raster , res, "NEAREST")

# print "All countries resampled!!!" 

# print "~~~~~~~~~~~~~~~~~~~~~~"
# print "  CONVERT TO ASCII	 "
# print "~~~~~~~~~~~~~~~~~~~~~~"
# gp.workspace = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Multimodel_Mean\\" + period
# # for model in modellist:
# diroutResample = gp.workspace + "\\tmp"
# if not os.path.exists(diroutResample):
	# os.system('mkdir ' + diroutResample)
	# # for country in countrylist:

		# # # Create Dir Resample folders
		# # gp.workspace = dirout + "\\" + country + "_extract_4km\\" + model

		
# diroutAscii = gp.workspace + "\\col_a1b_2030"
# if not os.path.exists(diroutAscii):
	# os.system('mkdir ' + diroutAscii)

# rasters = gp.ListRasters("*", "GRID")
# res = "0.0166666676"
# for raster in rasters:
	# gp.Resample_management(raster, diroutResample + "\\" + raster , res, "NEAREST")
	# if os.path.basename(raster)[0:3] == "bio":
		# OutAscii = diroutAscii + "\\a1b_2020_2049_ensemble_" + raster + "_1.asc"
	# else:
		# OutAscii = diroutAscii + "\\a1b_2020_2049_ensemble_" + raster + ".asc"
	# print "Converting " + os.path.basename(OutAscii)
	# gp.Extent = "-83 -5 -65 17"
	# gp.SnapRaster = "D:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_30s\Multimodel_Mean\2020_2049\tmp\bio"
	# gp.RasterToASCII_conversion(diroutResample + "\\" + raster, OutAscii)

# print "done!!!"    
			
print "Process done!!!"    
