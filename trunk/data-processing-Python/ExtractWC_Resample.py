#--------------------------------------------------
# Description: Extrae valores grids Worldclim por puntos
# Author: Carlos Navarro
# Actualizado: 18/03/2011
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractWC_Resample.py K:\ClimateData\WorldClim_data\Global_30s D:\Workspace\tmp_2 D:\Workspace\extract_countries\_extracts"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirtemp = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

countrylist = "col", "eth", "ind" #,"uga", "ken", "tza"# "bgd", "mli", "sen", "ner", "bfa", "gha", , "nep"
countryDc = {"col": "-83 -5 -65 17 ", "bgd": "87 19 94 28 ", "mli": "-13 8 6 27 ", "sen": "-19 11 -10 18 ", "ner": "-1 10 17 25 ",
			 "bfa": "-6.5 8 3 17 ", "gha": "-4 3 2 13 ", "eth": "32 0 49 17 ", "uga": "28 -3 36 6 ", "ken": "33 -7 43 7 ", "tza": "28 -14 41 1 ", "nep": "79 24 89 32 "}

res = "0.0166666676"
gp.workspace = dirbase 
variablelist = "prec", "tmax", "tmean", "tmin"

gp.CheckOutExtension("Spatial")

# for country in countrylist:
	# print " Extracting WC.. " + country 
	# dsList = sorted(gp.ListRasters("*", "GRID"))
	# for ds in dsList:
		
		# print ds

		# #Set local variables
		# # InPointsFC = mask 
		# diroutcountry = dirout + "\\" + country + "_extractWC"
		# if not os.path.exists(diroutcountry):
			# os.system('mkdir ' + diroutcountry)
		# OutRaster = diroutcountry + "\\" + ds

		# #Check out Spatial Analyst extension license
		
		# if not gp.Exists(OutRaster):
		# #Process
		# # gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")
		# # X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
			# gp.clip_management(ds, str(countryDc[country]), OutRaster)

country = "col"
# for country in countrylist:
print " Resampling WC.. " + country
gp.workspace = dirout + "\\" + country + "_extractWC"
diroutResample = dirout + "\\" + country + "_extractWC_4km"
if not os.path.exists(diroutResample):
	os.system('mkdir ' + diroutResample)

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
		gp.Extent = "-83 -5 -65 17"
		gp.SnapRaster = "D:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_30s\Multimodel_Mean\2020_2049\tmp\bio"
		gp.Resample_management(OutDtr, OutDtrRes , res, "NEAREST")			
		gp.delete_management(OutDelta)
		gp.delete_management(OutDtr)
		
	if not gp.Exists(OutTmeanRes):
		print "tmean_" + str(month)
		InExpression = gp.workspace + "\\tmean_" + str(month) + " * 0.1"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutTmean)    
		gp.Extent = "-83 -5 -65 17"
		gp.SnapRaster = "D:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_30s\Multimodel_Mean\2020_2049\tmp\bio"
		gp.Resample_management(OutTmean, OutTmeanRes , res, "NEAREST")	
		gp.delete_management(OutTmean)
	
	if not gp.Exists(OutPrecRes):
		print "prec_" + str(month)
		gp.Extent = "-83 -5 -65 17"
		gp.SnapRaster = "D:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_30s\Multimodel_Mean\2020_2049\tmp\bio"
		gp.Resample_management(gp.workspace + "\\prec_" + str(month), OutPrecRes , res, "NEAREST")		

for month in range (1, 19 + 1, 1):	
	raster = "bio_" + str(month)
	if os.path.basename(raster) == "bio_1"  or os.path.basename(raster) == "bio_2" or os.path.basename(raster) == "bio_4" or os.path.basename(raster) == "bio_5" or os.path.basename(raster) == "bio_6" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_8" or os.path.basename(raster) == "bio_9" or os.path.basename(raster) == "bio_10" or os.path.basename(raster) == "bio_11":
		if not gp.Exists(diroutResample + "\\" + raster):
			print raster
			InExpression = raster + " * 0.1"
			gp.SingleOutputMapAlgebra_sa(InExpression, dirtemp + "\\" + raster)          
			gp.Extent = "-83 -5 -65 17"
			gp.SnapRaster = "D:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_30s\Multimodel_Mean\2020_2049\tmp\bio"
			gp.Resample_management(dirtemp + "\\" + raster, diroutResample + "\\" + raster , res, "NEAREST")	
			gp.delete_management(dirtemp + "\\" + raster)
	else: 
		if not gp.Exists(diroutResample + "\\" + raster):
			print raster
			gp.Extent = "-83 -5 -65 17"
			gp.SnapRaster = "D:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_30s\Multimodel_Mean\2020_2049\tmp\bio"
			gp.Resample_management(raster, diroutResample + "\\" + raster , res, "NEAREST")

# for country in countrylist:
	# " Converting to Ascii WC.." + country
gp.workspace = dirout + "\\" + country + "_extractWC_4km"
diroutAscii = dirout + "\\" + country + "_a1b_2030"
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