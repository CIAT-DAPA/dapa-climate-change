# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# 		First, cut the climate data with cut_process.aml and cut_GCM.aml scripts. 
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_MaskGCM_for_Analogues.py F:\Analogues_GCM_data\tiles_by_countries F:\Analogues_GCM_data\ExtractByCountry_asciis 30s YES"
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
# dirtemp = sys.argv[2]
# if not os.path.exists(dirtemp):
    # os.system('mkdir ' + dirtemp)
dirout = sys.argv[2]
resolution = sys.argv[3]
tiled = sys.argv[4]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

# countrylist = "afr", "asi", "auz", "eur", "lat", "nam", "rus", "glo"

print "~~~~~~~~~~~~~~~~~~~~~~"
print "  CONVERT TO ASCII	 "
print "~~~~~~~~~~~~~~~~~~~~~~\n"

varlist = "bio", "dtr", "prec", "tmean"
period = "2020_2049"
countrylist =  sorted(os.listdir(dirbase))
country = "aus_30s" 
# for country in countrylist:

modellist =  sorted(os.listdir(dirbase + "\\" + country))
for model in modellist:
	
	if tiled == "NO":
	
		if model == "current":


			gp.workspace = dirbase + "\\" + country + "\\" + model 
			
			print "\n---> Processing: " + model,"current"
			
			diroutAscii = dirout + "\\" + country + "\\" + model
			if not os.path.exists(diroutAscii):
				os.system('mkdir ' + diroutAscii)
			
			rasters = gp.ListRasters("*", "GRID")

			for raster in rasters:
				
				if os.path.basename(raster)[0:3] == "bio":
					OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
				elif os.path.basename(raster)[0:4] == "prec" or os.path.basename(raster)[0:3] == "dtr" or os.path.basename(raster)[0:5] == "tmean":
					OutAscii = diroutAscii + "\\" + model + "_" + raster + ".asc"
				
				print "Converting " + os.path.basename(OutAscii)
				if not gp.Exists(OutAscii):
					gp.RasterToASCII_conversion(raster, OutAscii)

			trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
			for trashfile in trashList:
				os.remove(trashfile)
						
		else:
		

			gp.workspace = dirbase + "\\" + country + "\\" + model + "\\" + period
		
			print "\n---> Processing: " + model, period
			
			diroutAscii = dirout + "\\" + country + "\\" + model 
			if not os.path.exists(diroutAscii):
				os.system('mkdir ' + diroutAscii)
			
			rasters = gp.ListRasters("*", "GRID")

			for raster in rasters:
				
				if os.path.basename(raster)[0:3] == "bio":
					OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + "_1.asc"
				elif os.path.basename(raster)[0:4] == "prec" or os.path.basename(raster)[0:3] == "dtr" or os.path.basename(raster)[0:5] == "tmean":
					OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + ".asc"
				
				print "Converting " + os.path.basename(OutAscii)
				if not gp.Exists(OutAscii):
					gp.RasterToASCII_conversion(raster, OutAscii)
					
			trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
			for trashfile in trashList:
				os.remove(trashfile)
	
	
	else:
	
		if model == "current":

			for var in varlist:
				gp.workspace = dirbase + "\\" + country + "\\" + model + "\\" + var
				
				print "\n---> Processing: " + model,"current"
				
				diroutAscii = dirout + "\\" + country + "\\" + model + "\\" + var + "_asciis"
				if not os.path.exists(diroutAscii):
					os.system('mkdir ' + diroutAscii)
				
				rasters = gp.ListRasters("*", "GRID")

				for raster in rasters:
					
					if os.path.basename(raster)[0:3] == "bio":
						OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
					elif os.path.basename(raster)[0:4] == "prec" or os.path.basename(raster)[0:3] == "dtr" or os.path.basename(raster)[0:5] == "tmean":
						OutAscii = diroutAscii + "\\" + model + "_" + raster + ".asc"
					
					print "Converting " + os.path.basename(OutAscii)
					if not gp.Exists(OutAscii):
						gp.RasterToASCII_conversion(raster, OutAscii)

				trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
				for trashfile in trashList:
					os.remove(trashfile)
						
		else:
		
			for var in varlist:
				gp.workspace = dirbase + "\\" + country + "\\" + model + "\\" + period + "\\" + var
			
				print "\n---> Processing: " + model, period
				
				diroutAscii = dirout + "\\" + country + "\\" + model + "\\" + var + "_asciis"
				if not os.path.exists(diroutAscii):
					os.system('mkdir ' + diroutAscii)
				
				rasters = gp.ListRasters("*", "GRID")

				for raster in rasters:
						
					if os.path.basename(raster)[0:3] == "bio":
						OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + "_1.asc"
					elif os.path.basename(raster)[0:4] == "prec" or os.path.basename(raster)[0:3] == "dtr" or os.path.basename(raster)[0:5] == "tmean":
						OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + ".asc"
					
					print "Converting " + os.path.basename(OutAscii)
					if not gp.Exists(OutAscii):
						gp.RasterToASCII_conversion(raster, OutAscii)
						
				trashList = sorted(glob.glob(diroutAscii + "\\*.prj"))
				for trashfile in trashList:
					os.remove(trashfile)
				
				
print "done!!!"    







