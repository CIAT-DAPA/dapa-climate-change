# -----------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Claculate zonal statistics for diseggregated, interpolated, anomalies or downscaled surfaces
# -----------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: reclassify_gcm.py <dirbase> <dirout> <table>"
	print "	  - ie: python reclassify_gcm.py D:\Documents\Desktop\agrihack_climate_data\cmip5\downscaled_ensemble D:\Documents\Desktop\agrihack_climate_data\cmip5 D:\Documents\Desktop\agrihack_climate_data\observed\worldclim\Global_10min D:\Documents\Desktop\agrihack_climate_data\cmip5\ranges.dbf"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
dirwcl = sys.argv[3]
# resolution = sys.argv[3]
table = sys.argv[4]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    reclassify GCM " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


#Get lists of sress and models
rcplist = "rcp26", "rcp45", "rcp60","rcp85"
periodlist = "2020_2049", "2040_2069", "2060_2089","2070_2099"
varlist = "tmin", "tmax", "prec", "tmean"

gp.toolbox = "SA"
gp.Extent = "-26 -47 53 38"
					
# Looping scenarios
for rcp in rcplist:

	# Looping around periods
	for period in periodlist:
		
		# Set workspace by each model
		gp.workspace = dirbase + "\\" + rcp + "\\global_10min\\" + period
		
		# Define and create output directory
		diroutanom = dirout + "\\anomalies\\" + rcp + "\\" + period
		if not os.path.exists(diroutanom):
			os.system('mkdir ' + diroutanom)
		
		# Define and create output directory
		diroutshp = dirout + "\\anomalies_shp\\" + rcp + "\\" + period
		if not os.path.exists(diroutshp):
			os.system('mkdir ' + diroutshp)
		
		# Define and create output directory
		diroutjson = dirout + "\\anomalies_json\\" + rcp + "\\" + period
		if not os.path.exists(diroutjson):
			os.system('mkdir ' + diroutjson)
			
		for var in varlist:
		
			for month in range (1, 12 + 1, 1):
				
				print "\t", var, period, rcp
				
				# Calculate anomalies
				if var == "prec":
					
					if not gp.Exists(diroutanom + "\\" + var + "_" + str(month)):
						InExpression = " ( ( Float(" + gp.workspace + "\\" + var + "_" + str(month) + ") / Float(" + dirwcl + "\\" + var + "_" + str(month) + " ) ) - 1 ) * 100 " 
						gp.SingleOutputMapAlgebra_sa(InExpression,  diroutanom + "\\" + var + "_" + str(month))
					
					outRaster = diroutanom + "\\" + var + "_" + str(month) + "_r"
					if not gp.Exists(outRaster):
						reclassifyRanges = "-60 -40 1; -40 -20 2; -20 -15 3; -15 -10 4; -10 -5 5; -5 0 6; 0 5 7; 5 10 8; 10 15 9; 15 20 10; 20 40 11; 40 60 12"
						gp.Reclassify_sa(diroutanom + "\\" + var + "_" + str(month), "Value", reclassifyRanges, outRaster, "NODATA")
					
				else:
					if not gp.Exists(diroutanom + "\\" + var + "_" + str(month)):
						InExpression = " ( Float ( " + gp.workspace + "\\" + var + "_" + str(month) + " ) - Float ( " + dirwcl + "\\" + var + "_" + str(month) + " ) ) / 10" 
						gp.SingleOutputMapAlgebra_sa(InExpression,  diroutanom + "\\" + var + "_" + str(month))

					outRaster = diroutanom + "\\" + var + "_" + str(month) + "_r"
					if not gp.Exists(outRaster):
						reclassifyRanges = "0 0.5 1; 0.5 1 2; 1 1.5 3; 1.5 2 4; 2 2.5 5; 2.5 3 6; 3 3.5 7; 3.5 4 8; 4 5 9; 5 6 10; 6 7 11; 7 8 12"
						gp.Reclassify_sa(diroutanom + "\\" + var + "_" + str(month), "Value", reclassifyRanges, outRaster, "NODATA")

				OutPolygonFeatures = diroutshp + "\\" + var + "_" + str(month) + "_tmp.shp"
				OutPolygonFeatures_diss = diroutshp + "\\" + var + "_" + str(month) + ".shp"
				if not gp.Exists(OutPolygonFeatures_diss):
					gp.RasterToPolygon_conversion(outRaster, OutPolygonFeatures, "NO_SIMPLIFY")
					gp.Dissolve_management(OutPolygonFeatures, OutPolygonFeatures_diss, "GRIDCODE")
				
					if var == "prec":
						gp.joinfield (OutPolygonFeatures_diss, "GRIDCODE", table, "ID", "ranges_pre")
					else:
						gp.joinfield (OutPolygonFeatures_diss, "GRIDCODE", table, "ID", "ranges_tem")
				
					gp.delete_management(OutPolygonFeatures)
				
				OutGeoJSON = diroutjson + "\\" + var + "_" + str(month) + ".json"
				if not os.path.exists(diroutjson + "\\" + var + "_" + str(month) + ".json"):
					os.system("ogr2ogr -f GeoJSON " + OutGeoJSON + " " + OutPolygonFeatures_diss)
				
print "\n \t Process done!!"  
