# ---------------------------------------------------------
# Autor: Carlos Navarro
# ---------------------------------------------------------

import arcpy, os, sys, string, glob, arcgisscripting, math
from arcpy import env
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 00_LatLonRaster_v2.py S:\observed\gridded_products\srtm\Altitude_30s\alt D:\CIAT\Projects\col-cormacarena\_mask\_region ame 2"
	sys.exit(1)

# Arguments
dem = sys.argv[1]
dirout = sys.argv[2]
region = sys.argv[3]
ntiles = sys.argv[4]

# Check out Spatial Analyst extension license
arcpy.CheckOutExtension("spatial")
os.system('cls')

env.workspace = dirout

print "\n EXTRACT DEM"
# res = float(2.5) / 60
# OutAltRes = dirout + "\\alt_30s"
OutAlt = dirout + "\\altitude"
if not arcpy.Exists(OutAlt):
	arcpy.Clip_management(dem, "-113 -40 -34 25", OutAlt)
	# gp.Resample_management(OutAltRes, OutAlt , res, "NEAREST")
	# gp.delete_management(OutAltRes)

	
print "\n CREATING MASK"
OutRaster = dirout + "\\mask"
if not arcpy.Exists(OutRaster):
	InExpression = "(" + OutAlt + " * 0) + 1"
	gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
	
print "\n CREATING LAT RASTER"
OutRaster2 = dirout + "\\latitude"
if not arcpy.Exists(OutRaster2):
	InExpression2 = OutRaster + " * $$Ymap"
	gp.SingleOutputMapAlgebra_sa(InExpression2, OutRaster2)
	

print "\n CREATING LON RASTER"
OutRaster3 = dirout + "\\longitude"
if not arcpy.Exists(OutRaster3):
	InExpression3 = OutRaster + " * $$Xmap"
	gp.SingleOutputMapAlgebra_sa(InExpression3, OutRaster3)
	
if int(ntiles) > 1:
	
	# dim = str(int(ntiles) ** 0.5) + " " + str(int(ntiles) ** 0.5) + " "
	dim = "1 2 "
	print "\n SPLIT IN TILES"
	arcpy.SplitRaster_management(OutAlt, dirout, "altitude", "NUMBER_OF_TILES", "GRID", "#",  dim, "#", "5", "DEGREES", "#", "#")
	arcpy.SplitRaster_management(OutRaster2, dirout, "latitude", "NUMBER_OF_TILES", "GRID", "#", dim, "#", "5", "DEGREES", "#", "#")
	arcpy.SplitRaster_management(OutRaster3, dirout, "longitude", "NUMBER_OF_TILES", "GRID", "#", dim, "#", "5", "DEGREES", "#", "#")

	varlist = "altitude", "latitude", "longitude"

	print "\n CONVERTING TO ASCII BY TILES"
	for i in range(0, int(ntiles), 1):
		
		diroutasc = dirout + "\\tile-" + str(i + 1)
		if not os.path.exists(diroutasc):
			os.system("mkdir " + diroutasc)

		for var in varlist:
			arcpy.RasterToASCII_conversion(dirout + "\\" + var + str(i), diroutasc + "\\" + var + ".asc")
			arcpy.Delete_management(dirout + "\\" + var + str(i))

		
print "\n CONVERTING TO ASCII REGIONAL"

if not arcpy.Exists(dirout + "\\lon-prj-" + region + ".asc"):
	gp.RasterToASCII_conversion(OutAlt, dirout + "\\alt-prj-" + region + ".asc")
	gp.RasterToASCII_conversion(OutRaster2, dirout + "\\lat-prj-" + region + ".asc")
	gp.RasterToASCII_conversion(OutRaster3, dirout + "\\lon-prj-" + region + ".asc")

print "\t ..done!!"