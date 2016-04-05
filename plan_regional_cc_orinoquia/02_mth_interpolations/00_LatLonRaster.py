# ---------------------------------------------------------
# Autor: Carlos Navarro
# ---------------------------------------------------------

import arcpy, os, sys, string, glob
from arcpy import env

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python LatLonRaster.py S:\observed\gridded_products\srtm\Altitude_30s\alt D:\CIAT\Projects\col-cormacarena\_mask\_region ame"
	sys.exit(1)

arcpy.CheckOutExtension("spatial")

# Arguments
dem = sys.argv[1]
dirout = sys.argv[2]
region = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")
os.system('cls')

print "\n EXTRACT DEM"
# res = float(2.5) / 60
# OutAltRes = dirout + "\\alt_30s"
OutAlt = dirout + "\\altitude"
if not gp.Exists(OutAlt + ".asc"):
	arcpy.Clip_management(dem, "-113 -40 -34 25", OutAlt)
	# gp.Resample_management(OutAltRes, OutAlt , res, "NEAREST")
	# gp.delete_management(OutAltRes)

	
print "\n CREATING MASK"
OutRaster = dirout + "\\mask"
if not gp.Exists(OutRaster):
	InExpression = "(" + OutAlt + " * 0) + 1"
	arcpy.gp.RasterCalculator_sa(InExpression, OutRaster)
	
print "\n CREATING LAT RASTER"
OutRaster2 = dirout + "\\latitude"
if not gp.Exists(OutRaster2):
	InExpression2 = OutRaster + " * $$Ymap"
	arcpy.gp.RasterCalculator_sa(InExpression2, OutRaster2)
	

print "\n CREATING LON RASTER"
OutRaster3 = dirout + "\\longitude"
if not gp.Exists(OutRaster3):
	InExpression3 = OutRaster + " * $$Xmap"
	arcpy.gp.RasterCalculator_sa(InExpression3, OutRaster3)
	
	
print "\n SPLIT IN TILES"
arcpy.SplitRaster_management(raster, diroutGridsVar, raster + "_", "NUMBER_OF_TILES", "GRID", "#", str(countryDic [country]), "#", "0", "PIXELS", "#", "#")

	
gp.RasterToASCII_conversion(OutAlt, dirout + "\\alt-prj-" + region + ".asc")
gp.RasterToASCII_conversion(OutRaster2, dirout + "\\lat-prj-" + region + ".asc")
gp.RasterToASCII_conversion(OutRaster3, dirout + "\\lon-prj-" + region + ".asc")
gp.delete_management(OutAlt)
gp.delete_management(OutRaster)
print "\t ..done!!"