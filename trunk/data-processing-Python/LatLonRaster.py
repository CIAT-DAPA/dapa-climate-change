# ---------------------------------------------------------
# Autor: Carlos Navarro
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python LatLonRaster.py H:\CIAT\climate_change\strm_data\altitude_30s\alt D:\CIAT\Projects\ecu-hidroelectrica\02_baseline\_region ecu"
	sys.exit(1)

# Arguments
dem = sys.argv[1]
dirout = sys.argv[2]
region = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")
os.system('cls')

print "\n EXTRACT DEM"
res = float(2.5) / 60
OutAltRes = dirout + "\\alt_30s"
OutAlt = dirout + "\\altitude"
if not gp.Exists(OutAlt):
	gp.clip_management(dem, "-81 2 -77 -5", OutAltRes)
	gp.Resample_management(OutAltRes, OutAlt , res, "NEAREST")
	gp.delete_management(OutAltRes)
	
	
print "\n CREATING MASK"
OutRaster = dirout + "\\mask"
if not gp.Exists(OutRaster):
	InExpression = "(" + OutAlt + " * 0) + 1"
	gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
	
print "\n CREATING LAT RASTER"
OutRaster2 = dirout + "\\latitude"
if not gp.Exists(OutRaster2):
	InExpression2 = OutRaster + " * $$Ymap"
	gp.SingleOutputMapAlgebra_sa(InExpression2, OutRaster2)
	

print "\n CREATING LON RASTER"
OutRaster3 = dirout + "\\longitude"
if not gp.Exists(OutRaster3):
	InExpression3 = OutRaster + " * $$Xmap"
	gp.SingleOutputMapAlgebra_sa(InExpression3, OutRaster3)
	
	
gp.RasterToASCII_conversion(OutAlt, dirout + "\\alt-prj-" + region + ".asc")
gp.RasterToASCII_conversion(OutRaster2, dirout + "\\lat-prj-" + region + ".asc")
gp.RasterToASCII_conversion(OutRaster3, dirout + "\\lon-prj-" + region + ".asc")

print "\t ..done!!"