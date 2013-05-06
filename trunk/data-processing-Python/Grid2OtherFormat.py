# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert GRID to other format in a workspace 
# Date: April 11th, 2010
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Grid2OtherFormat.py <dirbase> <dirout> <wildcard> <switch> <format>"
	print "   - ex: python Grid2OtherFormat.py D:\Workspace D:\Workspace\_grids ALL YES TIFF"
	print " The format of the output raster dataset"
	print  "\t BMP — Bitmap graphic raster dataset format"
	print  "\t GIF — Graphic Interchange Format for raster datasets"
	print  "\t GRID — ESRI's GRID raster dataset format"
	print  "\t IMAGINE Image — ERDAS IMAGINE raster data format"
	print  "\t JP2000 — JPEG 2000 raster dataset format"
	print  "\t JPEG — Joint Photographic Experts Group raster dataset format"
	print  "\t PNG — Portable Network Graphic raster dataset format"
	print  "\t TIFF — Tag Image File Format for raster datasets"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]
switch = sys.argv[4]
format = sys.argv[5]

# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# Set workspace
gp.workspace = dirbase 

# Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids
for raster in rasters:
	
	if not gp.Exists(OutAscii):	
		
		# Raster to other format function
		gp.RasterToOtherFormat_conversion(raster, dirout, format)
		print "\t", raster, "converted"
	
	if switch == "YES":
		gp.delete_management(raster)
		
print "Process done!!"