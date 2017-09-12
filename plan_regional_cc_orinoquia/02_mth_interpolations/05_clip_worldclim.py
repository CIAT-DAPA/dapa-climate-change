# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Extract by mask grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python 05_clip_worldclim.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ex: python 05_clip_worldclim.py S:\observed\gridded_products\worldclim\Global_30s_v1_4 X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\baseline\tropico\baseline_wcl bio"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout =sys.argv[2]
wildcard = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear screen
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     EXTRACT BY MASK      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# Set Workspace
gp.workspace = dirbase

# Get a list of grids in the workspace 
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))

# Lopping around the grids
for raster in rasters:
	
	if not gp.exists(dirout + "\\" + os.path.basename(raster)):
		# Extract by mask function
		gp.clip_management(raster, "-114 -40 -34 25", dirout + "\\" + os.path.basename(raster))
		print "\t", os.path.basename(raster), "extracted"

print "\n\t Process done!!"