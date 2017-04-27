# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Extract by mask grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ExtractByMask.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ex: python ExtractByMask.py S:\observed\gridded_products\worldclim\Global_30s_v2 D:\CIAT\Workspace\palvarez\wcl_v2 D:\CIAT\_tools\AdminBoundaries\Colombia\_by_departaments\CA_adm\CA0.shp ALL"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout =sys.argv[2]
mask =sys.argv[3]
wildcard = sys.argv[4]

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
	rasters = sorted(gp.ListRasters("*", "TIF"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "TIF"))

# Lopping around the grids
for raster in rasters:
	
	# Extract by mask function
	gp.ExtractByMask_sa(raster, mask, dirout + "\\" + os.path.basename(raster))
	print "\t", os.path.basename(raster), "extracted"

print "\n\t Process done!!"