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
	print "   - ex: python ExtractByMask.py D:\CIAT\Projects\PNUMA\03-Projections\Avg_Anom_A2\2020_2049\no_cut D:\CIAT\Projects\PNUMA\03-Projections\Avg_Anom_A2\2020_2049 D:\CIAT\Projects\PNUMA\00-Administrative_boundaries\andesPeEcCo\andesPeruEcuadorCOL.shp ALL"
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
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))

# Lopping around the grids
for raster in rasters:
	
	# Extract by mask function
	gp.ExtractByMask_sa(raster, mask, dirout + "\\" + os.path.basename(raster))
	print "\t", os.path.basename(raster), "extracted"

print "\n\t Process done!!"