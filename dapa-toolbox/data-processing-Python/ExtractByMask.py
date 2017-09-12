# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Extract by mask grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ExtractByMask.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ex: python ExtractByMask.py D:\Workspace D:\Workspace\_cut D:\Workspace\_mask\mask ALL"
	print " dirbase	: Is the folder where your ESRI-Grid files are located"
	print " dirout	: Is the folder where the cut ESRI-Grid files are created"
	print " mask	: Input mask data defining areas to extract (ESRI-grid file or shapefile)."
	print "	wildcard : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	
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
	gp.AddMessage( "\t" + " " +  os.path.basename(raster) + " " +  "extracted" )

gp.AddMessage("\n \t ====> DONE!! <====")