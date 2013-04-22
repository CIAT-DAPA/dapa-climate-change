# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Copy rasters to another location
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python CopyRasters.py <dirbase> <dirout> <wildcard> <switch>"
	print "   - ex: python CopyRasters.py D:\Workspace D:\Workspace ALL NO"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]

print "\n~~~~ COPY RASTERS ~~~~\n"

# Clear screen
os.system('cls')

# Set workspace
gp.workspace = dirbase 

# Create output folder
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

# Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids
print "\t ..copying grids \n"
for raster in rasters:
	
	if not gp.Exists(dirout + "\\" + raster):
	
		# Copy function
		gp.copy_management(raster, dirout + "\\" + raster)
		print "\t", raster, "copied"
	
		# Remove asciis
		if switch == "YES":
			gp.delete_management(raster)
		
print "\n \t Process done!!"