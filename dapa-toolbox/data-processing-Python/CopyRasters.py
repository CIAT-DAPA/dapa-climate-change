# ---------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Purpouse: Copy grids files to another location
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python CopyRasters.py <dirbase> <dirout> <wildcard> <switch>"
	print "   - ex: python CopyRasters.py D:\Workspace D:\Workspace ALL NO"
	print " dirbase	: Is the folder where your ESRI-Grid files are located"
	print " dirout	: Is the folder where the ESRI-grids files are created"
	print "	wildcard : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	print " switch	: Remove ESRI-Grid files of base directory after copied"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]
switch = sys.argv[4]

# Clear screen
os.system('cls')

print "\n~~~~ COPY RASTERS ~~~~\n"

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
		gp.AddMessage( "\t" + " " +  raster + " " +  "copied" )
	
		# Remove asciis
		if switch == "YES":
			gp.delete_management(raster)

gp.AddMessage("\n \t ====> DONE!! <====")