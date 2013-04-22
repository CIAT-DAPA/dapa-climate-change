# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert GRID to ESRI Ascii in a workspace 
# Date: April 11th, 2010
# ---------------------------------------------------------


import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Grid2Ascii.py <dirbase> <dirout> <wildcard> <switch>"
	print "   - ex: python Grid2Ascii.py D:\Workspace D:\Workspace\_grids ALL YES"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]
switch = sys.argv[4]

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

gp.workspace = dirbase 

# Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids
for raster in rasters:
	
	# Define output
	OutAscii = dirout + "\\" + raster + ".asc"
	
	if not gp.Exists(OutAscii):	
		
		# Raster to ascii function
		gp.RasterToASCII_conversion(raster, OutAscii)
		print "\t", raster, "converted"
	
	if switch == "YES":
		gp.delete_management(raster)
		
print "Process done!!"

	