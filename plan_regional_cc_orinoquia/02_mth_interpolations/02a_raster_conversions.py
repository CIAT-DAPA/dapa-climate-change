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
	print "   - ex: python 02a_raster_conversions.py D:\cenavarro\col-cormacarena\monthly-interpolations\average\tile-1 D:\cenavarro\col-cormacarena\monthly-interpolations\average\tile-1 ALL NO"
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
	asclist = sorted(glob.glob(dirbase + "\\*.tif"))
else:	
	asclist = sorted(glob.glob(dirbase + "\\" + wildcard + "*.tif"))

print "\t", asclist

# Lopping around the grids
for raster in asclist:
	
	# Define output
	OutAscii = dirout + "\\" + os.path.basename(raster)[:-3]
	print os.path.basename(OutAscii)
	if not gp.Exists(OutAscii):	
		
		# Raster to ascii function
		# gp.RasterToASCII_conversion(raster, OutAscii)
		gp.RasterToOtherFormat_conversion(raster,dirout,"GRID")
		print "\t", raster, "converted"
	
	if switch == "YES":
		gp.delete_management(raster)
		
print "Process done!!"

	