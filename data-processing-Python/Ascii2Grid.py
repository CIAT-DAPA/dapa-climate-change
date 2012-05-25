# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Ascii2Grid.py D:\Workspace D:\Workspace prec INTEGER NO"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcart = sys.argv[3]
type = sys.argv[4]
switch = sys.argv[5]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear window
os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

# Get a list of grids in the workspace of each folder
print "\t ..listing asciis into " + dirbase

if wildcard == "NO":
	asclist = sorted(glob.glob(dirbase + "\\*.asc"))
else:	
	asclist = sorted(glob.glob(dirbase + "\\" + wildcard + "*.asc"))

# Lopping around the asciis
for asc in asclist:
	print os.path.basename(asc)
	if not gp.Exists(os.path.basename(asc)[:-4]):
		
		# Gdal option
		# os.system("gdal_translate -of AAIGRID " + asc + " " + os.path.basename(asc)[:-4])
		
		# ArcGIS option
		gp.ASCIIToRaster_conversion(asc, dirout + "\\" + os.path.basename(asc)[:-4], type)
		
		# Remove asciis
		if switch == "YES":
			os.remove(asc)
	
print "\t ..done!!"