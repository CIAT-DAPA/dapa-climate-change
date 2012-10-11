# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Ascii2Grid.py D:\Maxent_Nicaragua\mxe_outputs\sp-coffea_arabica_1\projections\summarize D:\Maxent_Nicaragua\mxe_outputs\sp-coffea_arabica_1\projections\summarize NO FLOAT NO coffea_arabica"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]
type = sys.argv[4]
switch = sys.argv[5]
specie = sys.argv[6]

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
	
	outnamegrid = "ca" + os.path.basename(asc).split(specie)[1][:-4]
	print outnamegrid
	
	# ArcGIS option
	if not gp.Exists(dirout + "\\" + outnamegrid):
		gp.ASCIIToRaster_conversion(asc, dirout + "\\" + outnamegrid, type)
					
print "\t ..done!!"