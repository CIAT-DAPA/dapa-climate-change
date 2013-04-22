# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Ascii2Grid.py <dirbase> <dirout> <wildcard> <type> <switch>"
	print "   - ex: python Ascii2Grid.py D:\Workspace D:\Workspace prec INTEGER NO"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]
type = sys.argv[4]
switch = sys.argv[5]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear screen
os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

# Create output folder
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

# Get a list of grids in the workspace
print "\t ..listing asciis into " + dirbase
if wildcard == "ALL":
	asclist = sorted(glob.glob(dirbase + "\\*.asc"))
else:	
	asclist = sorted(glob.glob(dirbase + "\\" + wildcard + "*.asc"))

# Lopping around the asciis
for asc in asclist:
	
	if not gp.Exists(os.path.basename(asc)[:-4]):
		
		# Convert function
		gp.ASCIIToRaster_conversion(asc, dirout + "\\" + os.path.basename(asc)[:-4], type)
		print "\t", os.path.basename(asc), "converted"
		
		# Remove asciis
		if switch == "YES":
			os.remove(asc)
	
print "\n \t Process done!!"