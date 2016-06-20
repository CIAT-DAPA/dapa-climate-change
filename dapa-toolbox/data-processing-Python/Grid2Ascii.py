# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert GRID to ESRI Ascii in a workspace 
# Date: April 11th, 2010
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Grid2Ascii.py <dirbase> <dirout> <wildcard> <switch>"
	print "   - ex: python Grid2Ascii.py D:\Workspace\grids D:\Workspace\ascii ALL NO"
	print " dirbase	: Is the folder where your ESRI-Grid files are located"
	print " dirout	: Is the folder where the ESRI-Asciis files are created"
	print "	wildcard : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	print " switch	: Remove ESRI-Grid files of base directory after copied"
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
print "     GRID TO ASCII     "
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
		gp.AddMessage( "\t"+ " " +  raster+ " " + "converted" )
	
	if switch == "YES":
		gp.delete_management(raster)
		
gp.AddMessage("\n \t ====> DONE!! <====")

	