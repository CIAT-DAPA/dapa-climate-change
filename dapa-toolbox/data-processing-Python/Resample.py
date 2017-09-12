# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 30th, 2010
# Purpose: Resample grids in a workspace
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Resample.py <dirbase> <dirout> <resolution> <method> <wildcard>"
	print "    - ie: python Resample.py D:\Workspace\grids D:\Workspace\_resampled 0.5 NEAREST ALL"
	print " dirbase	: Is the folder where your ESRI-Grid files are located"
	print " dirout	: Is the folder where the ESRI-grids files are created"
	print " resolution	: Is a numeric value indicating the resolution of the output files in arc-minutes"
	print " method : Resample types:"
	print "\t NEAREST Nearest neighbor assignment This is the default" 
	print "\t BILINEAR Bilinear interpolation"
	print "\t CUBIC Cubic convolution"
	print "\t MAJORITY Majority resampling"
	print "	wildcard : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
resolution = sys.argv[3]
method = sys.argv[4]
wildcard = sys.argv[5]

resol= float(resolution)/60

# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "\n~~~~~~~~~~~"
print " RESAMPLE  "
print "~~~~~~~~~~~\n"

# Set workspace
gp.workspace = dirbase 

# Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids
for raster in rasters:
	
	OutRaster = dirout + "\\" + raster
	if not gp.Exists(OutRaster):
		
		# Resample function
		gp.Resample_management(raster, OutRaster, str(resol), "NEAREST")	
		gp.AddMessage( "\t" + " " + raster + " " + "resampled" )
		
gp.AddMessage("\n \t ====> DONE!! <====")  
