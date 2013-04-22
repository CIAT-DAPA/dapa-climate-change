# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 30th, 2010
# Purpose: Resample grids in a workspace
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Resample.py <dirbase> <dirout> <resolution> <method> <wildcard>"
	print "    - ie: python Resample.py D:\Workspace D:\Workspace\_resampled 0.5 NEAREST ALL"
	print " Units Resolution: arcminutes"
	print " Resample types:"
	print "\t NEAREST Nearest neighbor assignment This is the default" 
	print "\t BILINEAR Bilinear interpolation"
	print "\t CUBIC Cubic convolution"
	print "\t MAJORITY Majority resampling"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
resolution = sys.argv[3]
method = sys.argv[4]
wildcard = sys.argv[5]

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
		gp.Resample_management(raster, OutRaster, str(resolution), "NEAREST")	
		print "\t", raster, "resampled"
		
print " Process done!!!"    
