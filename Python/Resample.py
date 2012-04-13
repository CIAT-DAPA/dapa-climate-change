# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Resample Downscaling Disaggregated
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Resample.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python Resample.py D:\Workspace\Request_Patricia\2020_2049 D:\Workspace\Request_Patricia\2020_2049_0_5d 0.5"
	sys.exit(1)

# ---------------------------------------------------------------------------
# Notes
# Units Resolution: arcminutes
# Resample types:   NEAREST Nearest neighbor assignment This is the default 
#                   BILINEAR Bilinear interpolation 
#                   CUBIC Cubic convolution 
#                   MAJORITY Majority resampling
# ---------------------------------------------------------------------------

dirbase = sys.argv[1]
# scenario = sys.argv[2]
dirout = sys.argv[2]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
resolution = sys.argv[3]
# method = sys.argv[5]
# dircut = sys.argv[3]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~"
print " RESAMPLE  "
print "~~~~~~~~~~~"

resolution = str(resolution)

# Set workspace
gp.workspace = dirbase 
rasters = gp.ListRasters("*", "GRID")
for raster in rasters:
	OutRaster = dirout + "\\" + raster
	if not gp.Exists(OutRaster):
		# gp.extent = "D:\climate_change\Baseline\WC_20km\snap\snap"
		# gp.SnapRaster = "D:\climate_change\Baseline\WC_20km\snap\snap"
		gp.Resample_management(raster, OutRaster, resolution, "NEAREST")	
		print raster,"resampled"
print "done!!!"    
