# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Convert to tiff
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Grid2Tiff.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python Grid2Tiff.py D:\climate_change\IPCC_CMIP3\SRES_A2\downscaled\Global_30s\Multimodel_Mean D:\climate_change\IPCC_CMIP3\SRES_A2\downscaled\Global_30s\Multimodel_Mean_tiff"
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
# resolution = sys.argv[4]
# method = sys.argv[5]

os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " RESAMPLE DISAGGREGATED "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# Get a list of periods and models
periodlist = "2010_2039", "2040_2069", "2070_2099"
# modellist = os.listdir(dirbase + "SRES_" + scenario + "\disaggregated")

# Correct resolution
# res = float(resolution) / 60


# for model in modellist:
model = "Multimodel_Mean"       
for period in periodlist:

	# Create Dir Resample folders
	diroutResample = dirout  + "\\" + period + "\\_cut"
	if not os.path.exists(diroutResample):
		os.system('mkdir ' + diroutResample)

	# Set workspace
	gp.workspace = dirbase + "\\" + period + "\\_cut"
	print "\n---> Processing: " + dirbase 

	# Get a list of rasters in workspace    
	rasters = gp.ListRasters("", "GRID")

	for raster in rasters:

		OutRaster = diroutResample + "\\" + raster

		#Resampling process
		if not gp.Exists(OutRaster) and not raster == "cons_mths":
			print raster 
			os.system("gdal_translate -of GTiff " + gp.workspace + "\\" + raster + " " + OutRaster + ".tif")
			
			print "    " + OutRaster + " converted"

		else:
			print "    " + OutRaster + " converted"

print "done!!!"    
