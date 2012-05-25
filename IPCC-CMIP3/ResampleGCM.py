# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 14/12/2011
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ResampleGCM.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python ResampleGCM.py D:\climate_change\IPCC_CMIP3 A1B D:\climate_change\IPCC_CMIP3 D:\climate_change\IPCC_CMIP3\tmp"
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
scenario = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
# resolution = sys.argv[4]
# method = sys.argv[5]
dirtemp = sys.argv[4]
# if not os.path.exists(dirtemp):
    # os.system('mkdir ' + dirtemp)

os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " 	RESAMPLE GCMs      "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# Get a list of periods and models
# periodlist = "2020_2049", "2010_2039"#"2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = os.listdir(dirbase + "\SRES_" + scenario + "\downscaled\Global_10min")

# Correct resolution
res = "0.5"

period = "2020_2049"
for model in modellist[18:]:
       
    # for period in periodlist:

	# Create Dir Resample folders
	diroutResample = dirout + "\SRES_" + scenario + "\downscaled\Global_30min" + "\\" + model + "\\" + period
	if not os.path.exists(diroutResample):
		os.system('mkdir ' + diroutResample)
	
	# Set workspace
	gp.workspace = dirbase + "\\SRES_" + scenario + "\\downscaled\\Global_10min\\" + model + "\\" + period
	print "\n---> Processing: " + model + "  " + period
	
	rasters = gp.ListRasters("*", "GRID")
	for raster in rasters:
		OutRaster = diroutResample + "\\" + raster
		if not gp.Exists(OutRaster):
			print raster,"resampled"
			gp.Resample_management(raster, OutRaster, res, "NEAREST")	

print "done!!!"    
