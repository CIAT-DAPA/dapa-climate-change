# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Resample Downscaling Disaggregated
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Resample_Disaggregated.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python Resample_Disaggregated.py P:\climate_change\IPCC_CMIP3\ A1B F:\IPCC_CMIP3_process resolution method"
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
resolution = sys.argv[4]
method = sys.argv[5]

os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " RESAMPLE DISAGGREGATED "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# Get a list of periods and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = os.listdir(dirbase + "SRES_" + scenario + "\disaggregated")

# Correct resolution
res = float(resolution) / 60


for model in modellist:
       
    for period in periodlist:

        # Create Dir Resample folders
        diroutResample = dirout + "\\SRES_" + scenario + "\\resample_disaggregated\\Global_" + resolution + "min\\" + model + "\\" + period
        if not os.path.exists(diroutResample):
            os.system('mkdir ' + diroutResample)

        # Set workspace
        gp.workspace = dirbase + "SRES_" + scenario + "\\disaggregated\\" + model + "\\" + period
        print "\n---> Processing: " + dirbase + "SRES_" + scenario + "\\disaggregated\\" + model + "\\" + period

        # Get a list of rasters in workspace    
        rasters = gp.ListRasters("", "GRID")

        for raster in rasters:

            OutRaster = diroutResample + "\\" + raster

            #Resampling process
            if not gp.Exists(OutRaster) and not raster == "cons_mths":
                gp.Resample_management(raster, OutRaster , res, "NEAREST")
                print "    " + OutRaster + " Resampled"

            else:
                print "    " + OutRaster + " Resampled"

print "done!!!"    
