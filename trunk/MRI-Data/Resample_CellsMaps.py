#---------------------------------------------------------------------
# Description:  Resample Cells Maps 0.5 degrees from 1 to 2.5 degrees
# Author: Carlos Navarro
# Date: 02/12/10
#---------------------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Resanple_CellsMaps.py D:\MRI_Analysis\Maps prec world"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
variable = sys.argv[2]
region = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')
gp.OverWriteOutput = 1

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~"
print "  RESAMPLE CELLS MAPS  "
print "~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

degreelist =  "2.5", "2", "1.5", "1"
gp.workspace = dirbase + "\\CellsMaps_0.5\\" +  variable + "_" + region

for degree in degreelist:

	print "     ---> Calculating to " + degree + " degrees " + variable + " " + region

	rasters = gp.ListRasters("", "GRID")
	for raster in rasters:

		OutRaster = dirbase + "\\CellsMaps_" + str(degree) + "\\" +  variable + "_" + region + "\\" + raster
		#Resampling process
		if not gp.Exists(OutRaster):
			gp.Resample_management(raster, OutRaster , str(degree), "NEAREST")
			print "     ---> " + OutRaster + " Resampled"

		else:
			print "     ---> " + OutRaster + " Resampled"
			
print "Done!!!!"