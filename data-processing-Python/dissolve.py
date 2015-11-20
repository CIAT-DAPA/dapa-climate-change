# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Convert to tiff
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 1:
	os.system('cls')
	print "\n Too few args"
	print "    - ie: python dissolve.py"
	sys.exit(1)

# ---------------------------------------------------------------------------
# Notes
# Units Resolution: arcminutes
# Resample types:   NEAREST Nearest neighbor assignment This is the default 
#                   BILINEAR Bilinear interpolation 
#                   CUBIC Cubic convolution 
#                   MAJORITY Majority resampling
# ---------------------------------------------------------------------------

os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " 	   DISSOLVE        "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

gp.Dissolve_management("D:\\CIAT\\Projects\\ecu-hidroelectrica\\04-Performance-gcm\\Administrative_boundaries\\ECU0.shp", "D:\\CIAT\\Projects\\ecu-hidroelectrica\\04-Performance-gcm\\Administrative_boundaries\\ECU_adm\\ECU0.shp", "ISO")

print "done!!!"    
