# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Extract by mask grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ExtractSRTM_hill.py <srtm> <shpdir> <dirout>"
	print "   - ex: python 01-ExtractSRTM_hill.py S:\observed\gridded_products\srtm\Altitude_30s\alt D:\CIAT\_tools\AdminBoundaries\SHP_files D:\CIAT\climate_change\csa_profiles"
	sys.exit(1)

# Arguments
inraster =sys.argv[1]
shpdir =sys.argv[2]
dirout =sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear screen
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     EXTRACT BY MASK      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

countrylist = "tza", "npl", "zmb", "moz", "pak", "btn", "bgd", "ben", "gmb", "civ", "phl"

for country in countrylist:

	diroutraster = dirout + "\\" + country
	if not os.path.exists(diroutraster):
		os.system('mkdir ' + diroutraster)		

	# Extract by mask function
	gp.ExtractByMask_sa(inraster, shpdir + "\\" + country + "_adm\\" + country + "0.shp", diroutraster + "\\srtm")
	print "\t", country, "srtm extracted"

	# Process: Hillshade
	gp.Hillshade_sa(diroutraster + "\\srtm", diroutraster + "\\hillshade", "325", "50", "NO_SHADOWS", "0.5")

print "\n\t Process done!!"