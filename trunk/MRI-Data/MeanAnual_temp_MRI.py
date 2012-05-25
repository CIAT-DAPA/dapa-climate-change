#-----------------------------------------------------------
# Description: Promedia los grids temperatura de los datos MRI
# Author: Carlos Navarro
# Date: 08/09/10
#-----------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python MeanAnual_temp_MRI.py M:\climate_change\IPCC_CMIP3\SRES_A2\disaggregated\Global_30s\miroc3_2_medres\2010_2039"
	sys.exit(1)

# Arguments
gp.workspace = sys.argv[1]


# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')


print "\n"
print "~~~~~~~~~~~~~~~"
print "  MEAN GRIDS   "
print "~~~~~~~~~~~~~~~"
print "\n"

gp.CellStatistics_sa("tmin_7;tmax_7", "E:\\Workspace\\tmean_7", "MEAN")

print "Done!!!!"
