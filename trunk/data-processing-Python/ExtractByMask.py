# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Extrae por mascara en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractByMask.py D:\climate_change\RCM_Data\Average_Models\2040_2069 D:\climate_change\RCM_Data\Average_Models\2040_2069\cut"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout =sys.argv[2]
mask =sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ EXTRACT BY MASK ~~~~\n"

gp.workspace = dirbase

# Get a list of grids in the workspace of each folder
print "\t ..listing grids into " + gp.workspace
rasterlist = gp.ListRasters("*", "GRID")		
for raster in rasterlist:
	print os.path.basename(raster)
	gp.ExtractByMask_sa(raster, mask, dirout + "\\" + os.path.basename(raster))
	# os.remove(asc)
	
print "\t ..done!!"