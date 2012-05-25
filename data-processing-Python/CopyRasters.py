# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Copy rasters to another location
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python CopyRasters.py D:\Workspace\Danny\all\all_grids D:\Workspace\Danny\all\Monfreda_Without_0"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]

# Check out Spatial Analyst extension license
os.system('cls')

print "\n~~~~ COPY RASTERS ~~~~\n"

gp.workspace = dirbase 

# Get a list of grids in the workspace of each folder
print "\t ..listing grids into " + gp.workspace

# for month in range(1, 12 + 1, 1):
	# raster = gp.workspace + "\\tean_" + str(month)
	# gp.copy_management(raster, dirout + "\\tmean_" + str(month))
	
rasters = sorted(gp.ListRasters("*", "GRID"))
for raster in rasters:
	gp.copy_management(raster, dirout + "\\" + raster)
	
print "\t ..done!!"