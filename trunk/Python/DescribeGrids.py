# ---------------------------------------------------------------------------
# Autor: Carlos Navarro
# Fecha: Noviembre 23 de 2011
# Proposito: Describe grids en un workspace
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Describe_Downscaled.py M:\climate_change\IPCC_CMIP3\"
	sys.exit(1)

dirbase = sys.argv[1]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     DESCRIBE GRIDS      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

if os.path.isfile(dirout + "\\_describe_data.txt"):
    outFile = open(dirout + "\\_describe_data.txt", "a")
else:
    outFile = open(dirout + "\\_describe_data.txt", "w")

outFile.write("GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")
rasters = gp.ListRasters("", "GRID")
for raster in sorted(rasters):
	print raster
	MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
	MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
	MEA = gp.GetRasterProperties_management(raster, "MEAN")
	STD = gp.GetRasterProperties_management(raster, "STD")
	CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
	outFile = open(dirout + "\\_describe_data.txt", "a")
	outFile.write(raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

outFile.close()

print "done!!!"    
