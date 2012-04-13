# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Convierte asciis a grids en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Ascii2Grid.py D:\Workspace\Danny\all\all"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

gp.workspace = dirbase

# Get a list of grids in the workspace of each folder
print "\t ..listing grids into " + gp.workspace
asclist = sorted(glob.glob(gp.workspace + "\\*.asc"))
for asc in asclist:
	print os.path.basename(asc)
	if not gp.Exists(os.path.basename(asc)[:-7]):
		# os.system("gdal_translate -of AAIGRID " + asc + " " + os.path.basename(asc)[:-4])
		# InExpression = asc + " * 0.01"
		# gp.SingleOutputMapAlgebra_sa(InExpression, gp.workspace + "\\" + os.path.basename(asc).split("_")[0] + "_" + os.path.basename(asc).split("_")[1])
		gp.ASCIIToRaster_conversion(asc, os.path.basename(asc)[:-7], "FLOAT")
		# gp.ASCIIToRaster_conversion(asc, os.path.basename(asc)[:-4], "INTEGER")
		# os.remove(asc)
	
print "\t ..done!!"