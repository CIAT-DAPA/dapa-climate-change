# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 08/11/2011
# Pourpose: Create shapefiles using a selection by atributes
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python CreateShapefiles.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python CreateShapefiles.py D:\CIAT\_tools\AdminBoundaries\Global\110m\110m-admin-0-regions.shp D:\CIAT\_tools\AdminBoundaries\Continents\regions UNREGION"
	sys.exit(1)


inshape = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
fieldname = sys.argv[3]


os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox = "analysis"

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CREATE SHAPEFILES   "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

cur = gp.SearchCursor(inshape,"","",fieldname,"")
row = cur.Next()

while row <> None:
	print str(row.UNREGION).split(".")
	diroutshape = dirout 
	if not os.path.exists(diroutshape):
		os.system('mkdir ' + diroutshape)
	outshape = diroutshape + "\\" + str(row.UNREGION).split(".")[2] + ".shp"
	outshape_buff = diroutshape + "\\" + str(row.UNREGION).split(".")[2] + "_buff0_5.shp"
	gp.select_analysis(inshape, outshape, fieldname + " = '" + row.UNREGION + "'")
	gp.buffer(outshape, outshape_buff, "0.5", "FULL")
	row = cur.Next()

