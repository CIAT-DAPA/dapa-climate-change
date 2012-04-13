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
	print "    - ie: python CreateShapefiles.py D:\Masks\COL_adm\COL_adm1.shp D:\Masks\COL_adm\_by_departaments HASC_1"
	sys.exit(1)

# ---------------------------------------------------------------------------
# Notes
# Units Resolution: arcminutes
# Resample types:   NEAREST Nearest neighbor assignment This is the default 
#                   BILINEAR Bilinear interpolation 
#                   CUBIC Cubic convolution 
#                   MAJORITY Majority resampling
# ---------------------------------------------------------------------------

inshape = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
fieldname = sys.argv[3]


os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CREATE SHAPEFILES   "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

cur = gp.SearchCursor(inshape,"","",fieldname,"")
row = cur.Next()

while row <> None:
	print str(row.HASC_1).split(".")[1]
	diroutshape = dirout + "\\" + str(row.HASC_1).split(".")[1] + "_adm"
	if not os.path.exists(diroutshape):
		os.system('mkdir ' + diroutshape)
	outshape = diroutshape + "\\" + str(row.HASC_1).split(".")[1] + "0.shp"
	gp.select_analysis(inshape, outshape, fieldname + " = '" + row.HASC_1 + "'")
	row = cur.Next()

