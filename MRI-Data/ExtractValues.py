#--------------------------------------------------
# Description: Extrae valores grids MRI por puntos
# Author: Carlos Navarro
# Actualizado: 25/08/10
#--------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractValues.py D:\MRI_grids\prec_monthly\SP0A D:\mask\stations 1979 1999 C:\MRI_Analysis\Extracted\prec_monthly"
	sys.exit(1)

dirbase = sys.argv[1]
dirmask = sys.argv[2]
inityear = int(sys.argv[3])
finalyear = int(sys.argv[4])
dirout = sys.argv[5]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp.workspace = dirbase

for year in range(inityear, finalyear + 1, 1):

    # Get a list of grids in the workspace of each folder
    dsList = gp.ListDatasets("prec_" + str(year) + "*", "all")
    
    lista = ""
    for ds in dsList:
        print ds

        # Set local variables
        InPointsFC = dirmask + "\\prcp_latin.shp"
        OutPointsFC = dirout + "\\" + ds

        # Check out Spatial Analyst extension license
        gp.CheckOutExtension("Spatial")

        # Process: Cell Statistics...
        gp.ExtractValuesToPoints_sa(InPointsFC, ds, OutPointsFC, "NONE", "VALUE_ONLY")


print "done"
