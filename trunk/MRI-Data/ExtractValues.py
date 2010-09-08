#--------------------------------------------------
# Description: Extrae valores grids MRI por puntos
# Author: Carlos Navarro
# Actualizado: 25/08/10
#--------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractValues.py E:\MRI_grids E:\MRI_Analysis\Extracted\ E:\MRI_grids\mask\stations\tmean_latin.shp 1979 2003 tmean"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
mask = sys.argv[3]
inityear = int(sys.argv[4])
finalyear = int(sys.argv[5])
variable = sys.argv[6]

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp.workspace = dirbase + "\\" + variable + "_monthly\\SP0A"

for year in range(inityear, finalyear + 1, 1):

    # Get a list of grids in the workspace of each folder
    dsList = gp.ListDatasets(variable + "_" + str(year) + "*", "all")
    
    lista = ""
    for ds in dsList:
        print ds

        # Set local variables
        InPointsFC = mask 
        OutPointsFC = dirout + "\\tmean_monthly_latin\\" + ds

        # Check out Spatial Analyst extension license
        gp.CheckOutExtension("Spatial")

        # Process: Cell Statistics...
        gp.ExtractValuesToPoints_sa(InPointsFC, ds, OutPointsFC, "NONE", "VALUE_ONLY")


print "done"
