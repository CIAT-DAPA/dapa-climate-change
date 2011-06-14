# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Sumariza grids de un workspace con una salida
# ---------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Sum_grids.py D:\climate_change\RCM_Data\SRES_A1B\HadCM3Q3\daily_grids\1959\SurPress D:\Workspace\temp Sum1"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
outgrid = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ SUM GRIDS ~~~~\n"

gp.workspace = dirbase 

# Get a list of grids in the workspace of each folder
print "\t ..listing grids"
dsList = gp.ListDatasets("*", "all")
lista = ""
for ds in dsList:
	lista = lista + ';' + ds 
LISTA = "\"" + lista[1:] + "\""
print LISTA
OutRaster = dirout + "\\" + str(outgrid)

# Process: Cell Statistics...
print "\t ..summing"
gp.CellStatistics_sa(LISTA, OutRaster, "SUM")
print "\t ..done!!"