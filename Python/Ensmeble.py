# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Sumariza grids de un workspace con una salida
# ---------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Ensemble.py Y:\EcoCrop-development\climate\world_10min_future_a1b Y:\EcoCrop-development\climate\world_10min_future_a1b\ensemble"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
# outgrid = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ ENSEMBLE ~~~~\n"
modelList = sorted(os.listdir(dirbase))
print modelList
ascList = sorted(glob.glob(dirbase + "\\bccr_bcm2_0\\*.asc"))
for asc in asclist:
	print os.path.basename(asc)
	for model in modelList:
		ds = dirbase + "\\" + str(model) + "\\" + os.path.basename(asc)
		lista = lista + ';' + ds 
	LISTA = "\"" + lista[1:] + "\""
	print LISTA
	OutRaster = dirout + "\\" + os.path.basename(asc)
	
# dsList = gp.ListDatasets("*", "all")
# lista = ""
# for ds in dsList:
	# lista = lista + ';' + ds 
# LISTA = "\"" + lista[1:] + "\""
# print LISTA
# OutRaster = dirout + "\\" + str(outgrid)

# # Process: Cell Statistics...
# print "\t ..summing"
# gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
# print "\t ..done!!"