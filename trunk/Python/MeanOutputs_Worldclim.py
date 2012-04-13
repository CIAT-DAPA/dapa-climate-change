#-----------------------------------------------------------
# Description: Promedia/suma raster TRMM
# Author: Carlos Navarro
# Date: 15/04/11
#-----------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python MeanOutputs_Worldclim.py D:\Workspace\ColPlains\Wordclim_interpolations\outputs\rain"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~"
print "   MEAN WORLDCLIM   "
print "~~~~~~~~~~~~~~~~"
print "\n"

# folderlist = sorted(os.listdir(dirbase))
# for folder in folderlist:
	# gp.workspace = dirbase + "\\" + folder + "\\tile-1"
	
	# rslist = sorted(glob.glob(gp.workspace + "\\*zip"))
	# for rs in rslist:
		# if not os.path.exists(rs[:-4]):
			# os.system('7za e -yo' + gp.workspace + " " + rs)
		
	# rsList = glob.glob(gp.workspace + "\\*.asc")
	# for rs in rsList:
		# print os.path.basename(rs)
		# gp.RasterToOtherFormat_conversion(os.path.basename(rs), gp.workspace, "GRID")
		# gp.delete_management(rs)

# for month in range (1, 12 + 1, 1):
	# for folder in folderlist:
		# gp.workspace = dirbase + "\\" + folder + "\\tile-1"

		# lista = ""
		# raster = gp.workspace + "\\tmin_" + str(month)
		# print "\t  " + os.path.basename(raster)
		# lista = lista + ";" + raster
	
	# LISTA = "\"" + lista[1:] + "\""		
		
	# print "\n\t   ..averaging "
	
	# OutRaster = dirbase + "\\tmin_" + str(month)
	# gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
		
	# print "\t ..done!!"

# folderlist = sorted(os.listdir(dirbase))
# # for folder in folderlist:
gp.workspace = dirbase 
dsList = sorted(gp.ListRasters("ra*", "GRID"))
lista = ""
for ds in dsList:
	lista = lista + ';' + ds 
LISTA = "\"" + lista[1:] + "\""
print LISTA

# Process: Cell Statistics...
print "\t ..averaging"
gp.CellStatistics_sa(LISTA, gp.workspace + "\\sum", "SUM")

# lista2 = ""
# for folder in folderlist:
	# ds = dirbase + "\\" + folder + "\\tile-1\\mean"	
	# lista2 = lista2 + ';' + ds 
# LISTA2 = "\"" + lista2[1:] + "\""
# print LISTA2

# gp.CellStatistics_sa(LISTA2, dirbase + "\\mean", "MEAN")
# gp.CellStatistics_sa(LISTA2, dirbase + "\\std", "STD")

# InExpression = dirbase + "\\std" + " / " + dirbase + "\\mean"
# gp.SingleOutputMapAlgebra_sa(InExpression, dirbase + "\\cv")
# print "\t ..done!!"
	
# print "Done!!!!"
