#-----------------------------------------------------------
# Description: Promedia/suma raster TRMM
# Author: Carlos Navarro
# Date: 15/04/11
#-----------------------------------------------------------

import arcgisscripting, os, sys
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

folderlist = sorted(os.listdir(dirbase))
for folder in folderlist:
	gp.workspace = dirbase + "\\" + folder + "\\tile-1"
	
	ziplist = sorted(glob.glob(dirinput + "\\*zip"))
	for zip in ziplist:
		if not os.path.exists(zip[:-4])
			os.system("7za x -yo " + gp.workspace + " " + zip)
		
	rsList = glob.glob(gp.workspace + "\\*.asc")
	for rs in rsList:
		print os.path.basename(rs)
		gp.RasterToOtherFormat_conversion(os.path.basename(rs), gp.workspace, "GRID")
		gp.delete_management(rs)

for month in range (1, 12 + 1, 1):
	for folder in folderlist:
		gp.workspace = dirbase + "\\" + folder + "\\tile-1"

		lista = ""
		raster = gp.workspace + "\\rain_" + str(month)
		print "\t  " + os.path.basename(raster)
		lista = lista + ";" + raster
	
	LISTA = "\"" + lista[1:] + "\""		
		
	print "\n\t   ..averaging "
	
	OutRaster = dirbase + "\\rain_" + str(month)
	gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
		
	print "\t ..done!!"

print "Done!!!!"
