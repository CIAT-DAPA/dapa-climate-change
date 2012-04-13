# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Extrae por mascara en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractByMask_from_Asciis.py D:\Workspace\extract_countries\ensemble\global_a1b_2030 D:\Workspace\extract_countries\ensemble"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
# mask =sys.argv[2]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ EXTRACT BY MASK ~~~~\n"

gp.workspace = dirbase

# Get a list of grids in the workspace of each folder
# print "\t ..listing grids into " + gp.workspace
# rasterlist = gp.ListRasters("*", "GRID")		
# for raster in rasterlist:
	# print os.path.basename(raster)
	# OutRaster = gp.workspace + "\\_cut_COL\\" + os.path.basename(raster)
	# gp.ExtractByMask_sa(raster, mask, OutRaster)
	# # os.remove(asc)
gp.toolbox = "SA"
gp.Extent = "-120 -33 -57 34"

asclist = sorted(glob.glob(gp.workspace + "\\*.asc"))
for asc in asclist:
	print os.path.basename(asc)[23:-4]
	OutRaster = dirbase + "\\" + os.path.basename(asc)[23:-4]
	if not gp.Exists(OutRaster):
		# os.system("gdal_translate -of AIG " + asc + " " + os.path.basename(asc)[:-4])
		gp.ASCIIToRaster_conversion(asc, OutRaster, "INTEGER")
	
	# X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
	diroutlat = dirout + "\\lat_a1b_2030"
	if not os.path.exists(diroutlat):
		os.system('mkdir ' + diroutlat)
	# OutRasterCut = diroutlat + "\\" + os.path.basename(asc)[23:-4]
	# gp.clip_management(OutRaster,"-120 -33 -57 34 ",OutRasterCut)
	OutAscii = diroutlat + "\\" + os.path.basename(asc)
	gp.RasterToASCII_conversion(OutRaster, OutAscii)
	
	# gp.delete_management(OutRasterCut)

		
print "\t ..done!!"