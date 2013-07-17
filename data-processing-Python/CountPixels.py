# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	# python CountPixels.py F:\template D:\CIAT\Workspace\Request\mmontes 
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear screen
os.system('cls')

print "\n~~~~ COUNT PIXELS ~~~~\n"

# Create output folder
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

# Get a list of grids in the workspace
jpglist = sorted(glob.glob(dirbase + "\\*.jpg"))
gp.workspace = dirout + "\\grids"

# Lopping around the asciis
for jpg in jpglist:
	
	# Convert function
	InRaster = jpg

	# Process: RasterToOtherFormat_conversion (convert to IMG format)
	gp.RasterToOtherFormat_conversion(InRaster,gp.workspace,"GRID")
	print "\t", os.path.basename(jpg), "converted"
	
rasters = sorted(gp.ListRasters("*", "GRID"))
for raster in rasters:

	# if raster[-2] == "c":

	InExpression = 'con(' + raster + ' < 128, 0, 1)'
	OutRaster = dirout + "\\reclass\\" + raster
	print InExpression, OutRaster
	gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
	print "\t", os.path.basename(raster), "reclass"

	gp.RasterToOtherFormat_conversion(OutRaster,dirout + "\\images","PNG")
	print "\t", os.path.basename(raster), "converted image"
	
	outTable = dirout + "\\reclass\\" + raster + ".dbf"
	gp.ZonalStatisticsAsTable_sa(OutRaster, "Value", OutRaster, outTable, "DATA")
	print "\t", os.path.basename(raster), " stats calcs done"

dbflist = sorted(glob.glob(dirout + "\\reclass\\*.dbf"))
for dbf in dbflist:
	yFile = open(dirout + "\\reclass\\list.txt",'a')
	yFile.write(os.path.basename(dbf))
	yFile.write(os.path.basename(dbf))
	yFile.close()
	
gp.Merge(dbflist, dirout + "\\reclass\\merge.dbf")

print "\n \t Process done!!"