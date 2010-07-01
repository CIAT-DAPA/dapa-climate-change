# ---------------------------------------------------------------------------
# Autor: Edward Dario Guevara Valencia
# Fecha: Junio 30 2010
# Proposito: Describe las propiedades de ASCIIs en un WorkSpace
# ADVERTENCIA. Es necesario tener instalado FWTools y tenerlo en el PATH
# ---------------------------------------------------------------------------

import arcgisscripting, subprocess

gp = arcgisscripting.create(9.3)

subprocess.call("cls", shell=True)

print "\n  Proposito: Describe las propiedades de ASCIIs en un WorkSpace"
print "\n  ADVERTENCIA. Es necesario tener instalado FWTools y tenerlo en el PATH"

gp.Workspace = raw_input("\n. Digite el path de los ASCIIs: ")

rasters = gp.ListRasters("*.asc", "ALL")

for raster in rasters:
	OutRaster = (raster.split('.'))[0]

	print "\n--> Processing " + raster

	# Process: ASC2TIF
	if not gp.exists(OutRaster + ".TIF"):
		subprocess.call("gdal_translate -ot Int16 -of GTiff " + gp.Workspace + "\\" + raster +
							 " " + gp.Workspace + "\\" + OutRaster + ".TIF", shell=True)
	
	MIN = gp.GetRasterProperties_management(OutRaster + ".TIF", "MINIMUM")
	MAX = gp.GetRasterProperties_management(OutRaster + ".TIF", "MAXIMUM")
	MEA = gp.GetRasterProperties_management(OutRaster + ".TIF", "MEAN")
	STD = gp.GetRasterProperties_management(OutRaster + ".TIF", "STD")
##	UNI = gp.GetRasterProperties_management(OutRaster + ".TIF", "UNIQUEVALUECOUNT")
	TOP = gp.GetRasterProperties_management(OutRaster + ".TIF", "TOP")
	LEF = gp.GetRasterProperties_management(OutRaster + ".TIF", "LEFT")
	RIG = gp.GetRasterProperties_management(OutRaster + ".TIF", "RIGHT")
	BOT = gp.GetRasterProperties_management(OutRaster + ".TIF", "BOTTOM")
	CEX = gp.GetRasterProperties_management(OutRaster + ".TIF", "CELLSIZEX")
	CEY = gp.GetRasterProperties_management(OutRaster + ".TIF", "CELLSIZEY")
	VAL = gp.GetRasterProperties_management(OutRaster + ".TIF", "VALUETYPE")
	COL = gp.GetRasterProperties_management(OutRaster + ".TIF", "COLUMNCOUNT")
	ROW = gp.GetRasterProperties_management(OutRaster + ".TIF", "ROWCOUNT")
##	BAN = gp.GetRasterProperties_management(OutRaster + ".TIF", "BANDCOUNTUSER")
	
	print "    MINIMUM: " + MIN.getoutput(0)
	print "    MAXIMUM: " + MAX.getoutput(0)
	print "    MEAN: " + MEA.getoutput(0)
	print "    STD: " + STD.getoutput(0)
##	print "    UNIQUEVALUECOUNT: " + UNI.getoutput(0)
	print "    TOP: " + TOP.getoutput(0)
	print "    LEFT: " + LEF.getoutput(0)
	print "    RIGHT: " + RIG.getoutput(0)
	print "    BOTTOM: " + BOT.getoutput(0)
	print "    CELLSIZEX: " + CEX.getoutput(0)
	print "    CELLSIZEY: " + CEY.getoutput(0)
	print "    VALUETYPE: " + VAL.getoutput(0)
	print "    COLUMNCOUNT: " + COL.getoutput(0)
	print "    ROWCOUNT: " + ROW.getoutput(0)
##	print "    BANDCOUNTUSER: " + BAN.getoutput(0)

	if gp.exists(OutRaster):
		gp.delete(OutRaster)
