# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Convierte asciis a grids en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python CalculateAnnual.py D:\Workspace\MS_llanos\_extractLlanos\SRES_A2 D:\Workspace\MS_llanos\_extractLlanos\summarice_SRES_A2" 
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout =sys.argv[2]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"


modellist = sorted(os.listdir(dirbase))
periodlist = "2020_2049", "2040_2069"
for model in modellist:
	print model
	for period in periodlist:
		print period
		gp.workspace = dirbase + "\\" + model + "\\" + period
		
		OutRaster = gp.workspace + "\\tmin"
		if not gp.Exists(OutRaster):
			lista = ""	
			gridList = gp.ListDatasets("tmin*", "all")
			for grid in gridList:
				raster = gp.workspace + "\\" + os.path.basename(grid)
				print " adding.. " + grid
				lista = lista + ";" + raster
			LISTA = "\"" + lista[1:] + "\""
			
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
		OutAscii = dirout + "\\" + model + "_" + period + "_tmin.asc" 
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		
		OutRaster = gp.workspace + "\\tmax"
		if not gp.Exists(OutRaster):
			lista = ""	
			gridList = gp.ListDatasets("tmax*", "all")
			for grid in gridList:
				raster = gp.workspace + "\\" + os.path.basename(grid)
				print " adding.. " + grid
				lista = lista + ";" + raster
			LISTA = "\"" + lista[1:] + "\""
			
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
		OutAscii = dirout + "\\" + model + "_" + period + "_tmax.asc" 
		gp.RasterToASCII_conversion(OutRaster, OutAscii)

		OutRaster = gp.workspace + "\\prec"
		if not gp.Exists(OutRaster):
			lista = ""	
			gridList = gp.ListDatasets("prec*", "all")
			for grid in gridList:
				raster = gp.workspace + "\\" + os.path.basename(grid)
				print " adding.. " + grid
				lista = lista + ";" + raster
			LISTA = "\"" + lista[1:] + "\""
			
			gp.CellStatistics_sa(LISTA, OutRaster, "SUM")
		OutAscii = dirout + "\\" + model + "_" + period + "_prec.asc" 
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		
		print "tmean"
		OutRaster = gp.workspace + "\\tmean"
		print " calculating.. tmean" 
		if not gp.Exists(OutRaster):
			LISTA = "\"" + gp.workspace + "\\tmax" + ";" + gp.workspace + "\\tmin" + "\""
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
		OutAscii = dirout + "\\" + model + "_" + period + "_tmean.asc" 
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		
		
print "\t ..done!!"