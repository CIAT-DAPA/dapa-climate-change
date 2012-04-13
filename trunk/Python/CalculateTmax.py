# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Convierte asciis a grids en un workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python CalculateTmax.py K:\MRIData\MRI_grids\SP0A"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

gp.workspace = dirbase 
# gridList = gp.ListDatasets("tmean*", "all")

for month in range(1, 8 + 1, 1):
# for grid in gridList:
	print "tmean_" + str(month)
	InExpression = gp.workspace + "\\tmean\\OUT_199002010000\\tmean_0" + str(month) + " - " + gp.workspace + "\\tmin\\OUT_199002010000\\tmin_0" + str(month)
	
	TmpRaster = "D:/Workspace/tmp/dtr_0" + str(month)
	if not gp.Exists(TmpRaster):
		gp.SingleOutputMapAlgebra_sa(InExpression, TmpRaster)
	# gp.Times_sa(gp.workspace + "\\dtr_" + str(month), 0.5, TmpRaster)
	
	InExpression2 = gp.workspace + "\\tmin\\OUT_199002010000\\tmin_0" + str(month) + " + " + "D:/Workspace/tmp/dtr_0" + str(month) + " + " + "D:/Workspace/tmp/dtr_0" + str(month)
	FinalRaster = gp.workspace + "\\tmax\\OUT_199002010000\\tmax_0" + str(month)
	if not gp.Exists(FinalRaster):
		print FinalRaster
		gp.SingleOutputMapAlgebra_sa(InExpression2, FinalRaster)

	# InExpression3 = gp.workspace + "\\tmean_" + str(month) + " - " + TmpRaster
	# FinalRaster1 = gp.workspace + "\\tmin_" + str(month)
	# print FinalRaster1
	# gp.SingleOutputMapAlgebra_sa(InExpression3, FinalRaster1)



	gp.delete_management(TmpRaster)
	# # OutRaster = OutDir + "\\" + var + "_" + str(month) + "_1"
	# OutAscii = dirbase + "\\a1b_2020_2049_ensemble_" + os.path.basename(grid) + ".asc"
	# print "\t calculating ensmeble" 
	# if not gp.Exists(OutAscii):
		# # gp.CellStatistics_sa(LIST, OutRaster, "MEAN")
		# gp.RasterToASCII_conversion(grid, OutAscii)
		# # gp.delete_management(OutRaster)
print "\t ..done!!"