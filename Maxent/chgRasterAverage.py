# ---------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Converts Maxent Asciis Outputs to ESRI Grid, calculate the change raster and calculate the average and std 
# ---------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python chgRasterAverage.py D:\Maxent_Nicaragua\mxe_outputs\sp-coffea_arabica_1\projections D:\Maxent_Nicaragua\mxe_outputs\sp-coffea_arabica_1\projections\changes D:\Maxent_Nicaragua\mxe_outputs1\sp-coffea_arabica_1\projections\summarize coffea_arabica 25"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
diroutavg = sys.argv[3]
specie = sys.argv[4]
folds = sys.argv[5]

th = "0.49458418636"
avgTh0 = "D:\Maxent_Nicaragua\mxe_outputs1\sp-coffea_arabica\crossval\ca_avg_th0"

gp.CheckOutExtension("Spatial")
os.system('cls')

# Lopping around the asciis
modellist = "bccr_bcm2_0","cccma_cgcm3_1_t47","cnrm_cm3","csiro_mk3_0","csiro_mk3_5","gfdl_cm2_0","gfdl_cm2_1","giss_model_er","ingv_echam4","inm_cm3_0","ipsl_cm4","miroc3_2_medres","miub_echo_g","mpi_echam5","mri_cgcm2_3_2a","ncar_ccsm3_0","ncar_pcm1","ukmo_hadcm3","ukmo_hadgem1"

listChgRaster = ""
listPrjRaster = ""

for model in modellist:
	
	diroutGrid = dirout + "\\" + model
	if not os.path.exists(diroutGrid):
		os.system('mkdir ' + diroutGrid)
	
	for fold in range(1, int(folds) +1, 1): 
		
		asc = dirbase + "\\" + specie + "_" + model + "_f" + str(fold) + ".asc"
		print model
		
		if not gp.Exists(diroutGrid + "\\f_" + str(fold)):
			
			# ArcGIS option
			gp.ASCIIToRaster_conversion(asc, diroutGrid + "\\f_" + str(fold), "FLOAT")
			

		prjRaster = diroutGrid + "\\f_" + str(fold)
		prjRasterTh0 = diroutGrid + "\\f_th0_" + str(fold)
		chgRasterTh0 = diroutGrid + "\\chg_th0_" + str(fold)
		prjRasterThNa = diroutGrid + "\\f_th_na_" + str(fold)
		
		
		print prjRasterTh0
		if not gp.Exists(prjRasterTh0):

			InExpression = "con(" + prjRaster + " < " + th + ", 0, " + prjRaster + ")"
			gp.SingleOutputMapAlgebra_sa(InExpression, prjRasterTh0)
		
		print chgRasterTh0
		if not gp.Exists(chgRasterTh0):

			InExpression = prjRasterTh0 + " - " + avgTh0
			gp.SingleOutputMapAlgebra_sa(InExpression, chgRasterTh0)
		
		print prjRasterThNa
		if not gp.Exists(prjRasterThNa):
			InExpression = "setnull(" + prjRaster + " < " + th + "," + prjRaster + ")"
			gp.SingleOutputMapAlgebra_sa(InExpression, prjRasterThNa)
			
		listPrjRaster = listPrjRaster + ';' + prjRasterThNa 
		listChgRaster = listChgRaster + ';' + chgRasterTh0 
				

StackChg = "\"" + listChgRaster[1:] + "\""
StackPrj = "\"" + listPrjRaster[1:] + "\""
# Process: Cell Statistics...

print "Calculating and writing mean probability change raster \n"
avgChgRaster = diroutavg + "\\" + specie[:5] + "_ENM_chg"
if not gp.Exists(avgChgRaster):
	avgChgRasterAsc = diroutavg + "\\" + specie + "_ENM_change.asc"
	gp.CellStatistics_sa(StackChg, avgChgRaster, "MEAN")
	gp.RasterToASCII_conversion(avgChgRaster, avgChgRasterAsc)
	print "\t ..done!!"

print "Calculating and writing std probability change raster \n"
avgChgRasterStd = diroutavg + "\\" + specie[:5] + "_ESD_chg"
if not gp.Exists(avgChgRasterStd):
	avgChgRasterStdAsc = diroutavg + "\\" + specie + "_ESD_change.asc"
	gp.CellStatistics_sa(StackChg, avgChgRasterStd, "STD")
	gp.RasterToASCII_conversion(avgChgRasterStd, avgChgRasterStdAsc)
	print "\t ..done!!"

print "Calculating and writing mean probability change raster \n"
avgPrjRaster = diroutavg + "\\" + specie[:5] + "_ENM_prj"
if not gp.Exists(avgPrjRaster):
	avgPrjRasterAsc = diroutavg + "\\" + specie + "_ENM_projections.asc"
	gp.CellStatistics_sa(StackPrj, avgPrjRaster, "MEAN")
	gp.RasterToASCII_conversion(avgPrjRaster, avgPrjRasterAsc)
	print "\t ..done!!"

print "Calculating and writing mean probability change raster \n"
avgPrjRasterStd = diroutavg + "\\" + specie[:5] + "_ESD_prj"
if not gp.Exists(avgPrjRasterStd):
	avgPrjRasterStdAsc = diroutavg + "\\" + specie + "_ESD_projections.asc"
	gp.CellStatistics_sa(StackPrj, avgPrjRasterStd, "STD")
	gp.RasterToASCII_conversion(avgPrjRasterStd, avgPrjRasterStdAsc)
	print "\t ..done!!"

listDirRaster = ""

for model in modellist:
	
	diroutGrid = dirout + "\\" + model
	for fold in range(1, int(folds) +1, 1): 
		
		# asc = dirbase + "\\" + specie + "_" + model + "_f" + str(fold) + ".asc"
		print model
	
		dirChgRaster = diroutGrid + "\\dir_chg_" + str(fold)
	
		print dirChgRaster
		if not gp.Exists(dirChgRaster):
			InExpression = "con((" + chgRasterTh0 + " * " + avgChgRaster + " > 0), 1, 0)"
			gp.SingleOutputMapAlgebra_sa(InExpression, dirChgRaster)
		
		listDirRaster = listDirRaster + ';' + dirChgRaster 

StackChg = "\"" + listDirRaster[1:] + "\""
print "Calculating and writing dir probability change raster \n"
dirChgRaster = diroutavg + "\\" + specie[:5] + "_dir_chg"
if not gp.Exists(dirChgRaster):
	avgDirRasterAsc = diroutavg + "\\" + specie + "_direction_change.asc"
	gp.CellStatistics_sa(StackChg, dirChgRaster, "SUM")
	gp.RasterToASCII_conversion(dirChgRaster, avgDirRasterAsc)
	print "\t ..done!!"

print "\nProcess done!!!"