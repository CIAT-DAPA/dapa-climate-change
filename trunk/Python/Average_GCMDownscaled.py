# ------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: June 16th 2011
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Average_GCMDownscaled.py L:\climate_change\IPCC_CMIP3 A2 D:\climate_change\IPCC_CMIP3\ 30s anomalies D:\Masks\COL_adm\COL_adm0.dbf"
	print "   Syntax	: <code.py>, <dirbase>, <scenario>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirout	: Out folder of txt describe archive"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: disaggregated, interpolated or downscaled"	
	sys.exit(1)


dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]
resolution = sys.argv[4]
type = sys.argv[5]
mask = sys.argv[6]

#Clear screen
os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + type + " " + str(resolution) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of scenarios and models
# scenariolist = "A1B", "A2", "B1"
periodlist = "2020_2049", "2040_2069"#  "2020_2049",  "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2010_2039", 

# for scenario in scenariolist:

modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type))

for period in periodlist:
	
	for model in modellist:
		print model, period
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period
		
			
		diroutMean = dirout + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period
		if not os.path.exists(diroutMean):
			os.system('mkdir ' + diroutMean)
		# diroutSTD = dirout + "\\SRES_" + scenario + "\\" + type + "\\Multimodel_STD\\" + period
		# if not os.path.exists(diroutSTD):
			# os.system('mkdir ' + diroutSTD)
		lista = ""	
		
		
		
		variablelist = gp.ListRasters("tmea*", "GRID")		
		for variable in variablelist:		
			

			raster = dirbase + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period + "\\" + variable
			print " adding.. " + raster
			lista = lista + ";" + raster
		LISTA = "\"" + lista[1:] + "\""
		print LISTA
		print "averaging \n"
		OutRasterMean = diroutMean + "\\tmean"
		if not gp.Exists(OutRasterMean):
			gp.CellStatistics_sa(LISTA, OutRasterMean, "MEAN")
		print "done!"

		diroutcut = diroutMean + "\\_cut" 
		if not os.path.exists(diroutcut):
			os.system('mkdir ' + diroutcut)				
		OutRasterMeanCut = diroutcut + "\\tmean"
		
		if not gp.Exists(OutRasterMeanCut):
			gp.ExtractByMask_sa(OutRasterMean, mask, OutRasterMeanCut)
			# gp.delete_management(OutRasterMean)
			print "\n\t   ..average " + variable + " done"
		else:
			print "\n\t   ..average " + variable + " done"
			
		lista = ""	
		variablelist = gp.ListRasters("prec*", "GRID")		
		for variable in variablelist:		
			

			raster = dirbase + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period + "\\" + variable
			lista = lista + ";" + raster
		LISTA = "\"" + lista[1:] + "\""
		print LISTA
		print "summing \n"
		OutRasterMean = diroutMean + "\\prec"
		if not gp.Exists(OutRasterMean):
			gp.CellStatistics_sa(LISTA, OutRasterMean, "SUM")
		print "done!"

		diroutcut = diroutMean + "\\_cut" 
		if not os.path.exists(diroutcut):
			os.system('mkdir ' + diroutcut)				
		OutRasterMeanCut = diroutcut + "\\prec"
		
		if not gp.Exists(OutRasterMeanCut):
			gp.ExtractByMask_sa(OutRasterMean, mask, OutRasterMeanCut)
			# gp.delete_management(OutRasterMean)
			print "\n\t   ..sum " + variable + " done"
		else:
			print "\n\t   ..sum " + variable + " done"
			# OutRasterSTD =  diroutSTD + "\\" + variable
			# if not gp.Exists(OutRasterSTD):
				# print "\n\t   ..calculing STD " + variable + "\n"
				# gp.CellStatistics_sa(LISTA, OutRasterSTD, "STD")
			# else:
				# print "\n\t   ..calculation STD " + variable + " done\n"


for period in periodlist:
	
	lista = ""	
	for model in modellist:
		raster = dirout + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period + "\\_cut\\tmean"
		print " adding.. " + raster
		lista = lista + ";" + raster
	LISTA = "\"" + lista[1:] + "\""
	print LISTA
	print "averaging \n"

	diroutSta = dirout + "\\SRES_" + scenario + "\\" + type

	gp.CellStatistics_sa(LISTA, diroutSta + "\\mean", "MEAN")
	gp.CellStatistics_sa(LISTA, diroutSta + "\\range", "RANGE")
	gp.CellStatistics_sa(LISTA, diroutSta + "\\std", "STD")
	InExpression = diroutSta + "\\std" + " / " + diroutSta + "\\mean"
	gp.SingleOutputMapAlgebra_sa(InExpression, diroutSta + "\\cv")

	print "done!"

	
	
	
	

print "Process done!!!"    
