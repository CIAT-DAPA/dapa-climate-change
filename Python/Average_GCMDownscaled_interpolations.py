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
	print "   - ie: python Average_GCMDownscaled_interpolations.py L:\climate_change\IPCC_CMIP3 A2 D:\climate_change\IPCC_CMIP3\ 30s interpolations D:\Masks\COL_adm\COL_adm0.dbf"
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
variablelist = "tmin", "tmax", "prec"
# for scenario in scenariolist:

modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type))

for period in periodlist:
	
	for model in modellist:
		print model, period
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period
	
		diroutMean = dirout + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period
		if not os.path.exists(diroutMean):
			os.system('mkdir ' + diroutMean)
		
		diroutcut = diroutMean + "\\_cut" 
		if not os.path.exists(diroutcut):
			os.system('mkdir ' + diroutcut)	
			
		for variable in variablelist:		

			lista = ""	
			OutRasterMean = diroutMean + "\\" + variable
			rsList = gp.ListRasters(variable + "*", "GRID")		
			for rs in rsList:		
				raster = dirbase + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period + "\\" + rs
				print " adding.. " + os.path.basename(raster)
				lista = lista + ";" + raster
			LISTA = "\"" + lista[1:] + "\""
			print "\naveraging.."
			
			OutRasterMeanCut = diroutcut + "\\" + variable
			if not gp.Exists(OutRasterMeanCut):
				if not gp.Exists(OutRasterMean):
					if variable == "tmin" or variable == "tmax":
						gp.CellStatistics_sa(LISTA, OutRasterMean, "MEAN")
					else:
						gp.CellStatistics_sa(LISTA, OutRasterMean, "SUM")
				print "average done!"

				print "cutting.."
				gp.ExtractByMask_sa(OutRasterMean, mask, OutRasterMeanCut)
				gp.delete_management(OutRasterMean)
				print "\n cutting " + variable + " done"
	
		LISTA2 = '"' + diroutcut + "\\tmax;" + diroutcut + '\\tmin"'
		print LISTA2
		if not gp.Exists(diroutcut + "\\tmean"):
			gp.CellStatistics_sa(LISTA2, diroutcut + "\\tmean", "MEAN")
		

# for period in periodlist:
	
	# lista = ""	
	# for model in modellist:
		# raster = dirout + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period + "\\_cut\\tmean"
		# print " adding.. " + raster
		# lista = lista + ";" + raster
	# LISTA = "\"" + lista[1:] + "\""
	# print LISTA
	# print "averaging \n"

	# diroutSta = dirout + "\\SRES_" + scenario + "\\" + type

	# gp.CellStatistics_sa(LISTA, diroutSta + "\\mean_t", "MEAN")
	# gp.CellStatistics_sa(LISTA, diroutSta + "\\range_t", "RANGE")
	# gp.CellStatistics_sa(LISTA, diroutSta + "\\std_t", "STD")
	# InExpression = diroutSta + "\\std_t" + " / " + diroutSta + "\\mean_t"
	# gp.SingleOutputMapAlgebra_sa(InExpression, diroutSta + "\\cv_t")

	# print "done!"

# for period in periodlist:
	
	# lista = ""	
	# for model in modellist:
		# raster = dirout + "\\SRES_" + scenario + "\\" + type + "\\" + model + "\\" + period + "\\_cut\\prec"
		# print " adding.. " + raster
		# lista = lista + ";" + raster
	# LISTA = "\"" + lista[1:] + "\""
	# print LISTA
	# print "averaging \n"

	# diroutSta = dirout + "\\SRES_" + scenario + "\\" + type

	# gp.CellStatistics_sa(LISTA, diroutSta + "\\mean_p", "MEAN")
	# gp.CellStatistics_sa(LISTA, diroutSta + "\\range_p", "RANGE")
	# gp.CellStatistics_sa(LISTA, diroutSta + "\\std_p", "STD")
	# InExpression = diroutSta + "\\std_p" + " / " + diroutSta + "\\mean_p"
	# gp.SingleOutputMapAlgebra_sa(InExpression, diroutSta + "\\cv_p")

	# print "done!"
	
	

print "Process done!!!"    
