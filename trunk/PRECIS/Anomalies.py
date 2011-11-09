#------------------------------------------------------------------------------------------------------------------------------------------------------
# Description:	Anomalies PRECIS
# Author: 	  	Carlos Navarro
# Date:  	  	02/08/11
# Actualized:	08/11/11
# -----------------------------------------------------------------------------------------------------------------------------------------------------

# Import system modules
import os, arcgisscripting, sys, string

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python Anomalies.py D:\climate_change\RCM_Data\Baseline D:\climate_change\RCM_Data\SRES_A1B"
	sys.exit(1)

# Arguments
dirbaseline = sys.argv[1]
dirbase = sys.argv[2]
os.system('cls')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                    	 ANOMALIES GRIDS PRECIS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Check out licenses
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
gp.OverWriteOutput = 1
# variableList = "tmean1_5", "prec"

# modellist = sorted(os.listdir(dirbase))
# for model in modellist:
	# periodlist = sorted(os.listdir(dirbase + "\\" + model + "\\30yrAverages\\"))
	# dirInRaster = dirbaseline + "\\" + str(model) + "\\30yrAverages\\1961_1990"
	
	# for period in periodlist:
	
		# gp.workspace = dirbase + "\\" + str(model) + "\\30yrAverages\\" + str(period)

		# print "\t > Processing " + model,str(period)
		# diroutPeriod = dirbase + "\\" + str(model) + "\\anomalies\\" + str(period)
		# if not os.path.exists(diroutPeriod):
			# os.system('mkdir ' + diroutPeriod)	
			
		# for variable in variableList:
			# for month in range(1, 12 + 1, 1):
				
				# if month < 10 and not gp.Exists(diroutPeriod + "\\" + str(variable) + "_0" + str(month)):
					# InRaster = dirInRaster + "\\" + str(variable) + "_0" + str(month)
					# FutRaster = gp.workspace + "\\" + str(variable) + "_0" + str(month)
					# OutRaster = diroutPeriod + "\\" + str(variable) + "_0" + str(month)
					
					# if not gp.Exists(OutRaster):
						# print "\t\t " + os.path.basename(OutRaster)
						# InExpression = FutRaster + " - " + InRaster
						# gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
					
				# if month > 9 and not gp.Exists(diroutPeriod + "\\" + str(variable) + "_" + str(month)):
					# InRaster = dirInRaster + "\\" + str(variable) + "_" + str(month)
					# FutRaster = gp.workspace + "\\" + str(variable) + "_" + str(month)
					# OutRaster = diroutPeriod + "\\" + str(variable) + "_" + str(month)
				
					# if not gp.Exists(OutRaster):
						# print "\t\t " + os.path.basename(OutRaster)
						# InExpression = FutRaster + " - " + InRaster
						# gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)


modellist = sorted(os.listdir(dirbase))
for model in modellist:
	periodlist = sorted(os.listdir(dirbase + "\\" + model + "\\30yrAverages\\"))
	for period in periodlist:
		
		print "\t > Processing " + model,str(period)
		gp.workspace = dirbase + "\\" + str(model) + "\\anomalies\\" + str(period)
		
		lista = ""
		variableList = gp.ListRasters("tmean*", "GRID")	
		for variable in variableList:
			raster = gp.workspace + "\\" + os.path.basename(variable)
			print " adding.. " + raster
			lista = lista + ";" + raster
		
		LISTA = "\"" + lista[1:] + "\""
		
		OutRasterAv = gp.workspace + "\\av_tmean"
		OutRasterSd = gp.workspace + "\\sd_tmean"
		if not gp.Exists(OutRasterAv):
			print OutRasterAv
			gp.CellStatistics_sa(LISTA, OutRasterAv, "MEAN")
		if not gp.Exists(OutRasterSd):
			print OutRasterSd
			gp.CellStatistics_sa(LISTA, OutRasterSd, "STD")

		diroutraster = "D:\Workspace\A_way_forward_on_adaptation_to_CC\P3" + "\\" + model
		if not os.path.exists(diroutraster):
			os.system('mkdir ' + diroutraster)	
		
		gp.ExtractByMask_sa(OutRasterAv, "D:\Masks\COL_adm\COL.shp", diroutraster + "\\" + os.path.basename(OutRasterAv))
		gp.ExtractByMask_sa(OutRasterSd, "D:\Masks\COL_adm\COL.shp", diroutraster + "\\" + os.path.basename(OutRasterSd))
			
		lista = ""
		variableList = gp.ListRasters("prec*", "GRID")	
		for variable in variableList:
			raster = gp.workspace + "\\" + os.path.basename(variable)
			print " adding.. " + raster
			lista = lista + ";" + raster
		
		LISTA = "\"" + lista[1:] + "\""
		
		OutRasterAv = gp.workspace + "\\av_prec"
		OutRasterSd = gp.workspace + "\\sd_prec"
		if not gp.Exists(OutRasterAv):
			print OutRasterAv
			gp.CellStatistics_sa(LISTA, OutRasterAv, "MEAN")
		if not gp.Exists(OutRasterSd):
			print OutRasterSd
			gp.CellStatistics_sa(LISTA, OutRasterSd, "STD")
			
		gp.ExtractByMask_sa(OutRasterAv, "D:\Masks\COL_adm\COL.shp", diroutraster + "\\" + os.path.basename(OutRasterAv))
		gp.ExtractByMask_sa(OutRasterSd, "D:\Masks\COL_adm\COL.shp", diroutraster + "\\" + os.path.basename(OutRasterSd))

print "\t > Process Done!!"