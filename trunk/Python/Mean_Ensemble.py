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
	print "   - ie: python Mean_Ensemble.py D:\EcoCrop-development\climate"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~"
print "   MEAN ENSEMBLE   "
print "~~~~~~~~~~~~~~~~"
print "\n"

varlist = "bio_1", "bio_12" #, "tmax", "tmin", "bio"

modellist = sorted(os.listdir(dirbase + "\\africa_10min_future_gcm"))
for model in modellist:
	print model
	print model
	for var in varlist:
		# for month in range (1, 12 + 1, 1):
		futureraster = dirbase + "\\africa_10min_future_gcm\\" + model + "\\2020_2049\\" + var
		currentraster = dirbase + "\\africa_10min\\" + var
		InExpression = (futureraster + " - " + currentraster)
		anomaliesfolder = dirbase + "\\africa_10min_anomalies\\" + model
		if not os.path.exists(anomaliesfolder):
			os.system('mkdir ' + anomaliesfolder)
		outraster = anomaliesfolder + "\\" + var
		
		# print "Calculating anomalies for " + var
		if not gp.Exists(outraster):
			gp.SingleOutputMapAlgebra_sa(InExpression, outraster)

		outTable1 = anomaliesfolder + "\\" + var + ".dbf"
		
		# print "calculating stats for " + os.path.basename(outTable1)
		
		if not gp.Exists(outTable1):
			gp.ZonalStatisticsAsTable_sa("D:\\EcoCrop-development\\analysis-mask\\starea-countries.shp", "FID", outraster, outTable1, "DATA")
		print model
		if var == "bio_1":
			gp.joinfield ("D:\\EcoCrop-development\\climate\\africa_10min_stat\\summary_tmean.dbf", "FID_", outTable1, "FID_", "MEAN" )
		else:
			gp.joinfield ("D:\\EcoCrop-development\\climate\\africa_10min_stat\\summary_prec.dbf", "FID_", outTable1, "FID_", "MEAN" )
	
for var in varlist:
	# for month in range (1, 12 + 1, 1):
	for model in modellist:
	
		gp.workspace = dirbase + "\\africa_10min_anomalies\\" + model
		lista = ""
		raster = gp.workspace + "\\" + var 
		# print "\t  " + os.path.basename(raster)
		lista = lista + ";" + raster

	LISTA = "\"" + lista[1:] + "\""		
	dirstat = dirbase + "\\africa_10min_stat"
	if not os.path.exists(dirstat):
		os.system('mkdir ' + dirstat)
	outmean = dirstat + "\\" + var + "_m"
	outsd = dirstat + "\\" + var + "_sd"
	if not gp.Exists(outmean):
		gp.CellStatistics_sa(LISTA, outmean, "MEAN")
	if not gp.Exists(outsd):
		gp.CellStatistics_sa(LISTA, outsd, "STD")

	outTable1 = dirstat + "\\" + var + "_m" + ".dbf"
	outTable2 = dirstat + "\\" + var + "_sd" + ".dbf"
	
	# print "calculating stats for " + os.path.basename(outTable1)
	
	# if not gp.Exists(outTable1):
		# gp.ZonalStatisticsAsTable_sa("D:\\EcoCrop-development\\analysis-mask\\starea-countries.shp", "FID", outmean, outTable1, "DATA")
	# if not gp.Exists(outTable2):
		# gp.ZonalStatisticsAsTable_sa("D:\\EcoCrop-development\\analysis-mask\\starea-countries.shp", "FID", outsd, outTable2, "DATA")
			
	
	
		
# var = "bio"
# # for var in varlist:

	# for folder in folderlist:
		# gp.workspace = dirbase + "\\" + folder + "\\2020_2049"


		
	# print "\n\t   ..averaging "
	
	# OutRaster = "D:\\EcoCrop-development\\climate\\africa_10min_ensemble\\" + var + "_" + str(month)
	# gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
	
	# OutAscii = "D:\\EcoCrop-development\\climate\\africa_10min_ensemble\\_asciis\\" + var + "_" + str(month) + ".asc"
	# diroutAscii = "D:\\EcoCrop-development\\climate\\africa_10min_ensemble\\_asciis"
	# if not os.path.exists(diroutAscii):
		# os.system('mkdir ' + diroutAscii)
	# print "    Converting " + OutAscii
	# gp.RasterToASCII_conversion(OutRaster, OutAscii)
			
	# print "\t ..done!!"

# print "Done!!!!"
