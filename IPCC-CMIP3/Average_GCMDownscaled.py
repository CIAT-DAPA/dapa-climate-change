# ------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: June 16th 2011
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Average_GCMDownscaled.py M:\climate_change\IPCC_CMIP3 B1 D:\climate_change\IPCC_CMIP3\ 30s downscaled"
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

#Clear screen
os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + type + " " + str(resolution) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of scenarios and models
# scenariolist = "A1B", "A2", "B1"
periodlist = "2010_2039", "2040_2069", "2070_2099"#  "2020_2049","2020_2049",  "2030_2059", "2040_2069", "2050_2079", "2060_2089", 
wildlist = "tm" , "prec"

# for scenario in scenariolist:

modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))

for period in periodlist:
	
	for wild in wildlist:
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\bccr_bcm2_0\\2020_2049"
		variablelist = gp.ListRasters(wild + "*", "GRID")
		
		diroutMean = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Multimodel_Mean\\" + period
		if not os.path.exists(diroutMean):
			os.system('mkdir ' + diroutMean)
		diroutSTD = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Multimodel_STD\\" + period
		if not os.path.exists(diroutSTD):
			os.system('mkdir ' + diroutSTD)

		for variable in variablelist:		
			lista = ""
			
			for model in modellist:
			
				raster = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\" + variable
				print "   .. adding\t" + model + "\t\t" + os.path.basename(raster)
				lista = lista + ";" + raster
			
			LISTA = "\"" + lista[1:] + "\""		
			
			OutRasterMean =  diroutMean + "\\" + variable
			if not gp.Exists(OutRasterMean):
				print "\n\t   ..averaging " + variable + ""
				gp.CellStatistics_sa(LISTA, OutRasterMean, "MEAN")
			else:
				print "\n\t   ..averaging " + variable + " done"
			
			# OutRasterSTD =  diroutSTD + "\\" + variable
			# if not gp.Exists(OutRasterSTD):
				# print "\n\t   ..calculing STD " + variable + "\n"
				# gp.CellStatistics_sa(LISTA, OutRasterSTD, "STD")
			# else:
				# print "\n\t   ..calculation STD " + variable + " done\n"
			
print "Process done!!!"    
