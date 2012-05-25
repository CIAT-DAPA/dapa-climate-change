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
	print "   - ie: python Average_GCMDownscaled.py M:\climate_change\IPCC_CMIP3 B1 D:\climate_change\IPCC_CMIP3\Ensemble 30s downscaled"
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
# mask = sys.argv[6]

#Clear screen
os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + type + " " + str(resolution) 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of scenarios and models
# scenariolist = "A1B", "A2", "B1"
# periodlist = "2020_2049", "2040_2069"#, "2070_2099"#  "2010_2039", "2030_2059", "2050_2079", "2060_2089", 
period = "2020_2049"

# for scenario in scenariolist:
# modellist = "bccr_bcm2_0", "cccma_cgcm3_1_t47", "cnrm_cm3", "gfdl_cm2_0", "gfdl_cm2_1", "ingv_echam4", "inm_cm3_0", "miroc3_2_medres", "miub_echo_g", "mpi_echam5", "mri_cgcm2_3_2a", "ncar_ccsm3_0", "ukmo_hadcm3"
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))

# for scenario in scenariolist:
# for period in periodlist:
gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\bccr_bcm2_0\\2020_2049"
variablelist = gp.ListRasters("*", "GRID")

diroutMean = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Ensemble\\" + period

if not os.path.exists(diroutMean):
	os.system('mkdir ' + diroutMean)
diroutascii = diroutMean + "\\_asciis"

if not os.path.exists(diroutascii):
	os.system('mkdir ' + diroutascii)
# diroutSTD = dirout + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\Multimodel_STD\\" + period
# if not os.path.exists(diroutSTD):
	# os.system('mkdir ' + diroutSTD)

for variable in variablelist:		
	
	# if not os.path.basename(variable).split("_")[0] == "bio" or os.path.basename(variable).split("_")[0] == "tmean":
	OutRasterMean =  diroutMean + "\\" + variable
	if not gp.Exists(OutRasterMean):
	
		lista = ""
		for model in modellist:
			raster = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\" + variable
			print "   .. adding\t" + model + "\t\t" + os.path.basename(raster)
			lista = lista + ";" + raster
		LISTA = "\"" + lista[1:] + "\""		
	
		# gp.toolbox = "SA"
		# X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
		# gp.Extent = "-83 -5 -65 17"
		# gp.Extent = mask
		# gp.SnapRaster = mask
		# gp.Mask = mask
		gp.CellStatistics_sa(LISTA, OutRasterMean, "MEAN")

		# OutAscii = diroutascii + "\\" + os.path.basename(OutRasterMean) + ".asc"
		# print "    Converting " + OutAscii
		# gp.RasterToASCII_conversion(OutRasterMean, OutAscii)
		# InZip = diroutascii + "\\" + os.path.basename(OutRasterMean).split("_")[0] + "_asc.zip"
		# os.system('7za a ' + InZip + " " + OutAscii)
		# os.remove(OutAscii)
			
		# OutRasterSTD =  diroutSTD + "\\" + variable
		# if not gp.Exists(OutRasterSTD):
			# print "\n\t   ..calculing STD " + variable + "\n"
			# gp.CellStatistics_sa(LISTA, OutRasterSTD, "STD")
		# else:
			# print "\n\t   ..calculation STD " + variable + " done\n"
		print "SRES_" + scenario, os.path.basename(OutRasterMean), "done"
	else:
		print "SRES_" + scenario, os.path.basename(OutRasterMean), "done"
print "Process done!!!"    
