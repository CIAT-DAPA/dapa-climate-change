#----------------------------------------------
# Description: Average models
# Author: 	   Carlos Navarro
# Date:  	   08/11/11
# ---------------------------------------------

# Import system modules
import os, arcgisscripting, sys, string

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python AverageModels.py D:\climate_change\RCM_Data\SRES_A1B D:\climate_change\RCM_Data\Average_Models"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

os.system('cls')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                    	  AVERAGE GRIDS PRECIS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Check out licenses
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")
gp.toolbox = "management"

# List of periods and variables
# fluxList = ["Prec", "EvSS", "EvCR", "SubSR", "TransR", "EvPotF1", "EvPotF2", "SoilMRZ"]
# kelvinList = ["TSmean", "TSmmax", "TSmmin", "Tmean1_5", "Tmmax1_5", "Tmmin1_5"]
# variableList = ["CloudAm", "EvPotR", "Press", "RHum1_5", "SHum1_5", "SLHeat", "SoilMAF", "Wsmean", "Wsmmax"]
# variableList = "tmean1_5", "prec"
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"

gp.workspace = dirbase + "\\ECHAM5\\anomalies\\2010_2039"
variablelist = gp.ListRasters("av_t*", "GRID")		
for variable in variablelist:	
	# periodlist = sorted(os.listdir(dirbase + "\\" + model + "\\30yrAverages\\"))
	for period in periodlist:
		modellist = sorted(os.listdir(dirbase))
		lista = ""	
		for model in modellist:

			diroutraster = dirout + "\\" + str(period)
			if not os.path.exists(diroutraster):
				os.system('mkdir ' + diroutraster)		

			raster = dirbase + "\\" + model + "\\anomalies\\" + str(period) + "\\" + os.path.basename(variable)
			print " adding.. " + raster
			lista = lista + ";" + raster
		
		LISTA = "\"" + lista[1:] + "\""

		print "\n averaging \n"
		OutRasterAv = diroutraster + "\\ann_tmean_av"
		OutRasterSd = diroutraster + "\\ann_tmean_std"
		if not gp.Exists(OutRasterAv):
			print OutRasterAv
			gp.CellStatistics_sa(LISTA, OutRasterAv, "MEAN")
		if not gp.Exists(OutRasterSd):
			print OutRasterSd
			gp.CellStatistics_sa(LISTA, OutRasterSd, "STD")
		print "done!"
				
print "\t > Process Done!!"