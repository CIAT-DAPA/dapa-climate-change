#------------------------------------------------------------------------------------------------------------------------------------------------------
# Description: 30yr Average grids for outputs PRECIS
# Author: 	   Carlos Navarro
# Date:  	   14/03/11
# -----------------------------------------------------------------------------------------------------------------------------------------------------

# Import system modules
import os, arcgisscripting, sys, string

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python 30yrAverage.py D:\climate_change\RCM_Data_1 SRES_A1B D:\climate_change\RCM_Data_1 ECHAM5"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
sres = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
model = sys.argv[4]
	
os.system('cls')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                    30 YR AVERAGE GRIDS PRECIS                     "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Check out licenses
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")
gp.toolbox = "management"

# List of periods and variables
periodList =  "1961", "2010", "2020", "2030", "2040", "2050", "2060", "2070"

# periodList =  "2010", "2020", "2030", "2040", "2050", "2060", "2070" #echam4

# period = "1961" #hadam3p_3 baseline
# period = "1979" #ncep_r2 baseline
# period = "2070" #hadam3p_3 sres a2

variablelist = "cloudam", "evcr", "evpotf1", "evpotf2", "evpotr", "evss", "prec", "press", "rhum", "shum", "slheat", "soilmaf", "soilmrz", "subsr", "tmax", "tmean", "tmin", "transr", "tsmean", "tsmmax", "tsmmin", "wsmean", "wsmmax"

for period in periodList:
	
	for variable in variablelist:

		gp.workspace = dirbase + "\\" + sres + "\\" + str(model) + "\\monthly_grids"
		diroutPeriod = dirout + "\\" + sres + "\\" + str(model) + "\\30yr_averages\\" + str(period) + "_" + str(int(period) + 29) 
		if not os.path.exists(diroutPeriod):
			os.system('mkdir ' + diroutPeriod)	
		
		print "Processing " + str(period) + "-" + str(int(period) + 29), model + "\n"

		for month in range(1, 12 + 1, 1):
			
			if month < 10 and not gp.Exists(diroutPeriod + "\\" + variable + "_" + str(month)):
				
				print "\t30yr Averaging " + variable + " 0" + str(month)
				
				lista = ""
				for year in range(int(period), int(period) + 29 + 1, 1):
					
					if gp.Exists(gp.workspace + "\\" + str(year) + "\\" + variable + "_0" + str(month)):
						raster = gp.workspace + "\\" + str(year) + "\\" + variable + "_0" + str(month)
						lista = lista + ";" + raster
				LISTA = "\"" + lista[1:] + "\""		

				gp.CellStatistics_sa(LISTA, diroutPeriod + "\\" + variable + "_" + str(month), "MEAN")
				
			if month > 10 and not gp.Exists(diroutPeriod + "\\" + variable + "_" + str(month)):
				
				print "\t30yr Averaging " + variable + " " + str(month)
				
				lista = ""
				for year in range(int(period), int(period) + 29 + 1, 1):
					
					if gp.Exists(gp.workspace + "\\" + str(year) + "\\" + variable + "_" + str(month)):
						raster = gp.workspace + "\\" + str(year) + "\\" + variable + "_" + str(month)
						lista = lista + ";" + raster
				LISTA = "\"" + lista[1:] + "\""		
				
				gp.CellStatistics_sa(LISTA, diroutPeriod + "\\" + variable + "_" + str(month), "MEAN")
		
		print "\nDone! \n"
		
print "Process Done!!"