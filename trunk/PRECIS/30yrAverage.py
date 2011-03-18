#------------------------------------------------------------------------------------------------------------------------------------------------------
# Description: Average grids for outputs PRECIS
# Author: 	   Carlos Navarro
# Date:  	   14/03/11
# Notes: 	   
# -----------------------------------------------------------------------------------------------------------------------------------------------------

# Import system modules
import os, arcgisscripting, sys, string

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python 30yrAverage.py L:\climate_change\RCM_Data\SRES_A1B\HadCM3Q0\monthly_grids G:\climate_change\RCM_Data\SRES_A1B\HadCM3Q0\30yrAverages_temp G:\climate_change\RCM_Data\SRES_A1B\HadCM3Q0\30yrAverages"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirtemp = sys.argv[2]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

os.system('cls')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                    	   AVERAGE GRIDS PRECIS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Check out licenses
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")
gp.toolbox = "management"

# List of periods and variables
periodList = "1961", "2010", "2020", "2030", "2040", "2050", "2060", "2070"
fluxList = ["Prec", "EvSS", "EvCR", "SubSR", "TransR", "EvPotF1", "EvPotF2", "SoilMRZ"]
kelvinList = ["TSmean", "TSmmax", "TSmmin", "Tmean1_5", "Tmmax1_5", "Tmmin1_5"]
variableList = ["CloudAm", "EvPotR", "Press", "RHum1_5", "SHum1_5", "SLHeat", "SoilMAF", "Wsmean", "Wsmmax"]

for period in periodList:
	
	print "\t > Processing " + str(period) + "_" + str(int(period) + 29) + "\n"
	diroutPeriod = dirout + "\\" + str(period) + "_" + str(int(period) + 29) 
	if not os.path.exists(diroutPeriod):
		os.system('mkdir ' + diroutPeriod)	
	
	# Variables to convert flux to mm/dia
	for flux in fluxList:
		for month in range(1, 12 + 1, 1):
			if month < 10 and not gp.Exists(diroutPeriod + "\\" + str(flux) + "_0" + str(month)):
				lista = ""
				print "\t   Processing " + str(flux) + "_0" + str(month) + "\n"
				print "\t   ..listing grids "
				for year in range(int(period), int(period) + 29 + 1, 1):
					if gp.Exists(dirbase + "\\" + str(year) + "\\" + flux + "\\" + str(flux) + "_0" + str(month)):
						
						raster = dirbase + "\\" + str(year) + "\\" + str(flux) + "\\" + str(flux) + "_0" + str(month)
						print "\t  " + str(year) + " " + os.path.basename(raster)
						lista = lista + ";" + raster
			
				LISTA = "\"" + lista[1:] + "\""		
				
				print "\n\t   ..averaging " + str(flux) + "_0" + str(month) + "\n"
				TmpRaster = dirtemp + "\\" + str(flux) + "_0" + str(month)
				gp.CellStatistics_sa(LISTA, TmpRaster, "MEAN")
				
				OutRaster = diroutPeriod + "\\" + str(flux) + "_0" + str(month)
				gp.Times_sa(TmpRaster, "86400", OutRaster)
				gp.delete_management(TmpRaster)
				print "\n\t   ..done! \n"
				
			if month > 9 and not gp.Exists(diroutPeriod + "\\" + str(flux) + "_" + str(month)): 			
				lista = ""
				print "\t   Processing " + str(flux) + "_" + str(month) + "\n"
				print "\t   ..listing grids "
				for year in range(int(period), int(period) + 29 + 1, 1):
					if gp.Exists(dirbase + "\\" + str(year) + "\\" + str(flux) + "\\" + str(flux) + "_" + str(month)):
						
						raster = dirbase + "\\" + str(year) + "\\" + str(flux) + "\\" + str(flux) + "_" + str(month)
						print "\t  " + str(year) + " " + os.path.basename(raster)
						lista = lista + ";" + raster
			
				LISTA = "\"" + lista[1:] + "\""		
				
				print "\n\t   ..averaging " + str(flux) + "_" + str(month) + "\n"
				TmpRaster = dirtemp + "\\" + str(flux) + "_" + str(month)
				gp.CellStatistics_sa(LISTA, TmpRaster, "MEAN")
				
				OutRaster = diroutPeriod + "\\" + str(flux) + "_" + str(month)
				gp.Times_sa(TmpRaster, "86400", OutRaster)
				gp.delete_management(TmpRaster)
				print "\n\t   ..done! \n"
				
	# Variables to convert kelvin to celcius
	for kelvin in kelvinList:
		for month in range(1, 12 + 1, 1):
			if month < 10 and not gp.Exists(diroutPeriod + "\\" + str(kelvin) + "_0" + str(month)):
				lista = ""
				print "\t   Processing " + str(kelvin) + "_0" + str(month) + "\n"
				print "\t   ..listing grids "
				for year in range(int(period), int(period) + 29 + 1, 1):
					if gp.Exists(dirbase + "\\" + str(year) + "\\" + str(kelvin) + "\\" + str(kelvin) + "_0" + str(month)):
						
						raster = dirbase + "\\" + str(year) + "\\" + str(kelvin) + "\\" + str(kelvin) + "_0" + str(month)
						print "\t  " + str(year) + " " + os.path.basename(raster)
						lista = lista + ";" + raster
			
				LISTA = "\"" + lista[1:] + "\""		
				
				print "\n\t   ..averaging " + str(kelvin) + "_0" + str(month) + "\n"
				TmpRaster = dirtemp + "\\" + str(kelvin) + "_0" + str(month)
				gp.CellStatistics_sa(LISTA, TmpRaster, "MEAN")
				
				OutRaster = diroutPeriod + "\\" + str(kelvin) + "_0" + str(month)
				InExpression = TmpRaster + " + 273.15"
				gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
				gp.delete_management(TmpRaster)
				print "\n\t   ..done! \n"
				
			if month > 9 and not gp.Exists(diroutPeriod + "\\" + str(kelvin) + "_" + str(month)): 
				lista = ""
				print "\t   Processing " + str(kelvin) + "_" + str(month) + "\n"
				print "\t   ..listing grids "
				for year in range(int(period), int(period) + 29 + 1, 1):
					if gp.Exists(dirbase + "\\" + str(year) + "\\" + str(kelvin) + "\\" + str(kelvin) + "_" + str(month)):
						
						raster = dirbase + "\\" + str(year) + "\\" + str(kelvin) + "\\" + str(kelvin) + "_" + str(month)
						print "\t  " + str(year) + " " + os.path.basename(raster)
						lista = lista + ";" + raster
			
				LISTA = "\"" + lista[1:] + "\""		
				
				print "\n\t   ..averaging " + str(kelvin) + "_" + str(month) + "\n"
				TmpRaster = dirtemp + "\\" + str(kelvin) + "_" + str(month)
				gp.CellStatistics_sa(LISTA, TmpRaster, "MEAN")
				
				OutRaster = diroutPeriod + "\\" + str(kelvin) + "_" + str(month)
				InExpression = TmpRaster + " + 273.15"
				gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
				gp.delete_management(TmpRaster)
				print "\n\t   ..done! \n"
				
	# Variables without convert
	for variable in variableList:
		for month in range(1, 12 + 1, 1):
			if month < 10 and not gp.Exists(diroutPeriod + "\\" + str(variable) + "_0" + str(month)):
				lista = ""
				print "\t   Processing " + str(variable) + "_0" + str(month) + "\n"
				print "\t   ..listing grids "
				for year in range(int(period), int(period) + 29 + 1, 1):
					if gp.Exists(dirbase + "\\" + str(year) + "\\" + str(variable) + "\\" + str(variable) + "_0" + str(month)):
						
						raster = dirbase + "\\" + str(year) + "\\" + str(variable) + "\\" + str(variable) + "_0" + str(month)
						print "\t  " + str(year) + " " + os.path.basename(raster)
						lista = lista + ";" + raster
			
				LISTA = "\"" + lista[1:] + "\""		
				
				print "\n\t   ..averaging " + str(variable) + "_0" + str(month) + "\n"
				OutRaster = diroutPeriod + "\\" + str(variable) + "_0" + str(month)
				gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
				print "\n\t   ..done! \n"
				
			if month > 9 and not gp.Exists(diroutPeriod + "\\" + str(variable) + "_" + str(month)): 
				lista = ""
				print "\t   Processing " + str(variable) + "_" + str(month) + "\n"
				print "\t   ..listing grids "
				for year in range(int(period), int(period) + 29 + 1, 1):
					if gp.Exists(dirbase + "\\" + str(year) + "\\" + str(variable) + "\\" + str(variable) + "_" + str(month)):
						
						raster = dirbase + "\\" + str(year) + "\\" + str(variable) + "\\" + str(variable) + "_" + str(month)
						print "\t  " + str(year) + " " + os.path.basename(raster)
						lista = lista + ";" + raster
			
				LISTA = "\"" + lista[1:] + "\""		
				
				print "\n\t   ..averaging " + str(variable) + "_" + str(month) + "\n"
				OutRaster = diroutPeriod + "\\" + str(variable) + "_" + str(month)
				gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
				print "\n\t   ..done! \n"
				
print "\t > Process Done!!"