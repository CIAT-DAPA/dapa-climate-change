# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 27-3-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, csv
from csv import writer as csvwriter, reader as cvsreader

#Syntax 
if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python read-ideam-daily.py S:\observed\weather_station\ideam-col\raw-daily\Meta2-Diarios.txt S:\observed\weather_station\ideam-col\organized-data-daily"
	sys.exit(1)

#Set variables 
infile = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

#Clear screen
os.system('cls')

monDc = {"1": 31, "2": 28, "3": 31, "4": 30, "5": 31, "6": 30, 
         "7": 31, "8": 31, "9": 30, "10": 31, "11": 30, "12": 31}

monDcLeap = {"1": 31, "2": 29, "3": 31, "4": 30, "5": 31, "6": 30, 
         "7": 31, "8": 31, "9": 30, "10": 31, "11": 30, "12": 31}

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     	 Read ideam daily files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

## Open weather file
file = open(infile)

## Loop around lines
for line in file:
	
	## Define var name
	if line.find("VALORES TOTALES DIARIOS DE PRECIPITACION") > -1:
		var = "prec"
	elif line.find("VALORES MEDIOS  DIARIOS DE TEMPERATURA") > -1:
		var = "tmean"
	elif line.find("VALORES MAXIMOS DIARIOS DE TEMPERATURA") > -1:
		var = "tmax"
	elif line.find("VALORES MINIMOS  DIARIOS DE TEMPERATURA") > -1:
		var = "tmin"
	elif line.find("VALORES MEDIOS  DIARIOS DE HUMEDAD RELATIVA") > -1:
		var = "rhum"
	elif line.find("VALORES TOTALES DIARIOS DE EVAPORACION") > -1:
		var = "evap"
	elif line.find("VALORES TOTALES DIARIOS DE BRILLO SOLAR") > -1:
		var = "sbright"
	elif line.find("DIARIOS DE VELOCIDAD DEL VIENTO") > -1: 
		var = "wsmean"

	## Read weather info txt plain file
	if line.find("ESTACION :") > -1:
		year = line[59:63]
		stNumber = line[104:112]
		stName = line[114:130]
	if line.find("LATITUD") > -1:
		lat = line[15:19]
		stType = line[48:52]
		dept = line[80:97]
		insDate = line[124:132]
	if line.find("LONGITUD") > -1:
		lon = line[15:19]
		mun = line[80:97]
		susDate = line[124:132]
			
	if line.find("ELEVACION") > -1:
		elev = line[15:19]

		## Create output folder per variable
		diroutvar = dirout + "\\" + var + "-per-station"
		if not os.path.exists(diroutvar):
			os.system('mkdir ' + diroutvar)

		## Write organized csv weather file file
		print stNumber, stName[:-1], year, var
		staFile = diroutvar + "\\" + stNumber + ".txt"
		if not os.path.isfile(staFile):
			wFile = open(staFile, "w")
			wFile.write("Date" + "\t" + "Value" + "\n")
			wFile.close()
		
		## Read and write climate data
		for index in range(1, 37, 1):
			line = file.next()
			if index > 5:
				day = line[11:13]
				
				## Split month columns
				for month in range(1, 12 + 1, 1):						
					initcol = 20 + int(9 * int(month - 1))
					if var == "wsmean":
						endcol = initcol + 7
					else:
						endcol = initcol + 5
					data = line[initcol:endcol]
			
					## NA data
					if data == "     " or data == "  +  " or data is None or len(data) == 0:
						val = "NA"
					else:
						val = float(data)
					
					## Include leap-year
					if int(year) % 400 == 0 or int(year) % 4 == 0:
						lastdaymonth = monDcLeap[str(month)]
					else:
						lastdaymonth = monDc[str(month)]
					
					## Obtain date
					if month < 10:
						date = year + "0" + str(month) + day
					else:
						date = year + str(month) + day
					
					## Write output file
					if int(day) < int(lastdaymonth) + 1:
						wFile = open(staFile, "a")
						wFile.write(date + "\t" + str(val) + "\n")
		wFile.close()
	
		## Write catalog file
		catFile = dirout + "\\ideam_" + var + "_stations.txt"
		infoSta = stNumber + "\t" + stName[:-1] + "\t" + lat + "\t" + lon + "\t" + elev + "\t" + stType + "\t" + dept + "\t" + mun + "\t" + insDate + "\t" + susDate + "\n"
		
		if not os.path.isfile(catFile):
			cFile = open(catFile, "w")
			cFile.write("StationNumber" + "\t" + "StationName" + "\t" + "Latitude" + "\t" + "Longitude" + "\t" + "Elevation" + "\t" + "StationType" + "\t" + "Departamente" + "\t" + "Municipality" + "\t" + "InstalationDate" + "\t" + "SuspensionDate " + "\n")
			cFile.write(infoSta)
			cFile.close()
		else:
			cFile = open(catFile, "a")
			cFile.write(infoSta)
			cFile.close()

## Close input txt file
file.close()