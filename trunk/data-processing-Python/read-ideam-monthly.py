# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 27-3-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, csv
from csv import writer as csvwriter, reader as cvsreader

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python read-ideam-daily.py \\dapadfs\data_cluster_4\observed\weather_station\ideam-col\raw-monthly\ideam-stations-request-2014_03\CIAT.txt S:\observed\weather_station\ideam-col\organized-monthly\conventional-madr-2nd-request ideam_stations"
	sys.exit(1)

#Set variables 
infile = sys.argv[1]
dirout = sys.argv[2]
summary = sys.argv[3]
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
	if line.find("VALORES TOTALES MENSUALES DE PRECIPITACION") > -1:
		var = "prec"
	elif line.find("VALORES MEDIA-MINI MENSUALES DE TEMPERATURA") > -1:
		var = "tmean"
	elif line.find("VALORES TOTALES MENSUALES DE BRILLO SOLAR") > -1:
		var = "sbright"
	elif line.find("VALORES MEDIOS  MENSUALES DE TEMPERATURA") > -1:
		var = "tmean"
	elif line.find("VALORES MINIMOS MENSUALES DE TEMPERATURA") > -1:
		var = "tmin"
	elif line.find("VALORES MAXIMOS MENSUALES DE TEMPERATURA") > -1:
		var = "tmax"
		
		
	## Read weather info txt plain file
	if line.find("ESTACION :") > -1:
		# year = line[59:63]
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
		print stNumber, stName[:-1], var
		staFile = diroutvar + "\\" + stNumber + ".txt"
		if not os.path.isfile(staFile):
			wFile = open(staFile, "w")
			wFile.write("Date" + "\t" + "Value" + "\n")
			wFile.close()
	
	## Read and write climate data
	
	year = line[1:5]
	
	if year.isdigit():
		
		if int(year) > 1950 and int(year) < 2014:
			print year
		
		# if line[1:5] 
		# for index in range(1, 37, 1):
			# line = file.next()
			# if index > 5:
				# day = line[11:13]
				
			
			## Split month columns
			for month in range(1, 12 + 1, 1):						
				initcol = 14 + int(9 * int(month - 1))
				if var == "wsmean":
					endcol = initcol + 7
				else:
					endcol = initcol + 5
				data = line[initcol:endcol]
		
				## NA data
				if data == "     " or data == "  +  " or data[0:3] == "  +" or data[0:3] == "  *" or data is None or len(data) == 0:
					val = "NA"
				else:
					if var == "wsmean":
						val = str(data)
					else:
						val = float(data)
				
				# ## Include leap-year
				# if int(year) % 400 == 0 or int(year) % 4 == 0:
					# lastdaymonth = monDcLeap[str(month)]
				# else:
					# lastdaymonth = monDc[str(month)]
				
				## Obtain date
				if month < 10:
					date = year + "0" + str(month)
				else:
					date = year + str(month)
				
				## Write output file
				# if int(day) < int(lastdaymonth) + 1:
				wFile = open(staFile, "a")
				wFile.write(date + "\t" + str(val) + "\n")
			
			wFile.close()
	
		## Write catalog file
		catFile = dirout + "\\" + summary + ".txt"
		infoSta = stNumber + "\t" + stName[:-1] + "\t" + lat + "\t" + lon + "\t" + elev + "\t" + stType + "\t" + var + "\t" + dept + "\t" + mun + "\t" + insDate + "\t" + susDate + "\n"
		
		if not os.path.isfile(catFile):
			cFile = open(catFile, "w")
			cFile.write("StationNumber" + "\t" + "StationName" + "\t" + "Latitude" + "\t" + "Longitude" + "\t" + "Elevation" + "\t" + "StationType" + "\t" + "Variable" + "\t" + "Departament" + "\t" + "Municipality" + "\t" + "InstalationDate" + "\t" + "SuspensionDate " + "\n")
			cFile.write(infoSta)
			cFile.close()
		else:
			cFile = open(catFile, "r")
			lst = cFile.readlines()
			lastline = lst[len(lst)-1]
			if not lastline == infoSta:
				cFile.close()
				cFile = open(catFile, "a")
				cFile.write(infoSta)
			cFile.close()

## Close input txt file
file.close()


### Sort 