# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 27-3-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, csv, glob
from csv import writer as csvwriter, reader as cvsreader

#Syntax 
if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 07_SENAMHI-read.py D:\CIAT\Projects\lat_bmu\01_Weather_Stations\PER\monthly-raw D:\CIAT\Projects\lat_bmu\01_Weather_Stations\PER\monthly-raw"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "       Read SENAMHI monthly files	     " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

## Open weather file
files = glob.glob(dirbase + "\*")
print files 
for infile in files:
	
	## Open weather file
	file = open(infile)

	## Loop around lines
	for line in file:
		
		## Define var name
		if line.find("PRECIPITACION TOTAL MENSUAL") > -1:
			var = "prec"
		elif line.find("TEMPERATURA MAXIMA MEDIA MENSUAL") > -1:
			var = "tmax"
		elif line.find("TEMPERATURA MINIMA MEDIA MENSUAL") > -1:
			var = "tmin"
		elif line.find("HUMEDAD RELATIVA MEDIA MENSUAL") > -1:
			var = "rhum"
		elif line.find("EVAPORACION TOTAL MENSUAL TANQUE") > -1:
			var = "evap"
		# elif line.find("VALORES TOTALES DIARIOS DE BRILLO SOLAR") > -1:
			# var = "sbright"
		# elif line.find("DIARIOS DE VELOCIDAD DEL VIENTO") > -1: 
			# var = "wsmean"

		## Read weather info txt plain file
		if line.find("ESTACION: ") > -1:
			stNumber = line[10:16]
			print stNumber
			
		if 'var' in locals():
			
			## Create output folder per variable
			diroutvar = dirout + "\\" + var + "-per-station"
			if not os.path.exists(diroutvar):
				os.system('mkdir ' + diroutvar)
				
			if 'stNumber' in locals():
				
				
				## Write organized csv weather file file
				staFile = diroutvar + "\\" + stNumber + "_raw_" + var + ".txt"
				if not os.path.isfile(staFile):
					wFile = open(staFile, "w")
					wFile.write("Year" + "\t" + "Month" + "\t" + "Value" + "\n")
					wFile.close()
				
		if line.split("\t")[0].isdigit():
						
			year = line.split("\t")[0]	
			for month in range(1, 12 + 1, 1):
			
				if month == int(12):
					value = line.split("\t")[month][:-1]
				else: 
					value = line.split("\t")[month]
				
				if value == "---" or value == "S/D":
					value = "NA"
				
				wFile = open(staFile, "a")
				wFile.write(year + "\t" + str(month) + "\t" + str(value) + "\n")
				
			wFile.close()
			
	## Close input txt file
	file.close()
