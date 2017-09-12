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
	print "   - ie: python 05-INAMHI-read.py Z:\DATA\WP2\01_Weather_Stations\ECU S:\observed\weather_station\ecu-inamhi\daily-raw prec"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]
variable = sys.argv[3]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     	 Read INAMHI daily files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

## Open weather file
file = open(dirbase + "\\" + variable + "_daily_merge.txt", "r")
lines =  file.read().splitlines()

dirout_var = dirout + "\\" + variable + "-per-station"
if not os.path.exists(dirout_var):
    os.system('mkdir ' + dirout_var)
	
## Write organized csv weather file file
staFile = dirout + "\\station_catalog_daily.txt"
if not os.path.isfile(staFile):
	wFile = open(staFile, "w")
	wFile.write("Station" + "\t" + "Name" + "\t" + "Lat" + "\t" + "Lon" + "\t" + "Alt" + "\n")
	wFile.close()
			
## Loop around lines
for k in range(len(lines)):

	line = lines[k]
	
	## Read weather info txt plain file
	if line.find("Nombre") > -1:
		line = lines[k+1]
		stName = line.split("\t")[0]
		stNumber = line.split("\t")[3].lower()
		lat = line.split("\t")[4]
		lon = line.split("\t")[5]
		alt = line.split("\t")[6]

		oFile = dirout_var + "\\" + stNumber + "_raw_" + variable + ".txt"
		cFile = open(oFile, "w")
		cFile.write("Date" + "\t" + "Value" + "\n")
		cFile.close()
		
		wFile = open(staFile, "a")
		wFile.write(stNumber + "\t" + stName + "\t" + lat + "\t" + lon + "\t" + alt + "\n")
		wFile.close()
		
		print stNumber, stName, variable
	
	if line.find("Mes") > -1:
		
		nYears = len(line.split("\t")) - 1
		
		for i in range (1, nYears, 1):
			
			year = line.split("\t")[i+1]

			if year:
				
				for j in range(1, 366 + 1, 1):		
					
					linemod = lines[k+j]
					
					month = "%02d" % int(linemod.split("\t")[0])
					day = "%02d" % int(linemod.split("\t")[1])
					date = year + month + day
					value = linemod.split("\t")[1 + i]
					
					if not value:
						value = "NA"
					
					cFile = open(oFile, "a")
					cFile.write(date + "\t" + value + "\n")
					cFile.close()
				
## Close input txt file
file.close()
