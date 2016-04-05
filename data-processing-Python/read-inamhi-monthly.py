# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 27-3-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, csv
from csv import writer as csvwriter, reader as cvsreader

#Syntax 
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python read-inamhi-monthly.py D:\CIAT\Projects\ecu-hidroelectrica\01_station_data\st_data_inamhi_raw\MENSUAL\tmax_all_stations_raw.txt D:\CIAT\Projects\ecu-hidroelectrica\01_station_data\st_data_inamhi_raw\MENSUAL inamhi_stations tmin"
	sys.exit(1)

#Set variables 
infile = sys.argv[1]
dirout = sys.argv[2]
summary = sys.argv[3]
variable = sys.argv[4]
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

## Write organized csv weather file file

staFile = dirout + "\\" + variable + "_all_stations_proc.txt"
if not os.path.isfile(staFile):
	wFile = open(staFile, "w")
	wFile.write("Station" + "\t" + "Year" + "\tJan\tFeb\tMar\tApr\tMay\tJun\tJul\tAug\tSep\tOct\tNov\tDec\n")
	wFile.close()
	
## Loop around lines
for line in file:

	## Read weather info txt plain file
	if line.find("M") > -1 and not line.find("ENE") > -1:
		stName = line.split("\t")[2]
		stNumber = line.split("\t")[0]
		lat = line.split("\t")[4]
		lon = line.split("\t")[3]
		alt = line.split("\t")[5]

		print stNumber, stName, variable
	
	## Read and write climate data
	
	year = line.split("\t")[0]
	
	if year.isdigit():

		print year
	
		## Write output file
		# if int(day) < int(lastdaymonth) + 1:
		wFile = open(staFile, "a")
		wFile.write(stNumber + "\t" + line)
		
		wFile.close()
	
		# ## Write catalog file
		
		# infoSta = stNumber + "\t" + stName[:-1] + "\t" + lat + "\t" + lon + "\t" + elev + "\t" + stType + "\t" + var + "\t" + dept + "\t" + mun + "\t" + insDate + "\t" + susDate + "\n"
		
		# if not os.path.isfile(catFile):
			# cFile = open(catFile, "w")
			# cFile.write("StationNumber" + "\t" + "StationName" + "\t" + "Latitude" + "\t" + "Longitude" + "\t" + "Elevation" + "\t" + "StationType" + "\t" + "Variable" + "\t" + "Departament" + "\t" + "Municipality" + "\t" + "InstalationDate" + "\t" + "SuspensionDate " + "\n")
			# cFile.write(infoSta)
			# cFile.close()
		# else:
			# cFile = open(catFile, "r")
			# lst = cFile.readlines()
			# lastline = lst[len(lst)-1]
			# if not lastline == infoSta:
				# cFile.close()
				# cFile = open(catFile, "a")
				# cFile.write(infoSta)
			# cFile.close()

## Close input txt file
file.close()


### Sort 