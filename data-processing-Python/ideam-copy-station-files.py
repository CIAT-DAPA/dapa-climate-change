# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 4-4-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, glob, shutil

#Syntax 
if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ideam-copy-station-files.py S:\observed\weather_station\ideam-col\organized-daily\conventional D:\CIAT\climate_change\station_data\ideam"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirlist = sys.argv[2]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     		Copy Station Files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

varlist = "evap", "prec", "rhum", "sbright", "srad", "sradv", "tmax", "tmean", "tmin", "wsmean"

filelist = sorted(glob.glob(dirlist + "\\*.txt"))
			
for var in varlist:

	for listfile in filelist:

		dirout = dirlist + "\\extract-" + str(os.path.basename(listfile))[:-4] + "\\" + var + "-per-station"
		if not os.path.exists(dirout):
			os.system("mkdir " + dirout)
	
		listfile = open(listfile)
		for line in listfile:
			
			if not line[:-1] == "StationNum":
				
				print "Extracting info for ", line[:-1]
				if os.path.exists(dirbase + "\\" + var + "-per-station\\" + line[:-1] + ".txt"):		
					shutil.copyfile(dirbase + "\\" + var + "-per-station\\" + line[:-1] + ".txt", dirout + "\\" + line[:-1] + ".txt")
					
print "Sorted Ideam Stations done!"