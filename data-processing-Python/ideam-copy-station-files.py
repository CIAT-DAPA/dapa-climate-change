# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 4-4-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, glob, shutil

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ideam-copy-station-files.py S:\observed\weather_station\col-ideam\daily-raw D:\CIAT\Workspace\col_cormacarena\00_station_data\station_list.txt D:\CIAT\Workspace\col_cormacarena\00_station_data"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
listfile = sys.argv[2]
dirout = sys.argv[3]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     		Copy Station Files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

varlist = "evap", "prec", "rhum", "sbright", "srad", "sradv", "tmax", "tmean", "tmin", "wsmean"

# filelist = sorted(glob.glob(dirlist + "\\*.txt"))
	
listfile = open(listfile)

for line in listfile:
	
	for var in varlist:
	
		if not os.path.exists(dirout + "\\" + var + "-per-station"):
			os.system("mkdir " + dirout + "\\" + var + "-per-station")
			
		print "Extracting info for ", line[:-1]
		if os.path.exists(dirbase + "\\" + var + "-per-station\\" + line[:-1] + "_raw_" + var + ".txt"):		
			shutil.copyfile(dirbase + "\\" + var + "-per-station\\" + line[:-1] + "_raw_" + var + ".txt", dirout + "\\" + var + "-per-station\\" + line[:-1] + "_raw_" + var + ".txt")
			
print "Sorted Ideam Stations done!"