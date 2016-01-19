# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 11-26-2015
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, glob, shutil

#Syntax 
if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ghcn-stations-copy-daily.py S:\observed\weather_station\ghcn\raw\daily\ghcnd_all D:\Documents\Desktop\agrihack_data\observed\weather_stations\ghcn-daily\ghcnd_all"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     		Copy Station Files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

countrylist = "AG","AO","BN","BC","UV","BY","CM","CT","CD","CF","IV","EG","ER","ET","GB","GV","KE","LT","LY","MA","MI","ML","MR","MP","MO","MZ","WA","NG","SH","SG","SE","SL","SF","SU","WZ","TZ","TO","TS","UG","ZA","ZI"

for country in countrylist:
	
	filelist = sorted(glob.glob(dirbase + "\\" + country + "*.dly"))
	
	for file in filelist:
		
		print os.path.basename(file)
		shutil.copyfile(file, dirout + "\\" + os.path.basename(file))
					
print "done!"