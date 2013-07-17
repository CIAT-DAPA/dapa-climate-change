# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 4-4-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, glob, shutil

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python gsod-stations-copy.py S:\observed\weather_station\gsod\raw D:\CIAT\Workspace\Request\mboa\gsod_extract D:\CIAT\Workspace\Request\mboa\list"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]
dirlist = sys.argv[3]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     		Copy Station Files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

for year in range(1960, 2013 + 1, 1):
	yeardir = dirbase + "\\" + str(year)
	file = open(dirlist + "\\st_gsod.csv")
	yearoutdir = dirout + "\\" + str(year)
	if not os.path.exists(yearoutdir):
		os.system("mkdir " + yearoutdir)
	
	for line in file:

		if line.split(",")[0][0:4] == "GSOD":
			# print "Copying", yeardir + "\\" + line.split(",")[0][4:] + "-" + line.split(",")[1][4:] + "-" + str(year) +  ".op.gz"
			if os.path.exists(yeardir + "\\" + line.split(",")[0][4:] + "-" + line.split(",")[1][4:] + "-" + str(year) +  ".op.gz"):
			
				print "Copying", yeardir + "\\" + line.split(",")[0][4:] + "-" + line.split(",")[1][4:] + "-" + str(year) +  ".op.gz"
				shutil.copyfile(yeardir + "\\" + line.split(",")[0][4:] + "-" + line.split(",")[1][4:] + "-" + str(year) +  ".op.gz", yearoutdir + "\\" + line.split(",")[0][4:] + "-" + line.split(",")[1][4:] + "-" + str(year) +  ".op.gz")
			
	# for listfile in filelist:

		# dirout = dirlist + "\\extract-" + str(os.path.basename(listfile))[:-4] + "\\" + var + "-per-station"
		# if not os.path.exists(dirout):
			# os.system("mkdir " + dirout)
	
		# listfile = open(listfile)
		# for line in listfile:
			
			# if not line[:-1] == "StationNum":
				
				# print "Extracting info for ", line[:-1]
				# if os.path.exists(dirbase + "\\" + var + "-per-station\\" + line[:-1] + ".txt"):		
					# shutil.copyfile(dirbase + "\\" + var + "-per-station\\" + line[:-1] + ".txt", dirout + "\\" + line[:-1] + ".txt")
