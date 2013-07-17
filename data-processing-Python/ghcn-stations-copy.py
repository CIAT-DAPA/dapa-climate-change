# ---------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 4-4-2013
# ----------------------------------------------------------------------------------------------------------------

import os, sys, string, glob, shutil

#Syntax 
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ghcn-stations-copy.py S:\observed\weather_station\ghcn\organized-data D:\CIAT\Workspace\Request\mboa\ghcn_extract D:\CIAT\Workspace\Request\mboa\list noadj"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]
dirlist = sys.argv[3]
type = sys.argv[4]

#Clear screen
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     		Copy Station Files			 " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

varlist = "rain", "tmax", "tmean", "tmin"
		
for var in varlist:
	
	if type == "adj":
		vardir = dirbase + "\\" + var + "_adj-per-station"
		varoutdir = dirout + "\\" + var + "-adj-per-station"
		if not os.path.exists(varoutdir):
			os.system("mkdir " + varoutdir)
		file = open(dirlist + "\\" + var + "_adj_ghcn.csv")
		
	else: 
		vardir = dirbase + "\\" + var + "-per-station"
		varoutdir = dirout + "\\" + var + "-per-station"
		if not os.path.exists(varoutdir):
			os.system("mkdir " + varoutdir)
			
		file = open(dirlist + "\\" + var + "_ghcn.csv")
		
	for line in file:

		if line.split(",")[0][0:4] == "GHCN":
			if os.path.exists(vardir + "\\" + line.split(",")[0] + ".csv"):
			
				print "Copying", line.split(",")[0]
				shutil.copyfile(vardir + "\\" + line.split(",")[0] + ".csv", varoutdir + "\\" + line.split(",")[0] + ".csv")
		
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
					
print "Sorted Ideam Stations done!"