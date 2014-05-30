#---------------------------------------------------------------------------------------------------------
# Description: This script is to prepare the CMIP5 raw climate data, spliting in monthly files. 
# Author: Carlos Navarro
# Date: 25/04/13
# Notes: This python script is to be run under Windows or Linux or with the proper software (cdo) available
# 		 within any of the following folders:
#      	 -/bin/
#      	 -/usr/local/bin/
#      	 -/USERNAME/bin/
# 		 cdo is required to merge raw files, separate years and then months. 
#---------------------------------------------------------------------------------------------------------

# Import system modules
import os, sys, string, glob, shutil

# Syntax
if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 00-check-daily-raw-data.py T:\gcm\cmip5\raw\daily historical"
	sys.exit(1)

# Define arguments
dirbase = sys.argv[1]
rcp = sys.argv[2]
# dirout = sys.argv[2]

# Clearing screen and getting the arguments
os.system("cls")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   Prepare Monthly CMIP5 raw files    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# Define variables, rcp's adn ensemble members
varList = {"tas": "tmean", "tasmax": "tmax", "tasmin": "tmin", "pr": "prec", "rsds": "rsds", "hur": "hur", "sfcWind": "wsmean"}
# rcpList = "historical", "rcp26", "rcp45", "rcp60", "rcp85"
# ens = "r1i1p1"

# Reorganize raw data files into a new subfolder 
# for rcp in rcpList:
rcpDir = dirbase + "\\" + rcp

# Get a list of models
modelList = sorted(os.listdir(rcpDir))
for model in modelList:
	
	# Get an ensemble list
	ensList = sorted(os.listdir(rcpDir + "\\" + model))
	for ens in ensList:
		
		if os.path.isdir(rcpDir + "\\" + model + "\\" + ens):
			
			# checkFile = rcpDir + "\\" + model + "\\" + ens + "-merge-daily-done.txt"
			
			# Ensemble full path
			ensDir = rcpDir + "\\" + model + "\\" + ens
			print ensDir
			print "\tProcess done for ", rcp, model, ens, "\n"
			sumFile = dirbase + "\\cmip5-" + rcp + "-daily-data-summary.txt"
			if not os.path.isfile(sumFile):
				sumFile = open(sumFile, "w")
				sumFile.write("rcp" + "\t" + "gcm" + "\t" + "ensemble" + "\t" + "prec" + "\t" + "tmax" + "\t" + "tmean" + "\t" + "tmin" + "\t" + "hur" + "\t" + "rsds" + "\n")
				sumFile.close()
			
			sumFile = open(sumFile, "a")
			
			precList = sorted(glob.glob(ensDir + "\\pr_*.nc"))
			print precList
			if not len(precList) == 0:
				precCheck = os.path.basename(precList[0]).split("_")[-1][:-3]
			else:
				precCheck = "no"
				
			tmaxList = sorted(glob.glob(ensDir + "\\tasmax_*.nc"))
			if not len(tmaxList) == 0:
				tmaxCheck = os.path.basename(tmaxList[0]).split("_")[-1][:-3]
			else:
				tmaxCheck = "no"
			
			tmeanList = sorted(glob.glob(ensDir + "\\tas_*.nc"))
			if not len(tmeanList) == 0:
				tmeanCheck = os.path.basename(tmeanList[0]).split("_")[-1][:-3]
			else:
				tmeanCheck = "no"
			
			tminList = sorted(glob.glob(ensDir + "\\tasmin_*.nc"))
			if not len(tminList) == 0:
				tminCheck = os.path.basename(tminList[0]).split("_")[-1][:-3]
			else:
				tminCheck = "no"
				
			hurList = sorted(glob.glob(ensDir + "\\hur_*.nc"))
			if not len(hurList) == 0:
				hurCheck = os.path.basename(hurList[0]).split("_")[-1][:-3]
			else:
				hurCheck = "no"

			rsdsList = sorted(glob.glob(ensDir + "\\rsds_*.nc"))
			if not len(rsdsList) == 0:
				rsdsCheck = os.path.basename(rsdsList[0]).split("_")[-1][:-3]
			else:
				rsdsCheck = "no"					
				
			# sumFile = open(sumFile, "a")
			sumFile.write(rcp + "\t" + model + "\t" + ens + "\t" + precCheck + "\t" + tmaxCheck + "\t" + tmeanCheck + "\t" + tminCheck + "\t" + hurCheck + "\t" + rsdsCheck + "\n" )
					
print "Process done!"
