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
	print "   - ie: python 00-check-availability.py T:\gcm\cmip5\raw amip"
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
		
		# Ensemble full path
		ensDir = rcpDir + "\\" + model + "\\" + ens
		print ensDir
		
		sumFile = dirbase + "\\" + rcp + "-daily-data-summary.txt"
		if not os.path.isfile(sumFile):
			sumFile = open(sumFile, "w")
			sumFile.write("rcp" + "\t" + "gcm" + "\t" + "ensemble" + "\t" + "prec" + "\t" + "tmax" + "\t" + "tmean" + "\t" + "tmin" + "\n")
			sumFile.close()
	
		# Get a list of nc files per variable
		ncList = sorted(glob.glob(ensDir + "\\pr_day*.nc"))
		if not len(ncList) == 0:
			
			precCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
		else:
			precCheck = "no"
			
		ncList = sorted(glob.glob(ensDir + "\\tasmax_day*.nc"))
		if not len(ncList) == 0:	
			
			tmaxCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
		else:
			tmaxCheck = "no"
		
		ncList = sorted(glob.glob(ensDir + "\\tas_day*.nc"))
		if not len(ncList) == 0:	
			
			tmeanCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
		else:
			tmeanCheck = "no"
		
		ncList = sorted(glob.glob(ensDir + "\\tasmin_day*.nc"))
		if not len(ncList) == 0:	
			
			tminCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
		else:
			tminCheck = "no"

		sumFile = open(sumFile, "a")
		sumFile.write(rcp + "\t" + model + "\t" + ens + "\t" + precCheck + "\t" + tmaxCheck + "\t" + tmeanCheck + "\t" + tminCheck + "\n" )
		
