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
if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python D:\jetarapues\_scripts\00-check_availability_cordex.py U:\rcm\cordex\order"
	sys.exit(1)

# Define arguments
dirbase = sys.argv[1]
# rcp = sys.argv[2]
# dirout = sys.argv[2]

# Clearing screen and getting the arguments
os.system("cls")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   Prepare Monthly CMIP5 raw files    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# Define variables, rcp's adn ensemble members
varList = {"tas": "tmean", "tasmax": "tmax", "tasmin": "tmin", "pr": "prec", "rsds": "rsds", "hur": "hur", "sfcWind": "wsmean"}
rcpList =["historical", "rcp26", "rcp45", "rcp60", "rcp85"] # sorted(os.listdir(dirbase + "\\day\\" + dominio )) #

# listdominio = sorted(os.listdir(dirbase))
listdominio = sorted([d for d in os.listdir(dirbase) if os.path.isdir(os.path.join(dirbase, d))])


for dominio in listdominio:

	fileCheck = dirbase + "\\" + dominio + "-daily-data-summary.txt"
	
	if not os.path.isfile(fileCheck):
		sumFile = open(fileCheck, "w")
		sumFile.write("dominio" + "\t" +"rcp" + "\t" +"gcm" + "\t" +"rcm" + "\t" + "ensemble" + "\t" + "ver" + "\t" + "prec" + "\t" + "tmax" + "\t" + "tmean" + "\t" + "tmin" + "\t" + "hur" + "\t" + "rsds" + "\t" + "sfcWind" + "\n")
		sumFile.close()			

		for rcp in rcpList:
			rcpDir = dirbase+ "\\" +dominio+"\\day\\" + rcp
			if os.path.exists(rcpDir):
				modelList = sorted(os.listdir(rcpDir))
				for model in modelList:
					# Get an ensemble list
					rcmList = sorted(os.listdir(rcpDir + "\\" + model))
					for rcm in rcmList:
						ensList = sorted(os.listdir(rcpDir + "\\" + model+ "\\" +rcm))
						for ens in ensList:
							verList = sorted(os.listdir(rcpDir + "\\" + model+ "\\" +rcm+ "\\" +ens))
							for ver in verList:
								
								# Ensemble full path
								ensDir = rcpDir + "\\" + model + "\\" + rcm+ "\\" + ens+ "\\" +ver
								print ensDir
							
								# Get a list of nc files per variable
								ncList = sorted(glob.glob(ensDir + "\\pr_"+dominio+"*.nc"))
								if not len(ncList) == 0:
									
									precCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									precCheck = "no"
									
								ncList = sorted(glob.glob(ensDir + "\\tasmax_"+dominio+"*.nc"))
								if not len(ncList) == 0:	
									
									tmaxCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									tmaxCheck = "no"
								
								ncList = sorted(glob.glob(ensDir + "\\tas_"+dominio+"*.nc"))
								if not len(ncList) == 0:	
									
									tmeanCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									tmeanCheck = "no"
								
								ncList = sorted(glob.glob(ensDir + "\\tasmin_"+dominio+"*.nc"))
								if not len(ncList) == 0:	
									
									tminCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									tminCheck = "no"
									
								ncList = sorted(glob.glob(ensDir + "\\hur_"+dominio+"*.nc"))
								if not len(ncList) == 0:	
									
									hurCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									hurCheck = "no"
									
								ncList = sorted(glob.glob(ensDir + "\\rsds_"+dominio+"*.nc"))
								if not len(ncList) == 0:	
									
									rsdsCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									rsdsCheck = "no"

								
								ncList = sorted(glob.glob(ensDir + "\\sfcWind_"+dominio+"*.nc"))
								if not len(ncList) == 0:	
									
									sfcWindCheck = os.path.basename(ncList[0]).split("_")[-1].split("-")[0][:4] + "_" + os.path.basename(ncList[-1]).split("_")[-1].split("-")[1][:-5]
								else:
									sfcWindCheck = "no"			

								sumFileA = open(fileCheck, "a")
								sumFileA.write(dominio + "\t" +rcp + "\t" +model + "\t" + rcm + "\t" + ens+ "\t" + ver + "\t" + precCheck + "\t" + tmaxCheck + "\t" + tmeanCheck + "\t" + tminCheck + "\t" + hurCheck + "\t" + rsdsCheck + "\t" + sfcWindCheck + "\n" )
								
