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
	print "   - ie: python 00-merge-daily-raw-data.py T:\data\gcm\cmip5\raw\daily historical"
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
			
			# if not os.path.exists(checkFile):
		
			# Define new folder for original files
			rawDir = ensDir + "\\original-data"
			if not os.path.exists(rawDir):
				os.system("mkdir " + rawDir)
		
			# Loop around variables
			for var in sorted(varList):
				
				# Get a list of nc files per variable
				ncList = sorted(glob.glob(ensDir + "\\" + var + "_day*.nc"))
				print ncList
				if not len(ncList) == 0:

					# Extract start and end date
					staYear = os.path.basename(ncList[0]).split("_")[-1].split("-")[0]
					endYear = os.path.basename(ncList[-1]).split("_")[-1].split("-")[1]
					
					# Define merge file by variable
					merNc = ensDir + "\\" + var + "_day_" + os.path.basename(ncList[0]).split("_")[2] + "_" + os.path.basename(ncList[0]).split("_")[3] + "_" + os.path.basename(ncList[0]).split("_")[4] + "_" + staYear + "-" + endYear
					
					#### Merge nc files per variable (or rename for raw singles files)
					
					if len(ncList) > 1:
						print " .> Merge ", rcp, model, ens, var
						if not os.path.exists(merNc):
							os.system("cdo mergetime " + ' '.join(ncList) + " " + merNc)
							for nc in ncList:
								shutil.move(nc, rawDir + "\\" + os.path.basename(nc))
						
					# else:
						# print " .> Not need merge ", rcp, model, ens, var
						# if not os.path.exists(merNc):
							# shutil.copyfile(ncList[0], merNc)
							
					# Compressing original data
					# for nc in ncList:
						# inZip = rawDir + "\\" + var + ".zip"
						# os.system('7za a ' + inZip + " " + nc)
						# os.remove(nc)

				
					# Move merge file
					# shutil.move(merNc, rawDir + "\\" + os.path.basename(merNc))
			
				# Write check txt file (one per model-ensemble)
				# checkFile = open(checkFile, "w")
				# checkFile.write(str(ncList))
				# checkFile.close()
			
				# print "\tProcess done for ", rcp, model, ens, "\n"
			
			# else:
			
				# print "\tProcess done for ", rcp, model, ens, "\n"
				# sumFile = dirbase + "\\cmip5-" + rcp + "-daily-data-summary.txt"
				# if not os.path.isfile(sumFile):
					# sumFile = open(sumFile, "w")
					# sumFile.write("rcp" + "\t" + "gcm" + "\t" + "ensemble" + "\t" + "prec" + "\t" + "tmax" + "\t" + "tmean" + "\t" + "tmin" + "\n")
					# sumFile.close()
				
					# precList = sorted(glob.glob(ensDir + "\\original-data\\prec.*.nc"))
					# if not len(precList) == 0:
						# precCheck = os.path.basename(precList[0]).split(".")[1]
					# else:
						# precCheck = "no"
						
					# tmaxList = sorted(glob.glob(ensDir + "\\original-data\\tmax.*.nc"))
					# if not len(tmaxList) == 0:
						# tmaxCheck = os.path.basename(tmaxList[0]).split(".")[1]
					# else:
						# tmaxCheck = "no"
					
					# tmeanList = sorted(glob.glob(ensDir + "\\original-data\\tmean.*.nc"))
					# if not len(tmeanList) == 0:
						# tmeanCheck = os.path.basename(tmeanList[0]).split(".")[1]
					# else:
						# tmeanCheck = "no"
					
					# tminList = sorted(glob.glob(ensDir + "\\original-data\\tmin.*.nc"))
					# if not len(tminList) == 0:
						# tminCheck = os.path.basename(tminList[0]).split(".")[1]
					# else:
						# tminCheck = "no"

					# sumFile = open(sumFile, "a")
					# sumFile.write(rcp + "\t" + model + "\t" + ens + "\t" + precCheck + "\t" + tmaxCheck + "\t" + tmeanCheck + "\t" + tminCheck + "\n" )
					
print "Process done!"
