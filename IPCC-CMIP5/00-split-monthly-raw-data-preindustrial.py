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
	print "   - ie: python 00-split-monthly-raw-data-preindustrial.py \\dapadfs\data_cluster_2\gcm\cmip5\raw\monthly historical"
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

# Define variables, rcp's and ensemble members
varList = {"tas": "tmean", "pr": "prec", "tasmax": "tmax", "tasmin": "tmin"} #, "rsds": "rsds", "hur": "hur", "sfcWind": "wsmean"}
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
			
			checkFile = rcpDir + "\\" + model + "\\" + ens + "-preindustrial-split-month-done.txt"
			
			# Ensemble full path
			ensDir = rcpDir + "\\" + model + "\\" + ens
			
			if not os.path.exists(checkFile):
			
				# Define new folder for original files
				rawDir = ensDir + "\\original-data"
				if not os.path.exists(rawDir):
					os.system("mkdir " + rawDir)
			
				# Loop around variables
				for var in varList:
					
					# Get a list of nc files per variable
					ncList = sorted(glob.glob(rawDir + "\\" + varList[var] + "*.nc"))
											
					# Compressing original data
					for merNc in ncList:
						
						#### Splitting the initial file into yearly files and copying into the yearly_path folder (2014, 2024, 2084)
						print "\tSplit yearly ", rcp, model, ens, var
						
						# Path where monthly files are located
						mthDir = ensDir + "\\monthly-files"
						if not os.path.exists(mthDir):
							os.system("mkdir " + mthDir)
						
						# Splitting the initial file into yearly files
						os.system("cdo splityear " + merNc + " " + mthDir + "/" +  varList[var] + "_")				
						
						if not model.split("_")[0] == "gfdl":
						
							# Splitting the yearly file into monthly files and copying into the multiyr_path folder
							ncYrList = sorted(glob.glob(mthDir + "\\" + "*.nc"))
							for ncYr in ncYrList:
								
								# Create folder by year
								year = os.path.basename(ncYr).split("_")[1][:-3]
							
								print "\tSplit monthly ", rcp, model, ens, var, os.path.basename(ncYr).split("_")[1][:-3]
								if int(year) < 1950:

									yrDir = mthDir + "\\" + year
									if not os.path.exists(yrDir):
										os.system("mkdir " + yrDir)
									
									# Split month function
									os.system("cdo splitmon " + ncYr + " " + yrDir + "/" + varList[var] + "_")
								
								os.remove(ncYr)
					
						else: 
							
							# Splitting the yearly file into monthly files and copying into the multiyr_path folder
							ncYrList = sorted(glob.glob(mthDir + "\\" + "*.nc*"))
							for ncYr in ncYrList:
								
								# Create folder by year
								year = os.path.basename(ncYr).split("_")[1][:-4]
							
								print "\tSplit monthly ", rcp, model, ens, var, os.path.basename(ncYr).split("_")[1][:-3]
								if int(year) < 1950:

									yrDir = mthDir + "\\" + year
									if not os.path.exists(yrDir):
										os.system("mkdir " + yrDir)
									
									# Split month function
									os.system("cdo splitmon " + ncYr + " " + yrDir + "/" + varList[var] + "_")
									ncMtList = sorted(glob.glob(yrDir + "\\" + varList[var] + "*.nc2"))
									for ncMt in ncMtList:
										if os.path.exists(ncMt[:-1]):
											os.remove(ncMt[:-1])
										os.rename(ncMt, ncMt[:-1])
								
								os.remove(ncYr)
					
				# Write check txt file (one per model-ensemble)
				checkFile = open(checkFile, "w")
				checkFile.write(str(ncList))
				checkFile.close()
		
			print "\tProcess done for ", rcp, model, ens, "\n"
				
				
print "Process done!"
