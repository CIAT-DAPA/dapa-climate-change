# ---------------------------------------------------------------------------------
# Author: Carlos Navarro, Jaime Tarapues
# Date: June 2020
# Purpose: Extract by points GCM-CMIP5
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

# python ExtractGCM_CMIP5_points.py T:\gcm\cmip5\anomalies D:\cenavarro\request_unal\points_valle.shp D:\cenavarro\request_unal\extract ALL 2020_2049,2040_2069,2060_2089 NO 30s ALL

# Arguments
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
rcpI = sys.argv[4]
periods = sys.argv[5]
switch = sys.argv[6]
resol = sys.argv[7]
variable = sys.argv[8]

# Clean screen
os.system('cls')

gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK ENSEMBLE, ANOMALIES AND WORLDCLIM  "
print "~~~~~~~~~~~~~~~~~~~~~~\n"

if rcpI == "ALL":
	rcpList = 'rcp26','rcp45','rcp60','rcp85'
else:
	rcpList = rcpI.split(",")

if periods == "ALL":
	periodlist = '2020_2049','2040_2069','2060_2089','2070_2099'
else:
	periodlist = periods.split(",")

if variable == "ALL":
	variablelist = ["bio","prec","tmin","tmax"]
else:
	variablelist = variable.split(",")

if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)		

# worldclim = "S:\observed\gridded_products\worldclim\Global_"+resol	

for rcp in rcpList:		

	modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + resol))

	outFile = dirout + "\\extracts-" + rcp + ".dbf"
	
	if not os.path.exists(outFile):
	
		for model in modellist:
		
			for period in periodlist:

				gp.workspace = dirbase + "\\"  + rcp + "\\global_" + resol + "\\" + model + "\\r1i1p1\\" + period 
				
				print "Extracting ", rcp, model, period
				
				for var in variablelist:
				
					if var == "bio":
						num = 19
					else:
						num = 12
					
					for month in range(1,num + 1, 1):
						
						variable = var + "_" + str(month)
					
						# Set output points
						outPoints = dirout + "\\" + rcp + "_" + model + "_" + period + "_" + variable + ".dbf"
						
						if not gp.Exists(outPoints):
							
							# Sample function
							gp.Sample_sa(variable, mask, outPoints, "")
					
	# Get a list of dbfs 
	dbfList = sorted(glob.glob(dirout + "\\" + rcp + "_*.dbf"))
	
	for dbf in dbfList:
		
		inData = dirout + "\\" + rcp + "_" + model + "_" + period + "_" + variablelist[0] + "_1.dbf"
		
		if not os.path.basename(dbf)[-9:] == variablelist[0] + "_1.dbf":
			
			fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			
			gp.joinfield (inData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
			
			print "\t", os.path.basename(dbf) + " joined"
			
			os.remove(dbf)
	
	# Rename output file
	os.rename(inData, outFile)
							
gp.AddMessage("\n \t ====> DONE!! <====")  