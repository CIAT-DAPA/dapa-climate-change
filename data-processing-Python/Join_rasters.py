# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Updated: July 28th, 2014
# Purpose: Purpose: Cut by mask, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------
import arcpy, os, sys, string, glob, shutil
from arcpy import env


# python Join_rasters.py T:\data\gcm\cmip5 F:\jetarapues rcp26 30s bcc_csm1_1 2020_2049 bio

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
resolution = sys.argv[4]
models = sys.argv[5]
periods = sys.argv[6]
variable = sys.argv[7]

# Clean screen
os.system('cls')

dataset = "downscaled"

if not os.path.exists(dirout + "\\" + dataset + "\\" + sres):
	os.system('mkdir ' + dirout + "\\" + dataset + "\\" + sres)
	

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of periods
if periods == "ALL":
	periodlist = '2020_2049','2040_2069','2060_2089','2070_2099'
else:
	periodlist = periods.split(",")
	
#Get lists of models
if models == "ALL":
	modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\" + sres + "\\Global_" + str(resolution) ))
else: 
	modellist = models.split(",")
#Get lists of data
if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")
	
print "Models: " + str(modellist)
print "Periods: " + str(periodlist)
print "Variables: " + str(variablelist)	

	
for model in modellist:
	# Looping around periods
	for period in periodlist:
	
		env.workspace = dirbase + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period

		if os.path.exists(env.workspace) and not os.path.exists(dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "_extract_" + period + "_done.txt"):
			print "\n---> Processing: "  + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period + "\n"
			diroutraster = dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period

			if not os.path.exists(diroutraster):
				os.system('mkdir ' + diroutraster)
				
			if variable == "ALL":
				var = "*"
			else:	
				var = variable + "*"
				
			for variable in variablelist:
				if variable == "bio":
					num = 19
				else:
					num = 12
				for month in range (1, num + 1, 1):
					if variable == "cons_mths":
						raster = env.workspace + "\\" + variable
					else:
						raster = env.workspace + "\\" + variable + "_" + str(month)

					OutRaster = diroutraster + "\\" + variable
					
					if not arcpy.Exists(OutRaster):
						arcpy.CopyRaster_management(variable+ "_1",OutRaster,"#","#","#","NONE","NONE","#")
						arcpy.DeleteField_management(OutRaster,"COUNT")						
						print "    Copied " + variable+ "_1"
						
					if variable + "_" + str(month)!=variable+ "_1":
						arcpy.JoinField_management(OutRaster,"Rowid",variable + "_" + str(month),"Rowid","VALUE")
						print "    Join " + os.path.basename(raster)

			print " Done!!"
			checkTXT = open(dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "_extract_" + period + "_done.txt", "w")
			checkTXT.close()
			
		else:
			print "The model " + model + " " + period + " is already processed" 
			print "Processing the next period \n"
