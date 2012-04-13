#--------------------------------------------------
# Description: Extrae valores grids Worldclim por puntos
# Author: Carlos Navarro
# Actualizado: 18/03/2011
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractValues_WorldClim_v1.py K:\ClimateData\WorldClim_data\Global_30s D:\Workspace\Flora\points_union.shp D:\Workspace\Flora\extract_wc_6 bio"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
variable = sys.argv[4]
	
gp.workspace = dirbase 
# variablelist = "bio", "prec"#, "tmax", "tmean", "tmin"

variable = "bio"
dsList = gp.ListDatasets(variable + "*", "all")

# For a extract by point uncomment this..
for ds in dsList:
	print "Extracting " + ds

	# Set local variables
	InPointsFC = mask 
	OutPointsFC = dirout + "\\_worldclim_" + ds + ".dbf"
	
	# Check out Spatial Analyst extension license
	gp.CheckOutExtension("Spatial")
	
	if not gp.Exists(OutPointsFC):
	# Process
		gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")

#Join dbfs 
if variable == "bio":

	print "\n"
	for month in range (1, 19 + 1, 1):
		
		dbf = dirout + "\\_worldclim_" + variable + "_" + str(month) + ".dbf" 

		if not os.path.basename(dbf)[-10:] == "bio_1.dbf":
			print "\tJoining .. " + os.path.basename(dbf)
			InData = dirout + "\\_worldclim_bio_1.dbf"
			fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])

if variable == "tmean":
	print "\n"
	for month in range (1, 12 + 1, 1):
		
		dbf = dirout + "\\_worldclim_" + variable + "_" + str(month) + ".dbf" 

		if not os.path.basename(dbf)[-11:] == "tmean_1.dbf":
			print "\tJoining .. " + os.path.basename(dbf)
			InData = dirout + "\\_worldclim_tmean_1.dbf"
			fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
else:
	print "\n"
	for month in range (1, 12 + 1, 1):
		dbf = dirout + "\\_worldclim_" + variable + "_" + str(month) + ".dbf" 

		if not os.path.basename(dbf)[-10:] == str(variable + "_1.dbf"):
			print "\tJoining .. " + os.path.basename(dbf)
			InData = dirout + "\\_worldclim_" + variable + "_1.dbf"
			fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])


xmlList = sorted(glob.glob(dirout + "\\*.xml"))
for xml in xmlList:
	os.remove(xml)

checkTXT = open(dirout + "\\_done.txt", "w")
checkTXT.close()
		
print "done"
