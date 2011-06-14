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


gp.workspace = "K:\ClimateData\WorldClim_data\Global_30s"
mask = "D:\Workspace\Bernardo\Point.shp"
dirout = "D:\Workspace\Bernardo\extract"
variables = ["prec", "tmin", "tmax", "tmean"]

#Get a list of grids in the workspace of each folder
dsList = gp.ListDatasets("*", "all")

lista = ""
for ds in dsList:
	print ds

	#Set local variables
	InPointsFC = mask 
	OutPointsFC = dirout + "\\_worldclim_" + ds + ".dbf"

	#Check out Spatial Analyst extension license
	gp.CheckOutExtension("Spatial")

	#Process: Cell Statistics...
	gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")

# Get a list of shps 
dbfList = sorted(glob.glob(dirout + "\\_worldclim*.dbf"))
for dbf in dbfList:
	print dbf
	if not os.path.basename(dbf) == "_worldclim_bio_1.dbf":
		InData = dirout + "\\_worldclim_bio_1.dbf"
		fields = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])	

print "done"
