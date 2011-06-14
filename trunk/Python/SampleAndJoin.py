#--------------------------------------------------
# Description: Extrae valores grids MRI por puntos
# Author: Carlos Navarro
# Actualizado: 25/08/10
#--------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)


os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp.workspace = "W:\ClimateData\WorldClim_data\Global_30s"
mask = "D:\Workspace\Nyando.shp"
variables = ["prec", "tmin", "tmax", "tmean"]

# Get a list of grids in the workspace of each folder
dsList = gp.ListDatasets("*", "all")

lista = ""
for ds in dsList:
	print ds

	#Set local variables
	InPointsFC = mask 
	OutPointsFC = "D:\\Workspace\\Nyando\\" + ds + ".dbf"

	#Check out Spatial Analyst extension license
	gp.CheckOutExtension("Spatial")

	#Process: Cell Statistics...
	gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")

gp.workspace = "D:\Workspace\Nyando"

# Get a list of shps 
for variable in variables:

	for month in range (2, 12 + 1, 1):
	
		print str(variable) + " " + str(month)
		InData = gp.workspace + "\\" + str(variable) + "_1.dbf"
		JoinData = gp.workspace + "\\" + str(variable) + "_" + str(month) + ".dbf"
		gp.joinfield (InData, "mask", JoinData, "mask", str(variable) + "_" + str(month))
		

print "done"
