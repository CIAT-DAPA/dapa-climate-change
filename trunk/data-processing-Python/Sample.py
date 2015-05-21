#--------------------------------------------------
# Description: Extrae valores grids MRI por puntos
# Author: Carlos Navarro
# Actualizado: 25/08/10
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)


os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp.workspace = "D:\\CIAT\\Articles\\lat-maxent-coffe\\raw_climate_data\\Baseline"
mask = "D:\\CIAT\\Articles\\lat-maxent-coffe\\occurrence_files\\occurrence_files\\coffea_arabica.shp"
# variables = ["prec", "tmin", "tmax", "tmean"]
print mask
# Get a list of grids in the workspace of each folder
dsList = gp.ListRasters("*", "ALL")
# dsList = glob.glob(gp.workspace + "\\*.asc")
print dsList
lista = ""
for ds in dsList:
	print ds

	#Set local variables
	InPointsFC = mask 
	OutPointsFC = gp.workspace + "\\" + ds + ".dbf"

	#Check out Spatial Analyst extension license
	gp.CheckOutExtension("Spatial")

	#Process: Cell Statistics...
	gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")

# gp.workspace = "D:\Workspace\Nyando"

# # Get a list of shps 
# for variable in variables:

	# for month in range (2, 12 + 1, 1):
	
		# print str(variable) + " " + str(month)
		# InData = gp.workspace + "\\" + str(variable) + "_1.dbf"
		# JoinData = gp.workspace + "\\" + str(variable) + "_" + str(month) + ".dbf"
		# gp.joinfield (InData, "mask", JoinData, "mask", str(variable) + "_" + str(month))
		

print "done"
