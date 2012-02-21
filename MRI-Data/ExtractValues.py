#--------------------------------------------------
# Description: Extrae valores grids MRI por puntos
# Author: Carlos Navarro
# Actualizado: 25/08/10
#--------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractValues.py K:\MRIData\MRI_grids\SP0A\prec D:\Workspace\MRI\Palmira D:\Workspace\MRI\points\palmira.shp"
	sys.exit(1)
	
dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
mask = sys.argv[3]
# inityear = int(sys.argv[4])
# finalyear = int(sys.argv[5])
# variable = sys.argv[6]
yearlist = "1986", "1990", "1997"
variablelist = "tmin", "tmax", "prec" 
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

for variable in variablelist:
	print variable
	dirbasevar = dirbase + "\\" + variable
	diroutgrid = dirout + "\\" + variable
	if not os.path.exists(diroutgrid):
		os.system('mkdir ' + diroutgrid)

	for year in yearlist:
		for month in range (1, 12 + 1, 1):
			if month < 10:
				gp.workspace = dirbasevar + "\\OUT_" + str(year) + "0" + str(month) + "010000"
				dsList = gp.ListDatasets("*", "all")
				lista = ""
				for ds in dsList:
					print ds
					OutPointsFC = diroutgrid + "\\" + str(year) + "0" + str(month) + ds[-2:]
					if not gp.Exists(OutPointsFC):
					# Set local variables
						gp.ExtractValuesToPoints_sa(mask, ds, OutPointsFC, "NONE", "VALUE_ONLY")
					
			else:
				gp.workspace = dirbasevar + "\\OUT_" + str(year) + str(month) + "010000"
				dsList = gp.ListDatasets("*", "all")
				lista = ""
				for ds in dsList:
					print ds
					OutPointsFC = diroutgrid + "\\" + str(year) + str(month) + ds[-2:]
					# Set local variables
					if not gp.Exists(OutPointsFC):
						gp.ExtractValuesToPoints_sa(mask, ds, OutPointsFC, "NONE", "VALUE_ONLY")

	# for year in yearlist:

	# for month in range (2, 12 + 1, 1):

		# if month < 10:
			# InFeature = variable + "_" + str(year) + "0" + str(month) + ".shp"
			# InData = dirbase + "\\" + variable + "_" + str(year) + "01.shp"
			# print "---> Joining " + variable + "_" + str(year)
			# gp.joinfield (InData, "STATION_ID", InFeature, "STATION_ID", "RASTERVALU")
			# print InFeature + " Joined!"

		# else:
			# InFeature = variable + "_" + str(year) + str(month) + ".shp"
			# InData = dirbase + "\\" + variable + "_" + str(year) + "01.shp"
			# print "---> Joining " + variable + "_" + str(year)
			# gp.joinfield (InData, "STATION_ID", InFeature, "STATION_ID", "RASTERVALU")
			# print InFeature + " Joined!"
print "done"
