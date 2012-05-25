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

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractValues_WorldClim.py D:\Workspace\Requests\Jvalencia\Ensemble\2040_2069 D:\Workspace\Mexico_SMO\Estaciones_Mexico\Estaciones_PCP.shp D:\Workspace\Requests\Jvalencia\Ensemble\2040_2069\_extract"
	sys.exit(1)
gp.CheckOutExtension("Spatial")
# Arguments
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	
gp.workspace = dirbase 
dsList = gp.ListDatasets("precd*", "all")

for ds in dsList:
	print "Extracting " + ds
	
	ds = gp.workspace + "\\" + os.path.basename(ds)
	InPointsFC = mask 
	OutPointsFC = dirout + "\\" + os.path.basename(ds) + ".dbf"

	gp.CheckOutExtension("Spatial")
	
	if not gp.Exists(OutPointsFC):
		print os.path.basename(OutPointsFC)
		gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")
		# gp.ZonalStatisticsAsTable_sa(mask, "FID", ds, OutPointsFC, "DATA")
	
dbfList = sorted(glob.glob(dirout + "\\precd*.dbf"))
for dbf in dbfList:
	InData = dirout + "\\" + "precd_1.dbf"
	if not os.path.basename(dbf)[-11:] == "precd_1.dbf":
		fields = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
		print dbf + " joined"

# xmlList = sorted(glob.glob(dirout + "\\*.xml"))
# for xml in xmlList:
	# os.remove(xml)
		
# print "\n"

# for month in range (1, 12 + 1, 1):
	# for variable in variablelist:

		# dbf = dirout + "\\_wc_" + variable + "_" + str(month) + ".dbf" 

		# if not os.path.basename(dbf)[-10:] == "tmin_1.dbf":
			# print "\tJoining .. " + os.path.basename(dbf)
			# InData = dirout + "\\_wc_tmin_1.dbf"
			# fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			# gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])

		
print "done"
