#-----------------------------------------------------------
# Description: Promedia los grids temperatura de los datos MRI
# Author: Carlos Navarro
# Date: 06/09/10
#-----------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
<<<<<<< .mine
	print "   - ie: python Mean_temp_MRI.py K:\MRIData\MRI_grids\SP0A 1979 2003 tmin E:\workspace\MRI\Global_5min"
=======
	print "   - ie: python Mean_temp_MRI.py K:\MRIData\MRI_grids\SP0A 1979 2003 tmin D:\Workspace\MRI"
>>>>>>> .r1395
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]
outdir = sys.argv[5]

dirout = outdir + "\\" + variable + "_monthly"
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	
# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

gp.OverWriteOutput = 1

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~"
print "   MEAN GRIDS   "
print "~~~~~~~~~~~~~~~~"
print "\n"

dirbase = "E:\\workspace\\MRI\\tmin_monthly"
for month in range (1, 12 + 1, 1):
	
	lista = ""
	for year in range(inityear, finalyear + 1, 1):
		if month < 10:
			ds = dirbase + "\\tmin_" + str(year) + "0" + str(month)
			print ds
		if month > 9:
			ds = dirbase + "\\tmin_" + str(year) + str(month)
		lista = lista + ';' + ds 
	LISTA = "\"" + lista[1:] + "\""
	print LISTA
	OutRaster = outdir + "\\tmin_" + str(month)
	gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
		# # Process: Cell Statistics...
		# print "\t ..averaging"
	
		# gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")	
	

		
			# # gp.workspace = dirbase + "\\" + variable + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			# # print "--->...processing : " + dirbase + "\\" + variable + "\\OUT_" + str(year) + "0" + str(month)
			# OutRaster = dirout + "\\" + variable + "_" + str(year) + "0" + str(month)
		
		# if month > 9:
			# # gp.workspace = dirbase + "\\" + variable + "\\OUT_" + str(year) + str(month) + "010000"
			# # print "--->... processing:" + dirbase + "\\" + variable + "\\OUT_" + str(year) + str(month)
			# OutRaster = dirout + "\\" + variable + "_" + str(year) + str(month)
		
		# if not gp.Exists(OutRaster):
			# try:
				# # Get a list of grids in the workspace of each folder
				# dsList = gp.ListDatasets(variable + "*", "all")

<<<<<<< .mine
				# print "\t ..done!!"
=======
		if month < 10 and gp.Exists(dirbase + "\\" + variable + "\\OUT_" + str(year) + "0" + str(month) + "010000"):
			gp.workspace = dirbase + "\\" + variable + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			print "--->...processing : " + dirbase + "\\" + variable + "\\OUT_" + str(year) + "0" + str(month)
>>>>>>> .r1395

<<<<<<< .mine
			# except:
				# print "No calculated!!!"
print "Done!!!!"=======
			# Get a list of grids in the workspace of each folder
			print "\t ..listing grids"
			dsList = gp.ListDatasets(variable + "*", "all")
			lista = ""
			for ds in dsList:
				lista = lista + ';' + ds 
			LISTA = "\"" + lista[1:] + "\""
			print LISTA
			OutRaster = dirout + "\\" + variable + "_" + str(year) + "0" + str(month)
			
			# Process: Cell Statistics...
			print "\t ..averaging"
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
			print "\t ..done!!"

		if month > 9 and gp.Exists(dirbase + "\\" + variable + "\\OUT_" + str(year) + str(month) + "010000"):
			gp.workspace = dirbase + "\\" + variable + "\\OUT_" + str(year) + str(month) + "010000"
			print "--->... processing:" + dirbase + "\\" + variable + "\\OUT_" + str(year) + str(month)

			# Get a list of grids in the workspace of each folder
			print "\t ..listing grids"
			dsList = gp.ListDatasets(variable + "*", "all")
			lista = ""
			for ds in dsList:
				lista = lista + ';' + ds 
			LISTA = "\"" + lista[1:] + "\""
			print LISTA
			OutRaster = dirout + "\\" + variable + "_" + str(year) + str(month)

			# Process: Cell Statistics...
			print "\t ..averaging"
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
			print "\t ..done!!"

print "Done!!!!"
>>>>>>> .r1395
