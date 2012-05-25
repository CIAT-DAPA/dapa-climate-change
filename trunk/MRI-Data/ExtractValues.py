#--------------------------------------------------
# Description: Extrae valores grids MRI por puntos
# Author: Carlos Navarro
# Actualizado: 25/08/10
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
<<<<<<< .mine
	print "   - ie: python ExtractValues.py K:\MRIData\MRI_grids\SP0A E:\Workspace\MRI\Palmira E:\Workspace\MRI\points\palmira.shp"
=======
	print "   - ie: python ExtractValues.py K:\MRIData\MRI_grids\SP0A D:\Workspace\MRI\Planaltina D:\Workspace\MRI\points\planaltina.shp 1984"
>>>>>>> .r1395
	sys.exit(1)
	
dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
mask = sys.argv[3]
year = int(sys.argv[4])
# finalyear = int(sys.argv[5])
# variable = sys.argv[6]
# yearlist = "1986", "1990", "1997"
variablelist = "tmin", "tmax", "prec" 
os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")


# for variable in variablelist:
	# print variable
	# dirbasevar = dirbase + "\\" + variable
	# diroutgrid = dirout + "\\" + variable
	# if not os.path.exists(diroutgrid):
		# os.system('mkdir ' + diroutgrid)

	# # for year in yearlist:
	# for month in range (1, 12 + 1, 1):
		# if month < 10:
			# gp.workspace = dirbasevar + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			# dsList = gp.ListDatasets("*", "all")
			# lista = ""
			# # ds = variable + "_" + str(month
			# for ds in dsList:
				
				# OutPointsFC = diroutgrid + "\\" + str(year) + "0" + str(month) + ds[-2:] + ".dbf"
				# if not gp.Exists(OutPointsFC):
				# # Set local variables
					
					# gp.Sample_sa(ds, mask, OutPointsFC, "")
				
		# else:
			# gp.workspace = dirbasevar + "\\OUT_" + str(year) + str(month) + "010000"
			# dsList = gp.ListDatasets("*", "all")
			# lista = ""
			# for ds in dsList:
				# print ds
				# OutPointsFC = diroutgrid + "\\" + str(year) + str(month) + ds[-2:] + ".dbf"
				# # Set local variables
				# if not gp.Exists(OutPointsFC):
					# gp.Sample_sa(ds, mask, OutPointsFC, "")

for variable in variablelist:
	print variable
# variable = "prec"
	dirbase = "D:\\Workspace\\MRI\\Planaltina\\" + variable
	dbfList = sorted(glob.glob(dirbase + "\\1984*.dbf"))
	for dbf in dbfList[0:250]:
		print dbf
		# print os.path.basename(dbf)
		if not os.path.basename(dbf) == "19840101.dbf":
			InData = dirbase + "\\19840101.dbf"
			JoinData = dirbase + "\\" + os.path.basename(dbf)
			# print os.path.basename(dbf)[-6:-4]
			gp.joinfield (InData, "mask", JoinData, "mask", variable + "_" + str(os.path.basename(dbf)[-6:-4]))
				
	for dbf in dbfList[250:]:
		print dbf
		# print os.path.basename(dbf)
		if not os.path.basename(dbf) == "19840908.dbf":
			InData = dirbase + "\\19840908.dbf"
			JoinData = dirbase + "\\" + os.path.basename(dbf)
			# print os.path.basename(dbf)[-6:-4]
			gp.joinfield (InData, "mask", JoinData, "mask", variable + "_" + str(os.path.basename(dbf)[-6:-4]))
				

print "done"
