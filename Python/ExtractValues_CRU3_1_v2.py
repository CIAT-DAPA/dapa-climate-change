#--------------------------------------------------
# Description: Extrae valores CRU por puntos
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

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractValues_CRU3_1_v2.py D:\climate_change\CRU_31 D:\Workspace\Rafael\municipios_centroid2.shp D:\Workspace\Rafael\_extractCRU 2005 2009 1"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
# variable = sys.argv[2]
mask = sys.argv[2]
dirout = sys.argv[3]
inityear = int(sys.argv[4])
finalyear = int(sys.argv[5])
mode = sys.argv[6]

# Modes: 	1. Extract by points and groups by variables
# 			2. Extract by mask
#			3. Extract by points and group for marksim

if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)	
variablelist = "tmn", "tmx", "pre", "tmp"
InPoints = mask 

#Get a list of grids in the workspace of each folder

if mode == "1":
	for year in range (inityear, finalyear + 1, 1):
		for variable in variablelist:
			for month in range (1, 12 + 1, 1):
			
				gp.workspace = dirbase + "\\" + variable
				OutPoints = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
				InRaster = variable + "_" + str(year) + "_" + str(month)		
				
				#Check out Spatial Analyst extension license
				gp.CheckOutExtension("Spatial")

				#Process: Cell Statistics...
				print InRaster
				gp.Sample_sa(InRaster, InPoints, OutPoints, "")
		
		
	for variable in variablelist:
		dbflist = sorted(glob.glob(dirout + "\\" + variable + "*.dbf"))
		for dbf in dbflist:
			if not os.path.basename(dbf) == variable + "_" + str(inityear) + "_1.dbf":
				print "\tJoining .. " + os.path.basename(dbf)
				InData = dirout + "\\" + variable + "_" + str(inityear) + "_1.dbf"
				fields = os.path.basename(dbf)[:-4].split("_")
				if fields[2] == "10" or fields[2] == "11" or fields[2] == "12":
					gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2][:-1])
				else:
					gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2])
				
				os.remove(dbf)
	
	xmlList = sorted(glob.glob(dirout + "\\*.xml"))
	for xml in xmlList:
		os.remove(xml)

if mode == "2":
	for year in range (inityear, finalyear + 1, 1):
		for month in range (1, 12 + 1, 1):
			for variable in variablelist:

				gp.workspace = dirbase + "\\" + variable
				InRaster = variable + "_" + str(year) + "_" + str(month)
				OutRaster = dirout + "\\" + variable + "_" + str(year) + "_" + str(month)				
				
				#Check out Spatial Analyst extension license
				gp.CheckOutExtension("Spatial")

				#Process: Cell Statistics...
				print InRaster
				gp.ExtractByMask_sa(gp.workspace + "\\" + InRaster, mask, OutRaster)

				
if mode == "3":
	for year in range (inityear, finalyear + 1, 1):
		for month in range (1, 12 + 1, 1):
			for variable in variablelist:

				gp.workspace = dirbase + "\\" + variable
				OutPoints = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
				InRaster = variable + "_" + str(year) + "_" + str(month)		
				
				#Check out Spatial Analyst extension license
				gp.CheckOutExtension("Spatial")

				#Process: Cell Statistics...
				print InRaster, InPoints, OutPoints
				gp.Sample_sa(InRaster, InPoints, OutPoints, "")

		
		for month in range (1, 12 + 1, 1):
			for variable in variablelist:
				if not variable == "tmp":
					dbf = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
					if not os.path.basename(dbf) == "tmn_" + str(year) + "_1.dbf":
						print "\tJoining .. " + os.path.basename(dbf)
						InData = dirout + "\\tmn_" + str(year) + "_1.dbf"
						fields = os.path.basename(dbf)[:-4].split("_")
						if fields[2] == "10" or fields[2] == "11" or fields[2] == "12":
							gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2][:-1])
						else:
							gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2])
						
						os.remove(dbf)
	
	xmlList = sorted(glob.glob(dirout + "\\*.xml"))
	for xml in xmlList:
		os.remove(xml)

	# For extract altitude
	# dbfList = sorted(glob.glob(dirout + "\\*.dbf"))
	# for dbf in dbfList:
		# print dbf
		# if not os.path.basename(dbf) == "alt.dbf":
			# InData = dirout + "\\" + "alt.dbf"
			# fields = os.path.basename(dbf)[:-4]
			# gp.joinfield (InData, "mask", dbf, "mask", "alt")	
		
print "done"

print "\n"
