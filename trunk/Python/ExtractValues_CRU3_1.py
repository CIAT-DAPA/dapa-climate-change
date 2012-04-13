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
	print "   - ie: python ExtractValues_CRU3_1.py D:\climate_change\CRU_31 D:\Workspace\samuel_extract\MexicoCorrect.shp D:\Workspace\samuel_extract\MexicoCorrect"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	
gp.workspace = dirbase 
variablelist = "pre", "tmx", "tmp", "tmn"

for variable in variablelist:
	gp.workspace = dirbase + "\\" + variable
	diroutpoints = dirout + "\\" + variable 
	if not os.path.exists(diroutpoints):
		os.system('mkdir ' + diroutpoints)
	
	for year in range (1960, 2009 + 1, 1):
		dsList = gp.ListDatasets(variable + "_" + str(year) + "*", "all")
		for ds in dsList:
			print ds
		
			# For a extract by point uncomment this..
			print "Extracting " + ds

			# Set local variables
			InPointsFC = mask 

			OutPointsFC = diroutpoints + "\\" + ds + ".dbf"

			# Check out Spatial Analyst extension license
			gp.CheckOutExtension("Spatial")

			# Process
			if not gp.Exists(OutPointsFC):
				gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")

	# Get a list of dbfs 
	for year in range (1960, 1979 + 1, 1):
		for month in range (1, 12 + 1, 1):
	# dbfList = sorted(glob.glob(diroutpoints + "\\*.dbf"))
	# for dbf in dbfList:
			dbf = diroutpoints + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
			print os.path.basename(dbf)
			InData = diroutpoints + "\\" + variable + "_1960_1.dbf"
			if not dbf == "InData":
				if month > 9:
					gp.joinfield (InData, "mask", dbf, "mask", os.path.basename(dbf)[:-5])	
				else:
					gp.joinfield (InData, "mask", dbf, "mask", os.path.basename(dbf)[:-4])	

	for year in range (1980, 1999 + 1, 1):
		for month in range (1, 12 + 1, 1):
	# dbfList = sorted(glob.glob(diroutpoints + "\\*.dbf"))
	# for dbf in dbfList:
			dbf = diroutpoints + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
			print os.path.basename(dbf)
			InData = diroutpoints + "\\" + variable + "_1980_1.dbf"
			if not dbf == "InData":
				if month > 9:
					gp.joinfield (InData, "mask", dbf, "mask", os.path.basename(dbf)[:-5])	
				else:
					gp.joinfield (InData, "mask", dbf, "mask", os.path.basename(dbf)[:-4])	
					
	for year in range (2000, 2009 + 1, 1):
		for month in range (1, 12 + 1, 1):
	# dbfList = sorted(glob.glob(diroutpoints + "\\*.dbf"))
	# for dbf in dbfList:
			dbf = diroutpoints + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
			print os.path.basename(dbf)
			InData = diroutpoints + "\\" + variable + "_2000_1.dbf"
			if not dbf == "InData":
				if month > 9:
					gp.joinfield (InData, "mask", dbf, "mask", os.path.basename(dbf)[:-5])	
				else:
					gp.joinfield (InData, "mask", dbf, "mask", os.path.basename(dbf)[:-4])	
# # print "\n"
# # for month in range (1, 12 + 1, 1):
	# # for variable in variablelist:
		# # dbf = dirout + "\\_worldclim_" + variable + "_" + str(month) + ".dbf" 

		# # if not os.path.basename(dbf)[-10:] == "tmin_1.dbf":
			# # print "\tJoining .. " + os.path.basename(dbf)
			# # InData = dirout + "\\_worldclim_tmin_1.dbf"
			# # fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			# # gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
			# # os.remove(dbf)

# # xmlList = sorted(glob.glob(dirout + "\\*.xml"))
# # for xml in xmlList:
	# # os.remove(xml)

# # checkTXT = open(dirout + "\\_done.txt", "w")
# # checkTXT.close()
			
			
# For a extract by mask or clip uncomment this..

# for variable in variablelist:
	# gp.workspace = dirbase + "\\" + variable
	# for year in range (1960, 2009 + 1, 1):
		# dsList = gp.ListDatasets(variable + "_" + str(year) + "*", "all")
		# for ds in dsList:
			# print ds

			#Set local variables
			# InPointsFC = mask 
			# OutRaster = dirout + "\\" + ds

			#Check out Spatial Analyst extension license
			# gp.CheckOutExtension("Spatial")

			#Process
			# gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")
			# X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
			# gp.clip_management(ds,"-75 1 -71 5 ",OutRaster)
			# outTable = dirout + "\\" + ds + ".dbf"
			# gp.ZonalStatisticsAsTable_sa(mask, "NAME_1", ds, outTable, "DATA")
			# For convert to ascii uncomment this..
		
		# diroutascii = dirout + "\\_asciis" 
		
		# if not os.path.exists(diroutascii):
			# os.system('mkdir ' + diroutascii)

		# OutAscii = diroutascii + "\\" + os.path.basename(OutRaster) + ".asc"
		# print "    Converting " + OutAscii
		# gp.RasterToASCII_conversion(OutRaster, OutAscii)
		# InZip = diroutascii + "\\" + os.path.basename(OutRaster).split("_")[0] + "_asc.zip"
		# os.system('7za a ' + InZip + " " + OutAscii)
		# os.remove(OutAscii)
		# gp.delete_management(OutRaster)
		
print "done"
