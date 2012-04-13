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
	print "   - ie: python ExtractValues_WorldClim.py K:\ClimateData\WorldClim_data\Global_30s D:\Workspace\jramirez\lushoto.shp D:\Workspace\Flora\extract_wc_8"
	sys.exit(1)
gp.CheckOutExtension("Spatial")
# Arguments
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	
gp.workspace = dirbase 
dsList = gp.ListDatasets("*", "all")

for ds in dsList:
	print "Extracting " + ds
	
	ds = gp.workspace + "\\" + os.path.basename(ds)
	InPointsFC = mask 
	OutPointsFC = dirout + "\\" + os.path.basename(ds) + ".dbf"

	gp.CheckOutExtension("Spatial")
	
	if not gp.Exists(OutPointsFC):
		print os.path.basename(OutPointsFC)
		gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")
	

dbfList = sorted(glob.glob(dirout + "\\*.dbf"))
for dbf in dbfList:
	InData = dirout + "\\" + "bio_1.dbf"
	if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
		fields = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
		print dbf + " joined"

		
# print "\n"

# for month in range (1, 12 + 1, 1):
	# for variable in variablelist:

		# dbf = dirout + "\\_wc_" + variable + "_" + str(month) + ".dbf" 

		# if not os.path.basename(dbf)[-10:] == "tmin_1.dbf":
			# print "\tJoining .. " + os.path.basename(dbf)
			# InData = dirout + "\\_wc_tmin_1.dbf"
			# fields = os.path.basename(dbf)[:-4].split("_")[-2:]
			# gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])


# xmlList = sorted(glob.glob(dirout + "\\*.xml"))
# for xml in xmlList:
	# os.remove(xml)

# checkTXT = open(dirout + "\\_done.txt", "w")
# checkTXT.close()
			
			
# For a extract by mask or clip uncomment this..
# dsList = sorted(gp.ListRasters("bio*", "GRID"))

# for ds in dsList:
	# print ds

	# #Set local variables
	# # InPointsFC = mask 
	# OutRaster = dirout + "\\" + ds

	# #Check out Spatial Analyst extension license
	# gp.CheckOutExtension("Spatial")

	# #Process
	# # gp.Sample_sa(ds, InPointsFC, OutPointsFC, "")
	# # X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
	# # gp.clip_management(ds,"79 24 89 32 ",OutRaster)
	# gp.ExtractByMask_sa(ds, mask, OutRaster)
	
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
