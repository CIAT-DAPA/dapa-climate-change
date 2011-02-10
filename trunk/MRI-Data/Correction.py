# Description: Convert ASCII to GRID in a workspace of MRI datasets
# Author: Carlos Navarro
# Date: 26/07/10
# Notes: Get a list of asciis compresses in gz format in the workspace of each folder, and descompress.
#        Converts asciis to GRID format using geoprocessor tools
#        Caution!.. If you choose any of the variables prmax, tmean, wsmax, you must be put str(rs)[44:56] gp.RasterToOtherFormat_conversion

import arcgisscripting, os, sys, glob, shutil
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Mean_Raster_Prec.py <dirbase> <inityear> <finalyear> <outdir> <variable>"
	print "   - ie: python Correction.py W:\MRIData\MRIAAIGrid\SP0A 1979 2003 prmax D:\MRI_grids\temp D:\MRI_grids\temp2"
	sys.exit(1)

dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]
dirtemp = sys.argv[5]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
dirtemp2 = sys.argv[6]
if not os.path.exists(dirtemp2):
	os.system('mkdir ' + dirtemp2)

gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

yrList = ["199404", "199405", "199406", "199407", "199408", "199409", "199410", "199411", "199412", "199501", "199502", "199505", "199506", "199507", "199508", "199509", "199510", "199511", "199512", "199601", "199602", "199603", "199604"]
mnList = ["197904", "198005", "198007", "198102", "198108", "198201", "198210", "198502", "198510", "198512", "198608", "198610", "199102", "199307", "199505", "199611", "200107", "200203", "200204"]

for yr in yrList:

	gp.workspace = dirbase + "\\OUT_" + str(yr) + "010000"
	print "\n---> Processing: " + dirbase + "\\OUT_" + str(yr) + "010000"

	dsList = glob.glob(gp.workspace + "\\" + variable + "_0*")
	for ds in dsList:
		os.system('7za e -yo' + gp.workspace + " " + ds)
	
	rsList = glob.glob(gp.workspace + "\\" + variable + "_0*.asc")
	for rs in rsList:
		print os.path.basename(rs)
		gp.RasterToOtherFormat_conversion(os.path.basename(rs), dirtemp, "GRID")
		gp.delete_management(rs)

	gp.workspace = dirtemp 
	rasters = gp.ListRasters(variable + "_0*", "GRID")
	for raster in rasters:
		print raster
		InRaster = gp.workspace + "\\" + raster
		OutRaster = dirtemp2 + "\\" + raster
		InExpression = InRaster + " / 86400"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		##gp.Times_sa(InRaster, "86400", OutRaster)
		gp.delete_management(InRaster)
		
		OutAscii = dirtemp2 + "\\" + raster + ".asc"
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		InZip = dirtemp2 + "\\" + raster + ".asc.gz"
		os.system('7za a ' + InZip + " " + OutAscii)
		gp.delete_management(OutRaster)

		OutZip = dirbase + "\\OUT_" + str(yr) + "010000" + "\\" + raster + ".asc.gz"
		if os.path.isfile(OutZip):
			os.remove(OutZip)
		shutil.copyfile(InZip, OutZip)
		os.remove(OutAscii)
		os.remove(InZip)

for mn in mnList:

	gp.workspace = dirbase + "\\OUT_" + str(mn) + "010000"
	print "\n---> Processing: " + dirbase + "\\OUT_" + str(mn) + "010000"

	dsList1 = glob.glob(gp.workspace + "\\" + variable + "_1*")
	for ds in dsList1:
		os.system('7za e -yo' + gp.workspace + " " + ds)
	
	rsList1 = glob.glob(gp.workspace + "\\" + variable + "_1*.asc")
	for rs in rsList1:
		print os.path.basename(rs)
		gp.RasterToOtherFormat_conversion(os.path.basename(rs), dirtemp, "GRID")
		gp.delete_management(rs)

	dsList2 = glob.glob(gp.workspace + "\\" + variable + "_2*")
	for ds in dsList2:
		os.system('7za e -yo' + gp.workspace + " " + ds)
	
	rsList2 = glob.glob(gp.workspace + "\\" + variable + "_2*.asc")
	for rs in rsList2:
		print os.path.basename(rs)
		gp.RasterToOtherFormat_conversion(os.path.basename(rs), dirtemp, "GRID")
		gp.delete_management(rs)

	dsList3 = glob.glob(gp.workspace + "\\" + variable + "_3*")
	for ds in dsList3:
		os.system('7za e -yo' + gp.workspace + " " + ds)
	
	rsList3 = glob.glob(gp.workspace + "\\" + variable + "_3*.asc")
	for rs in rsList3:
		print os.path.basename(rs)
		gp.RasterToOtherFormat_conversion(os.path.basename(rs), dirtemp, "GRID")
		gp.delete_management(rs)
		
	gp.workspace = dirtemp 
	rasters1 = gp.ListRasters(variable + "_1*", "GRID")
	for raster in rasters1:
		print raster
		InRaster = gp.workspace + "\\" + raster
		OutRaster = dirtemp2 + "\\" + raster
		InExpression = InRaster + " / 86400"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		##gp.Times_sa(InRaster, "86400", OutRaster)
		gp.delete_management(InRaster)
		
		OutAscii = dirtemp2 + "\\" + raster + ".asc"
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		InZip = dirtemp2 + "\\" + raster + ".asc.gz"
		os.system('7za a ' + InZip + " " + OutAscii)
		gp.delete_management(OutRaster)

		OutZip = dirbase + "\\OUT_" + str(mn) + "010000" + "\\" + raster + ".asc.gz"
		if os.path.isfile(OutZip):
			os.remove(OutZip)
		shutil.copyfile(InZip, OutZip)
		os.remove(OutAscii)
		os.remove(InZip)

	gp.workspace = dirtemp 
	rasters2 = gp.ListRasters(variable + "_2*", "GRID")
	for raster in rasters2:
		print raster
		InRaster = gp.workspace + "\\" + raster
		OutRaster = dirtemp2 + "\\" + raster
		InExpression = InRaster + " / 86400"
		gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		##gp.Times_sa(InRaster, "86400", OutRaster)
		gp.delete_management(InRaster)
		
		OutAscii = dirtemp2 + "\\" + raster + ".asc"
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		InZip = dirtemp2 + "\\" + raster + ".asc.gz"
		os.system('7za a ' + InZip + " " + OutAscii)
		gp.delete_management(OutRaster)

		OutZip = dirbase + "\\OUT_" + str(mn) + "010000" + "\\" + raster + ".asc.gz"
		if os.path.isfile(OutZip):
			os.remove(OutZip)
		shutil.copyfile(InZip, OutZip)
		os.remove(OutAscii)
		os.remove(InZip)

	gp.workspace = dirtemp 
	rasters3 = gp.ListRasters(variable + "_3*", "GRID")
	for raster in rasters3:
		print raster
		InRaster = gp.workspace + "\\" + raster
		OutRaster = dirtemp2 + "\\" + raster
		InExpression = InRaster + " / 86400"
		if gp.exists(raster):
			gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
			##gp.Times_sa(InRaster, "86400", OutRaster)
			gp.delete_management(InRaster)
			
			OutAscii = dirtemp2 + "\\" + raster + ".asc"
			gp.RasterToASCII_conversion(OutRaster, OutAscii)
			InZip = dirtemp2 + "\\" + raster + ".asc.gz"
			os.system('7za a ' + InZip + " " + OutAscii)
			gp.delete_management(OutRaster)

			OutZip = dirbase + "\\OUT_" + str(mn) + "010000" + "\\" + raster + ".asc.gz"
			if os.path.isfile(OutZip):
				os.remove(OutZip)
			shutil.copyfile(InZip, OutZip)
			os.remove(OutAscii)
			os.remove(InZip)

print "Done!!!!"

	