# Description: Convert ASCII to GRID in a workspace of MRI datasets
# Author: Carlos Navarro
# Date: 26/07/10
# Notes: Get a list of asciis compresses in gz format in the workspace of each folder, and descompress.
#        Converts asciis to GRID format using geoprocessor tools
#        Caution!.. If you choose any of the variables prmax, tmean, wsmax, you must be put str(rs)[44:56] gp.RasterToOtherFormat_conversion

import arcgisscripting, os, sys, glob
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
	
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

yrList = ["198905", "198906", "198907", "198908", "198909", "198910", "198911", "198912", "199001", "199003", "199004", "199005", "199006", "199007", "199008", "199009", "199010", "199011", "199101", "199102", "199103", "199104", "199105", "199106", "199107", "199108", "199109", "199110", "199111", "199112", "199201", "199202", "199203", "199204", "199205", "199206", "199207", "199208", "199209", "199210", "199211", "199212", "199301", "199302", "199303", "199304", "199305", "199306", "199307", "199308", "199309", "199310", "199311", "199312", "199401", "199402", "199403", "199404", "199405", "199406", "199407", "199408", "199409", "199410", "199411", "199412", "199501", "199502", "199505", "199506", "199507", "199508", "199509", "199510", "199511", "199512", "199601", "199602", "199603", "199604", "199605"]
mnList = ["197904", "198005", "198007", "198102", "198108", "198201", "198210", "198302", "198502", "198510", "198512", "198603", "198608", "198610", "198611", "198812", "199002", "199102", "199307", "199503", "199505", "199611", "200106", "200107", "200203", "200204"]

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
		gp.delete_management(OutRaster)
		os.remove(InZip)

# for mn in mnList:

	# gp.workspace = dirbase + "\\OUT_" + str(mn) + "010000"
	# print "\n---> Processing: " + dirbase + "\\OUT_" + str(mn) + "010000"

	# foldersout = dirout + "\\OUT_" + str(mn) + "010000"
	# if not os.path.exists(foldersout) and os.path.exists(gp.workspace):
		# os.system('mkdir ' + foldersout)

	# dsList = glob.glob(gp.workspace + "\\" + variable + "_1*")
	# for ds in dsList:
		# os.system('7za e -yo' + gp.workspace + " " + ds)
	
	# rsList = glob.glob(gp.workspace + "\\" + variable + "_1*.asc")
	# for rs in rsList:
		# print os.path.basename(rs)
		# gp.RasterToOtherFormat_conversion(os.path.basename(rs),foldersout,"GRID")
		# gp.delete_management(rs)

	# rasters = gp.ListRasters(variable + "_1*", "GRID")
	# for raster in rasters:
		# print raster
		# InRaster = gp.workspace + '\\' + raster
		# OutRaster = dirtemp + "\\" + raster
		# InExpression = InRaster + " / 86400"
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		#gp.Times_sa(InRaster, "86400", OutRaster)
		# gp.delete_management(InRaster)
		# gp.CopyRaster_management(OutRaster, InRaster)
		# gp.delete_management(OutRaster)	

	# dsList1 = glob.glob(gp.workspace + "\\" + variable + "_2*")
	# for ds in dsList1:
		# os.system('7za e -yo' + gp.workspace + " " + ds)
	
	# rsList1 = glob.glob(gp.workspace + "\\" + variable + "_2*.asc")
	# for rs in rsList1:
		# print os.path.basename(rs)
		# gp.RasterToOtherFormat_conversion(os.path.basename(rs),foldersout,"GRID")
		# gp.delete_management(rs)

	# rasters1 = gp.ListRasters(variable + "_2*", "GRID")
	# for raster in rasters1:
		# print raster
		# InRaster = gp.workspace + '\\' + raster
		# OutRaster = dirtemp + "\\" + raster
		# InExpression = InRaster + " / 86400"
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		#gp.Times_sa(InRaster, "86400", OutRaster)
		# gp.delete_management(InRaster)
		# gp.CopyRaster_management(OutRaster, InRaster)
		# gp.delete_management(OutRaster)	

	# dsList = glob.glob(gp.workspace + "\\" + variable + "_3*")
	# for ds in dsList:
		# os.system('7za e -yo' + gp.workspace + " " + ds)
	
	# rsList = glob.glob(gp.workspace + "\\" + variable + "_3*.asc")
	# for rs in rsList:
		# print os.path.basename(rs)
		# gp.RasterToOtherFormat_conversion(os.path.basename(rs),foldersout,"GRID")
		# gp.delete_management(rs)

	# rasters = gp.ListRasters(variable + "_3*", "GRID")
	# for raster in rasters:
		# print raster
		# InRaster = gp.workspace + '\\' + raster
		# OutRaster = dirtemp + "\\" + raster
		# InExpression = InRaster + " / 86400"
		# gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		#gp.Times_sa(InRaster, "86400", OutRaster)
		# gp.delete_management(InRaster)
		# gp.CopyRaster_management(OutRaster, InRaster)
		# gp.delete_management(OutRaster)	
		
print "Done!!!!"

	