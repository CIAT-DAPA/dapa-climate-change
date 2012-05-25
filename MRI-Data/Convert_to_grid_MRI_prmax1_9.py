# Description: Convert ASCII to GRID in a workspace of MRI datasets
# Author: Carlos Navarro
# Date: 26/07/10
# Notes: Get a list of asciis compresses in gz format in the workspace of each folder, and descompress.
#        Converts asciis to GRID format using geoprocessor tools
#        Caution!.. If you choose any of the variables prmax, tmean, wsmax, you must be put str(rs)[44:56] gp.RasterToOtherFormat_conversion

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Mean_Raster_Prec.py <dirbase> <inityear> <finalyear> <outdir> <variable>"
	print "   - ie: python Convert_to_grid_MRI.py K:\MRIData\MRIAAIGrid\SP0A 1984 2003 D:\MRI_grids\prec\SP0A prec"
	sys.exit(1)

dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
dirout = sys.argv[4]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
variable = sys.argv[5]


os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"


for year in range(inityear, finalyear + 1, 1):

	for month in range (1, 12 + 1, 1):

		if month < 10:
			gp.workspace = dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"

			foldersout = dirout + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			if not os.path.exists(foldersout) and os.path.exists(gp.workspace):
				os.system('mkdir ' + foldersout)

			dsList = glob.glob(gp.workspace + "\\" + variable + "*")
			for ds in dsList:
				os.system('7za e -yo' + gp.workspace + " " + ds)
			
			rsList = glob.glob(gp.workspace + "\\" + variable + "*.asc")
			for rs in rsList:
				print rs[44:56]
				gp.RasterToOtherFormat_conversion(rs[44:56],foldersout,"GRID")
				gp.delete_management(rs)


		else:
			gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
			print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + str(month) + "010000"

			foldersout = dirout + "\\OUT_" + str(year) + str(month) + "010000"
			if not os.path.exists(foldersout) and os.path.exists(gp.workspace):
				os.system('mkdir ' + foldersout)

			dsList = glob.glob(gp.workspace + "\\" + variable + "*")
			for ds in dsList:
				os.system('7za e -yo' + gp.workspace + " " + ds)

			rsList = glob.glob(gp.workspace + "\\" + variable + "*.asc")
			for rs in rsList:
				print rs[44:56]
				gp.RasterToOtherFormat_conversion(rs[44:56],foldersout,"GRID")
				gp.delete_management(rs)

	
print "Done!!!!"

	