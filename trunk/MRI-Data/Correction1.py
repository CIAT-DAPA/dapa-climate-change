# Description: Convert ASCII to GRID in a workspace of MRI datasets
# Author: Carlos Navarro
# Date: 25/02/11
# Notes: Get a list of asciis compresses in gz format in the workspace of each folder, and descompress.
#        Converts asciis to GRID format using geoprocessor tools
#        Caution!.. If you choose any of the variables prmax, tmean, wsmax, you must be put str(rs)[44:56] gp.RasterToOtherFormat_conversion

import arcgisscripting, os, sys, glob, shutil, os.path, string
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Mean_Raster_Prec.py <dirbase> <inityear> <finalyear> <outdir> <variable>"
	print "   - ie: python Correction1.py K:\MRIData\MRIAAIGrid\tmp\SN0A prmax E:\MRI_grids\temp2 E:\MRIData\MRIAAIGrid\SN0A"
	sys.exit(1)

dirbase =sys.argv[1]
# inityear = int(sys.argv[2])
# finalyear = int(sys.argv[3])
variable = sys.argv[2]
dirtemp = sys.argv[3]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
outdir = sys.argv[4]
if not os.path.exists(outdir):
	os.system('mkdir ' + outdir)
	
gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

if os.path.isfile(outdir + "\\desc_" + variable + ".txt"):
    outFile = open(outdir + "\\desc_" + variable + ".txt", "a")
else:
    outFile = open(outdir + "\\desc_" + variable + ".txt", "w")

outFile.write("DATE" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")


#yrList = ["201610", "201704", "201903", "202304", "202502", "202505", "203203", "203402", "203911"]
#yrList = ["203902", "203905"]
yrList = ["201601", "202607", "202705", "203201"]

for yr in yrList:

	gp.workspace = dirbase + "\\OUT_" + str(yr) + "010000"
	print "\n---> Processing: " + dirbase + "\\OUT_" + str(yr) + "010000"
	
	dirtemp2 = outdir + "\\" + "\\OUT_" + str(yr) + "010000"
	if not os.path.exists(dirtemp2):
		os.system('mkdir ' + dirtemp2)
	
	dsList = glob.glob(gp.workspace + "\\" + variable + "_*")
	for ds in dsList:
		os.system('7za e -yo' + gp.workspace + " " + ds)
	
	
	rsList = glob.glob(gp.workspace + "\\" + variable + "_*.asc")
	for rs in rsList:
		print os.path.basename(rs)
		gp.RasterToOtherFormat_conversion(os.path.basename(rs), dirtemp, "GRID")
		gp.delete_management(rs)

	gp.workspace = dirtemp 
	rasters = gp.ListRasters(variable + "_*", "GRID")
	for raster in rasters:
		print raster
		InRaster = gp.workspace + "\\" + raster
		OutRaster = dirtemp2 + "\\" + raster
		InExpression = InRaster + " * 86400"
		##gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
		gp.Times_sa(InRaster, "86400", OutRaster)
		gp.delete_management(InRaster)
		
		MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
		MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
		MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
		STD = gp.GetRasterProperties_management(OutRaster, "STD")
		CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
		outFile = open(outdir + "\\desc_" + variable + ".txt", "a")
		outFile.write(str(yr) + "\t" + OutRaster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")
				
		OutAscii = dirtemp2 + "\\" + raster + ".asc"
		gp.RasterToASCII_conversion(OutRaster, OutAscii)
		InZip = dirtemp2 + "\\" + raster + ".asc.gz"
		os.system('7za a ' + InZip + " " + OutAscii)
		gp.delete_management(OutRaster)
		

		# OutZip = dirbase + "\\OUT_" + str(yr) + "010000" + "\\" + raster + ".asc.gz"
		# if os.path.isfile(OutZip):
			# os.remove(OutZip)
		# shutil.copyfile(InZip, OutZip)
		# os.remove(OutAscii)
		# os.remove(InZip)


print "Done!!!!"

	