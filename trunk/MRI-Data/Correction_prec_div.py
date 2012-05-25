#-----------------------------------------------------------------------
# Description: Correccion precipitacion datos MRI
# Author: Carlos Navarro
# Date: 26/07/10
# Notes: Multiplica por 86400 los datos prec de los dias 1 a 9
#-----------------------------------------------------------------------

import arcgisscripting, os, sys, glob, shutil
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Correction_prec_div.py B:\prmax\SN0A\OUT_201709010000 E:\MRI_grids\tmp prmax K:\MRIData\MRIAAIGrid\SN0A\OUT_201709010000 10 23"
	sys.exit(1)

dirbase = sys.argv[1]
dirtemp = sys.argv[2]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
variable = sys.argv[3]
dirout = sys.argv[4]
inday = int(sys.argv[5])
finalday = int(sys.argv[6])

gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CORRECTION GRIDS     "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

gp.workspace = dirbase
print "\n---> Processing: " + gp.workspace

for day in range(inday, finalday + 1, 1):

	raster = str(variable) + "_" + str(day)
	print raster
	
	InRaster = gp.workspace + '\\' + raster
	OutRaster = dirtemp + "\\" + raster
	InExpression = InRaster + " / 86400"
	##gp.SingleOutputMapAlgebra_sa(InExpression, OutRaster)
	gp.Times_sa(InRaster, "86400", OutRaster)
	print "divided"
	
	OutAscii = dirtemp + "\\" + raster + ".asc"
	gp.RasterToASCII_conversion(OutRaster, OutAscii)
	InZip = dirtemp + "\\" + raster + ".asc.gz"
	os.system('7za a ' + InZip + " " + OutAscii)
	print "convert and compressed"

	OutZip = dirout + "\\" + raster + ".asc.gz"
	if os.path.isfile(OutZip):
		os.remove(OutZip)
	shutil.copyfile(InZip, OutZip)
	print "replaced"
	os.remove(InZip)
	os.remove(OutAscii)
	gp.delete(OutRaster)

print "Done!!!!"
