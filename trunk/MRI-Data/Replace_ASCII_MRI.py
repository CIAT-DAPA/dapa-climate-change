#-----------------------------------------------------------------------
# Description: Reemplaza los archivos ASCII por los nuevos corregidos
# Author: Carlos Navarro
# Date: 02/09/10
#-----------------------------------------------------------------------

import arcgisscripting, os, sys, glob, shutil
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "-ie: python Replace_ASCII_MRI.py F:\MRI_grids\prec\SN0A K:\MRIData\MRIAAIGrid\SN0A F:\MRI_grids\tmp\prec\SN0A 1979 2003 prec"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
dirtemp = sys.argv[3]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
inityear = int(sys.argv[4])
finalyear = int(sys.argv[5])
variable = sys.argv[6]

gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "      REPLACE ASCIIS     "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"


for year in range(inityear, finalyear + 1, 1):
    for month in range (1, 12 + 1, 1):
        if month < 10:
            gp.workspace = dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            
            rasters = gp.ListRasters(variable + "_0*", "GRID")
            for raster in rasters:
                print raster
                InRaster = gp.workspace + '\\' + raster
                OutRaster = dirtemp + "\\" + raster + ".asc"
                gp.RasterToASCII_conversion(InRaster, OutRaster)
##                gp.delete_management(InRaster)
##                InZip = dirtemp + "\\" + raster + ".zip"
##                os.system('7za a' + InZip + " " + OutRaster)
##                OutZip = gp.workspace + '\\' + raster + ".zip"
##                shutil.copyfile(InZip, OutZip)
##                gp.delete_management(OutRaster)
##                gp.delete_management(InZip)
                
        else:
            gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            
            rasters = gp.ListRasters(variable + "_0*", "GRID")
            for raster in rasters:
                print raster
                InRaster = gp.workspace + '\\' + raster
                OutRaster = dirtemp + "\\" + raster + ".asc"
                gp.RasterToASCII_conversion(InRaster, OutRaster)
##                gp.delete_management(InRaster)
##                InZip = dirtemp + "\\" + raster + ".zip"
##                os.system('7za a' + InZip + " " + OutRaster)
##                OutZip = gp.workspace + '\\' + raster + ".zip"
##                shutil.copyfile(InZip, OutZip)
##                gp.delete_management(OutRaster)
##                gp.delete_management(InZip)

print "Done!!!!"
