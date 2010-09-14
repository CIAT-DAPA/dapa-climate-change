#-----------------------------------------------------------------------
# Description: Reemplaza los archivos ASCII por los nuevos corregidos
# Author: Carlos Navarro
# Date: 02/09/10
#-----------------------------------------------------------------------

import arcgisscripting, os, sys, shutil
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "-ie: python Replace_ASCII_MRI.py G:\MRI_grids K:\MRIData\MRIAAIGrid 2015 2039 prec SN0A"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
inityear = int(sys.argv[3])
finalyear = int(sys.argv[4])
variable = sys.argv[5]
scenario = sys.argv[6]

gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "      REPLACE ASCIIS     "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"


for year in range(inityear, finalyear + 1, 1):
    for month in range (1, 12 + 1, 1):
        if month < 10 and gp.Exists(dirout + "\\" + scenario + "\\OUT_" + str(year) + "0" + str(month) + "010000"):
            gp.workspace = dirbase + "\\" + variable + "\\" + scenario + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\" + variable + "\\" + scenario + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            
            rasters = gp.ListRasters(variable + "_0*", "GRID")
            for raster in rasters:
                print raster
                InRaster = gp.workspace + '\\' + raster
                OutRaster = gp.workspace + "\\" + raster + ".asc"
                gp.RasterToASCII_conversion(InRaster, OutRaster)
                InZip = gp.workspace + "\\" + raster + ".asc.gz"
                os.system('7za a ' + InZip + " " + OutRaster)
                OutZip = dirout + "\\" + scenario + "\\OUT_" + str(year) + "0" + str(month) + "010000" + "\\" + raster + ".asc.gz"
                if os.path.isfile(OutZip):
					os.remove(OutZip)
                shutil.copyfile(InZip, OutZip)
                gp.delete_management(OutRaster)
                os.remove(InZip)
                
        if month > 9 and gp.Exists(dirout + "\\" + scenario + "\\OUT_" + str(year) + str(month) + "010000"):
            gp.workspace = dirbase + "\\" + variable + "\\" + scenario + "\\OUT_" + str(year) + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\" + variable + "\\" + scenario + "\\OUT_" + str(year) + str(month) + "010000"
            
            rasters = gp.ListRasters(variable + "_0*", "GRID")
            for raster in rasters:
                print raster
                InRaster = gp.workspace + '\\' + raster
                OutRaster = gp.workspace + "\\" + raster + ".asc"
                gp.RasterToASCII_conversion(InRaster, OutRaster)
                InZip = gp.workspace + "\\" + raster + ".asc.gz"
                os.system('7za a ' + InZip + " " + OutRaster)
                OutZip = dirout + "\\" + scenario + "\\OUT_" + str(year) + str(month) + "010000" + "\\" + raster + ".asc.gz"
                if os.path.isfile(OutZip):
					os.remove(OutZip)
                shutil.copyfile(InZip, OutZip)
                gp.delete_management(OutRaster)
                os.remove(InZip)

print "Done!!!!"
