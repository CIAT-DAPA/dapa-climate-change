#-----------------------------------------------------------------------
# Description: Correccion precipitacion datos MRI
# Author: Carlos Navarro
# Date: 26/07/10
# Notes: Multiplica por 86400 los datos prec de los dias 1 a 9
#-----------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Correction_prec.py D:\MRI_grids\prec\SP0A D:\MRI_grids\tmp 1979 2003 prec"
	sys.exit(1)

dirbase = sys.argv[1]
dirtemp = sys.argv[2]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
inityear = int(sys.argv[3])
finalyear = int(sys.argv[4])
variable = sys.argv[5]


gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CORRECTION GRIDS     "
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
                OutRaster = dirtemp + "\\" + raster
                gp.Times_sa(InRaster, "86400", OutRaster)
                gp.delete_management(InRaster)
                gp.CopyRaster_management(OutRaster, InRaster)
                gp.delete_management(OutRaster)
                
        else:
            gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            
            rasters = gp.ListRasters(variable + "_0*", "GRID")
            for raster in rasters:
                print raster
                InRaster = gp.workspace + '\\' + raster
                OutRaster = dirtemp + "\\" + raster
                gp.Times_sa(InRaster, "86400", OutRaster)
                gp.delete_management(InRaster)
                gp.CopyRaster_management(OutRaster, InRaster)
                gp.delete_management(OutRaster)

print "Done!!!!"
