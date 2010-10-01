#-----------------------------------------------------------------------------------------
# Description: Correccion precipitacion downscaling interpolados
# Author: Carlos Navarro
# Date: 26/07/10
# Notes: Multiplica por el numero de dias del mes Grids erroneos interpolados precipitacion
#------------------------------------------------------------------------------------------

import arcgisscripting, string, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Correction_prec_interpolated.py P:\climate_change\IPCC_CMIP3\ F:\IPCC_CMIP3_process\tmp 2010_2039 bccr_bcm2_0"
	sys.exit(1)

dirbase = sys.argv[1]
dirtemp = sys.argv[2]
if not os.path.exists(dirtemp):
	os.system('mkdir ' + dirtemp)
period = sys.argv[3]
model = sys.argv[4]


gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CORRECTION GRIDS     "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

monDc = {"1": 31, "2": 28, "3": 31, "4": 30, "5": 31, "6": 30, 
         "7": 31, "8": 31, "9": 30, "10": 31, "11": 30, "12": 31}

gp.workspace = dirbase + "\\interpolations\\" + model + "\\" + period
print "\n---> Processing: " + model + " " + period

for month in range (1, 12 + 1, 1):
	InRaster = gp.workspace + "\\prec_" + str(month)
	OutRaster = dirtemp + "\\prec_" + str(month)
	gp.Times_sa(InRaster, int(monDc [month]), OutRaster)
	gp.delete_management(InRaster)
	gp.CopyRaster_management(OutRaster, InRaster)
	gp.delete_management(OutRaster)

print "Done!!!!"
