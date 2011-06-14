# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Grid2Ascii.py <dirbase> <outdir> <variable>"
	print "   - ie: python Ascii2Gridi2.py D:\climate_change\Tyndall_Data\Global_30s\ukmo_hadgem1\2080s\_asciis Y:\Tyndall_data\A1Bp50\30yrAverages"
	sys.exit(1)

dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
# variable = sys.argv[3]

gp.CheckOutExtension("Spatial")
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

#varlist = "bio", "cons", "prec", "tmax", "tmean", "tmin"
gp.workspace = dirbase 
print "\n---> Processing: " + dirbase

var = "prec"

asclist = sorted(glob.glob(dirbase + "\\" + str(var) + "*.asc"))

for asc in asclist:
	print asc
	OutRaster = dirbase + "\\" + os.path.basename(asc)[:-4]
	if not gp.Exists(OutRaster):
		gp.ASCIIToRaster_conversion(asc, OutRaster, "FLOAT")
		
	OutRaster10 = dirout + "\\Global_10min\\ukmo_hadgem1\\2080s\\" + os.path.basename(asc)[:-4] 
	if not gp.Exists(OutRaster10):
		print OutRaster10
		gp.Resample_management(OutRaster, OutRaster10, "0.166667", "NEAREST")	
		
	OutRaster5 = dirout + "\\Global_5min\\ukmo_hadgem1\\2080s\\" + os.path.basename(asc)[:-4] 
	if not gp.Exists(OutRaster5):
		gp.Resample_management(OutRaster, OutRaster5, "0.08333333", "NEAREST")	

	OutRaster2_5 = dirout + "\\Global_2_5min\\ukmo_hadgem1\\2080s\\" + os.path.basename(asc)[:-4] 
	if not gp.Exists(OutRaster2_5):
		gp.Resample_management(OutRaster, OutRaster2_5, "0.04166667", "NEAREST")		



print "Done!!!!"

	