# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Grid2Ascii.py <dirbase> <outdir> <variable>"
	print "   - ie: python Ascii2Gridi.py D:\EcoCrop-development\climate\afasia_2_5min_future"
	sys.exit(1)

dirbase =sys.argv[1]
# dirout = sys.argv[2]
# if not os.path.exists(dirout):
	# os.system('mkdir ' + dirout)
# variable = sys.argv[3]

gp.CheckOutExtension("Spatial")
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ASCII TO GRID       "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

print "\n---> Processing: " + dirbase

modellist = sorted(os.listdir(dirbase))
for model in modellist:
	print model
	gp.workspace = dirbase + "\\" + model + "\\2020_2049\\_asciis"
	asclist = sorted(glob.glob(gp.workspace + "\\*.asc"))

	for asc in asclist:
		print "converting", os.path.basename(asc)
		OutRaster = dirbase + "\\" + model + "\\2020_2049\\" + os.path.basename(asc)[:-4]
		print OutRaster
		if not gp.Exists(OutRaster):
			gp.ASCIIToRaster_conversion(asc, OutRaster, "FLOAT")


print "Done!!!!"

	