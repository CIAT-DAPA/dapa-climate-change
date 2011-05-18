#-----------------------------------------------------------------------------------------
# Description: Correction lost files prec_10, prec_11, prec_12
# Author: Carlos Navarro
# Date: 18/05/11
#------------------------------------------------------------------------------------------

import arcgisscripting, string, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python fix_prec.py Y:\Tyndall_data\A1Bp50\30yrAverages"
	sys.exit(1)

dirbase = sys.argv[1]

gp.CheckOutExtension("Spatial")
gp.toolbox = "management"
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CORRECTION LOST FILES     "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

resDc = {"2_5min": "2.5", "5min": "5", "10min": "10"}
modellist = sorted(os.listdir(dirbase + "\\Global_30s"))
periodlist = "2020s", "2030s", "2040s", "2050s", "2060s", "2070s", "2080s"
gridlist = "prec_10", "prec_11", "prec_12" 

for res in sorted(resDc):
	
	for model in modellist:
		
		for period in periodlist:
			
			gp.workspace = dirbase + "\\Global_30s\\" + model + "\\" + period
			print "---> Processing: " + model + " " + period + " " + res
			dirout = dirbase + "\\Global_" + str(res) + "\\" + model + "\\" + period
			
			for grid in gridlist:
				#Resampling process
				InRaster = gp.workspace + "\\" + grid
				OutRaster = dirout + "\\" + grid
				print "   " + grid
				if not gp.Exists(OutRaster):
					gp.Resample_management(InRaster, OutRaster , str(decVar [res]), "NEAREST")
					print "    " + OutRaster + " Resampled"
				else:
					print "    " + OutRaster + " Resampled"

print "Done!!!!"