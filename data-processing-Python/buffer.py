# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Convert to tiff
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 1:
	os.system('cls')
	print "\n Too few args"
	print "    - ie: python buffer.py"
	sys.exit(1)

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " 	   BUFFER        "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

gp.toolbox = "analysis"
gp.workspace = "D:\\CIAT\\Projects\\col-cormacarena\\01-datos-clima\\stations-averages"
outdir = "D:\\CIAT\\Projects\\col-cormacarena\\01-datos-clima\\merge_all"
distname = "0_5deg"
distancedeg = "0.5 DecimalDegrees"

# Buffer roads.shp based on each road feature's value in the Distance field,
# and dissolve buffers into groups according to values from Road_Type field.
varlist = "rain", "tmin", "tmax" 
for var in varlist:
	print var, distname
	gp.buffer(gp.workspace + "\\" + var + "_ame.shp", outdir + "\\" + var + "_ame_buff" + distname + ".shp", distancedeg, "FULL", "ROUND", "NONE")

print "done!!!"    
