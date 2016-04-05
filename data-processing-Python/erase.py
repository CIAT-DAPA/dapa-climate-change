# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Convert to tiff
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 1:
	os.system('cls')
	print "\\n Too few args"
	print "    - ie: python erase.py"
	sys.exit(1)

# ---------------------------------------------------------------------------
# Notes
# Units Resolution: arcminutes
# Resample types:   NEAREST Nearest neighbor assignment This is the default 
#                   BILINEAR Bilinear interpolation 
#                   CUBIC Cubic convolution 
#                   MAJORITY Majority resampling
# ---------------------------------------------------------------------------

os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " 	   ERASE        "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

gp.toolbox  = "analysis"
gp.erase("X:\\ALPACAS\\Plan_Regional_de_Cambio_Climatico_Orinoquia\\Informacion_IGAC\\20151104_CIAT\\Coberturas Vegetales\\Coberturas_llanos_diss.shp", "X:\\ALPACAS\\Plan_Regional_de_Cambio_Climatico_Orinoquia\\Areas_Protegidas\\ares_protegidas_llanos_pnn_runap.shp")

print "done!!!"    
