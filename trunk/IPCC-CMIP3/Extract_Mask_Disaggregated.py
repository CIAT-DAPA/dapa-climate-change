# ---------------------------------------------------------------------------
# Autor: Carlos Navarro
# Fecha: Septiembre 13 de 2010
# Proposito: Extraction with Centroamerica mask, diseggregation surfaces
# Nota: Si el proceso se interrumpe borrar el ultimo periodo procesado
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_Mask_Disaggregated.py L:\climate_change\IPCC_CMIP3 A1B G:\IPCC_CMIP3\mask\Centroamerica.shp G:\IPCC_CMIP3"
	sys.exit(1)

dirbase = sys.argv[1]
scenario = sys.argv[2]
mask = sys.argv[3]
dirout = sys.argv[4]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK DISAGGREGATTED "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069"
modellist = os.listdir(dirbase + "\\SRES_" + scenario + "\\disaggregated")
print "Available models: " + str(modellist)

for model in modellist:
    for period in periodlist:

        gp.workspace = dirbase + "\\SRES_" + scenario + "\\disaggregated" + "\\" + model + "\\" + period
        print "\n---> Processing: " + dirbase + "\\SRES_" + scenario + "\\disaggregated" + "\\" + model + "\\" + period

        diroutraster = dirout + "\\SRES_" + scenario + "\\disaggregated" + "\\" + model + "\\" + period

        if not os.path.exists(diroutraster):
            os.system('mkdir ' + diroutraster)
            rasters = gp.ListRasters("", "GRID")
            for raster in rasters:
                print "    Extracting " + raster
                OutRaster = diroutraster + "\\" + raster
                gp.ExtractByMask_sa(raster, mask, OutRaster)
        else:
            print "The model " + model + " " + period + " is already processed"
            print "Processing the next period \n"

print "done!!!"    
