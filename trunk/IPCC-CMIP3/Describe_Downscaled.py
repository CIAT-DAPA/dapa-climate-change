# ---------------------------------------------------------------------------
# Autor: Carlos Navarro
# Fecha: Agosto 24 de 2010
# Proposito: Describe las propiedades de Grids del Downscaling Interpolations
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Describe_Downscaled.py M:\climate_change\IPCC_CMIP3\ A1B D:\Describes\Downscaled"
	sys.exit(1)

dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print " DESCRIBE DOWNSCALED "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = os.listdir(dirbase + "SRES_" + scenario + "\downscaled")

if os.path.isfile(dirout + "\\Downscaled_SRES_" + scenario + ".txt"):
    outFile = open(dirout + "\\Downscaled_SRES_" + scenario + ".txt", "a")
else:
    outFile = open(dirout + "\\Downscaled_SRES_" + scenario + ".txt", "w")

outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")

for model in modellist:
    for period in periodlist:
        gp.workspace = dirbase + "SRES_" + scenario + "\downscaled" + "\\" + model + "\\" + period
        print "\n---> Processing: " + dirbase + "SRES_" + scenario + "\downscaled" + "\\" + model + "\\" + period

        rasters = gp.ListRasters("", "GRID")
        rasterssort = rasters.sort()
        for raster in rasterssort:
            print raster
            MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
            MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
            MEA = gp.GetRasterProperties_management(raster, "MEAN")
            STD = gp.GetRasterProperties_management(raster, "STD")
            CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
            outFile = open(dirout + "\\Interpolations_SRES_" + scenario + ".txt", "a")
            outFile.write(scenario + "\t" + model + "\t" + period + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

outFile.close()

print "done!!!"    
