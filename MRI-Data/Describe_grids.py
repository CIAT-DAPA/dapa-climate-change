# ---------------------------------------------------------------------------
# Autor: Carlos Navarro
# Fecha: Agosto 3 de 2010
# Proposito: Describe las propiedades de Grids en un WorkSpace
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, os.path, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Describe_grids.py E:\MRI_grids\tmean1\SP0A 1979 2003 tmean E:\MRI_grids\_describes"
	sys.exit(1)

dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]
dirout = sys.argv[5]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     DESCRIBE GRIDS      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

if os.path.isfile(dirout + "\\desc_" + variable + ".txt"):
    outFile = open(dirout + "\\desc_" + variable + ".txt", "a")
else:
    outFile = open(dirout + "\\desc_" + variable + ".txt", "w")

outFile.write("DATE" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")

for year in range(inityear, finalyear + 1, 1):
    for month in range (1, 12 + 1, 1):
        if month < 10:
            gp.workspace = dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            
            rasters = gp.ListRasters("", "GRID")
            for raster in rasters:
                print raster
                MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
                MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
                MEA = gp.GetRasterProperties_management(raster, "MEAN")
                STD = gp.GetRasterProperties_management(raster, "STD")
                CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
                outFile = open(dirout + "\\desc_" + variable + ".txt", "a")
                outFile.write(str(year) + "0" + str(month) + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

        else:
            gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            print "\n---> Processing: " + dirbase + "\\OUT_" + str(year) + str(month) + "010000"

            rasters = gp.ListRasters("", "GRID")
            for raster in rasters:
                print raster
                MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
                MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
                MEA = gp.GetRasterProperties_management(raster, "MEAN")
                STD = gp.GetRasterProperties_management(raster, "STD")
                CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
                CEY = gp.GetRasterProperties_management(raster, "CELLSIZEY")
                outFile = open(dirout + "\\desc_" + variable + ".txt", "a")
                outFile.write(str(year) + str(month) + "\t" + raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

outFile.close()
print "done!!!"    
