# ---------------------------------------------------------------------------
# Autor: Carlos Navarro
# Fecha: Mayo 26 de 2010
# Proposito: Extract by mask MRI dataset
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, os.path, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Describe_grids.py E:\MRI_grids\prec\SP0A 1979 2003 prec F:\MRI_grids\_extract_ColPlains\prec\SP0A D:\Masks\ColPlains\ColPlains.shp"
	sys.exit(1)

dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]
dirout = sys.argv[5]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
mask = sys.argv[6]
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     EXTRACT BY MASK MRI" + str(varriable)
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

diroutdescribe = dirout + "\\_describes"  
if os.path.isfile(diroutdescribe + "\\" + str(variable) + "_" + dirbase.split("\\")[:-1] + ".txt"):
    outFile = open(diroutdescribe + "\\" + str(variable) + "_" + dirbase.split("\\")[:-1] + ".txt", "a")
else:
    outFile = open(diroutdescribe + "\\" + str(variable) + "_" + dirbase.split("\\")[:-1] + ".txt", "w")

outFile.write("DATE" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")

for year in range(inityear, finalyear + 1, 1):
    for month in range (1, 12 + 1, 1):
        if month < 10:
            gp.workspace = dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            print "\n---> Processing: " + str(year) + "\t0" + str(month) + " " + str(variable)
            
            rasters = gp.ListRasters(str(variable) + "*", "GRID")
            for raster in rasters:
                print raster
				diroutraster = dirout + "\\OUT_" + str(year) + "0" + str(month) + "010000"
				if not os.path.exists(dirout):
					os.system('mkdir ' + dirout)
				OutRaster = diroutraster + "\\" + raster
                gp.ExtractByMask_sa(raster, mask, OutRaster)
				
				MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
                MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
                MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
                STD = gp.GetRasterProperties_management(OutRaster, "STD")
                CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
                outFile = open(diroutdescribe + "\\" + str(variable) + "_" + dirbase.split("\\")[:-1] + ".txt", "a")
                outFile.write(str(year) + "0" + str(month) + "\t" + OutRaster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

        else:
            gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            print "\n---> Processing: " + str(year) + "\t" + str(month) + " " + str(variable)

            rasters = gp.ListRasters(str(variable) + "*", "GRID")
            for raster in rasters:
                print raster
				diroutraster = dirout + "\\OUT_" + str(year) + + str(month) + "010000"
				if not os.path.exists(dirout):
					os.system('mkdir ' + dirout)
				OutRaster = diroutraster + "\\" + raster
                gp.ExtractByMask_sa(raster, mask, OutRaster)
				
				MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
                MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
                MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
                STD = gp.GetRasterProperties_management(OutRaster, "STD")
                CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
                outFile = open(diroutdescribe + "\\" + str(variable) + "_" + dirbase.split("\\")[:-1] + ".txt", "a")
                outFile.write(str(year) + "0" + str(month) + "\t" + OutRaster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

outFile.close()
print "done!!!"    
