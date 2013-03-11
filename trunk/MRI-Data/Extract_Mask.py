# ---------------------------------------------------------------------------
# Autor: Carlos Navarro
# Fecha: Mayo 26 de 2010
# Proposito: Extract by mask MRI dataset
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, os.path, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_Mask.py E:\MRIData\MRI_grids\SP0A\prec 1979 2003 prec F:\MRI_grids\_extract_ColPlains\prec\SP0A D:\Masks\ColPlains\ColPlains.shp"
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
# dirdescribe = sys.argv[7]
# if not os.path.exists(dirdescribe):
	# os.system('mkdir ' + dirdescribe)

os.system('cls')
gp.CheckOutExtension("Spatial")
	
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     EXTRACT BY MASK MRI" + str(variable)
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

for year in range(inityear, finalyear + 1, 1):
    for month in range (1, 12 + 1, 1):
        if month < 10:
            gp.workspace = dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            diroutraster = dirout + "\\OUT_" + str(year) + "0" + str(month) + "010000"
            if not os.path.exists(diroutraster):
                os.system('mkdir ' + diroutraster)			
            
            print "\n---> Processing: " + str(year) + "\t0" + str(month) + " " + str(variable)
            rasters = gp.ListRasters(str(variable) + "*", "GRID")
            for raster in rasters:
                print raster
                OutRaster = diroutraster + "\\" + raster
                if not gp.Exists(OutRaster):
                    gp.ExtractByMask_sa(gp.workspace + "\\" + raster, mask, OutRaster)
                    
        else:
            gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
            diroutraster = dirout + "\\OUT_" + str(year) + str(month) + "010000"
            if not os.path.exists(diroutraster):
                os.system('mkdir ' + diroutraster)
            
            print "\n---> Processing: " + str(year) + "\t" + str(month) + " " + str(variable)
            rasters = gp.ListRasters(str(variable) + "*", "GRID")
            for raster in rasters:
                print raster

                OutRaster = diroutraster + "\\" + raster
                if not gp.Exists(OutRaster):
                    gp.ExtractByMask_sa(raster, mask, OutRaster)
                    
print "done!!!"    
