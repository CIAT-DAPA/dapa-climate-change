# ---------------------------------------------------------
# Autor: Carlos Navarro
# Proposito: Sumariza grids de un workspace con una salida
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob, shutil
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Ensemble.py D:\Workspace\extract_countries\by_regions D:\Workspace\extract_countries\by_regions\ensemble"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ ENSEMBLE ~~~~\n"

varlist = "bio", "prec", "tmean", "dtr"

ziplist = sorted(glob.glob(dirbase + "\\*.zip"))
for zip in ziplist:
	
	if not os.path.exists(zip[:-4]):
		os.system('7za x ' + zip + " -o" + dirbase)
	
	currentlist = sorted(glob.glob(zip[:-4] + "\\current*"))
	for current in currentlist:
		os.remove(current)
		
	OutDir = dirout + "\\" + os.path.basename(zip)[:-4]
	if not os.path.exists(OutDir):
		os.system('mkdir ' + OutDir)
		
	for var in varlist:
		if var == "bio":
			for month in range(1, 19 + 1, 1):
				asclist = sorted(glob.glob(zip[:-4] + "\\*" + var + "_" + str(month) + "_1.asc"))
				list = ""
				for asc in asclist:
					print "\t\t",asc
					list = list + ';' + asc
				LIST = "\"" + list[1:] + "\""

				OutRaster = OutDir + "\\" + var + "_" + str(month) + "_1"
				OutAscii = OutDir + "\\a1b_2020_2049_ensemble_" + var + "_" + str(month) + "_1.asc"
				print "\t calculating ensmeble" 
				if not gp.Exists(OutRaster):
					gp.CellStatistics_sa(LIST, OutRaster, "MEAN")
					gp.RasterToASCII_conversion(OutRaster, OutAscii)
					gp.delete_management(OutRaster)
					
		else:
			for month in range(1, 12 + 1, 1):
				asclist = sorted(glob.glob(zip[:-4] + "\\*" + var + "_" + str(month) + ".asc"))
				list = ""
				for asc in asclist:
					print "\t\t",asc
					list = list + ';' + asc
				LIST = "\"" + list[1:] + "\""
				
				OutRaster = OutDir + "\\" + var + "_" + str(month)
				OutAscii = OutDir + "\\a1b_2020_2049_ensemble_" + var + "_" + str(month) + ".asc"
				print "\t calculating ensmeble"
				if not gp.Exists(OutRaster):
					gp.CellStatistics_sa(LIST, OutRaster, "MEAN")
					gp.RasterToASCII_conversion(OutRaster, OutAscii)
					gp.delete_management(OutRaster)
					
	shutil.rmtree(zip[:-4])


print "\t ..done!!"