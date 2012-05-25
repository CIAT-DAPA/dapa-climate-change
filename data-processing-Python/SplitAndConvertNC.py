# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: December 6th, 2011
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python SplitAndConvertNC.py D:\climate_change\IPCC_CMIP5\rcp45"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox = "management"

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"


nclist = sorted(glob.glob(dirbase + "\\*.nc"))
for nc in nclist:
	var = os.path.basename(nc).split("_")[0]
	if  os.path.basename(nc).split("_")[1] == "Amon":
		dirout = dirbase + "\\" + var + "_mon" 
		if not os.path.exists(dirout):
			os.system('mkdir ' + dirout)	
			os.system("cdo splityear " + nc + " " + dirout + "\\mon_" + var + "_")
			ncyearlist = sorted(glob.glob(dirout + "\\*.nc"))
			for ncyear in ncyearlist:
				if not ncyear == dirout + "\\mon_" + var + "_2006.nc":
					os.remove(ncyear)
				else:
					os.system("cdo splitmon " + ncyear + " " + ncyear[:-3] + "_")
					outascii = ncyear[:-3] + "_01.asc"
					os.system("gdal_translate -of AAIGRID -b 1 -sds " + ncyear[:-3] + "_01.nc " + outascii)
					os.rename(outascii + "4", outascii)
					coordsys = "Coordinate Systems/Geographic Coordinate Systems/World/WGS 1984.prj"
					gp.defineprojection(outascii, coordsys)
					gp.ProjectRaster_management(outascii, dirout + "\\" + var + "_200601", coordsys)
	else:
		dirout = dirbase + "\\" + var + "_day" 
		if not os.path.exists(dirout):
			os.system('mkdir ' + dirout)
			os.system("cdo splityear " + nc + " " + dirout + "\\day_" + var + "_")
			ncyearlist = sorted(glob.glob(dirout + "\\*.nc"))
			for ncyear in ncyearlist:
				if not ncyear == dirout + "\\day_" + var + "_2006.nc":
					os.remove(ncyear)
				else:
					os.system("cdo splitmon " + ncyear + " " + ncyear[:-3] + "_")
					ncmonlist = sorted(glob.glob(ncyear[:-3] + "_*.nc"))
					for ncmon in ncmonlist:
						if not ncmon == dirout + "\\day_" + var + "_2006_01.nc":
							os.remove(ncmon)
						else:
							os.system("cdo splitday " + ncmon + " " + ncmon[:-3] + "_")
					
							outascii = ncmon[:-5] + "_01_01.asc"
							os.system("gdal_translate -of AAIGRID -b 1 -sds " + ncmon[:-6] + "_01_01.nc " + outascii)
							os.rename(outascii + "4", outascii)
							coordsys = "Coordinate Systems/Geographic Coordinate Systems/World/WGS 1984.prj"
							gp.defineprojection(outascii, coordsys)
							gp.ProjectRaster_management(outascii, dirout + "\\" + var + "060101", coordsys)
	