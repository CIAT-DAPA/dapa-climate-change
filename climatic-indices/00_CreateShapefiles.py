# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 08/11/2011
# Pourpose: Create shapefiles using a selection by atributes
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python 00_CreateShapefiles.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python 00_CreateShapefiles.py S:\admin_boundaries\gadm\gadm41_1.shp E:\yapu_climate_risk\admin_boundaries GID_0 1"
	sys.exit(1)

	
# iso_list_af = "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", "CIV", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MYT", "MAR", "NER", "NGA", "COG", "MOZ", "NAM", "REU", "RWA", "SHN", "ZAF", "SDN", "SWZ", "TZA", "COD", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE", "STP", "SEN", "SYC", "SLE", "SOM"

# No adm2
# iso_list_af = "COM", "CPV", "ESH", "LBY", "LSO", "MUS", "MYT", "SYC"

iso_list_lam =  "ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV", "GUF", "GLP", "GTM", "GUY", "HTI", "HND", "MTQ", "MEX", "NIC", "PAN", "PRY", "PER", "SUR", "URY", "VEN", "VIR", "ATG"

# No adm1
iso_list_lam = "AIA", "ANT", "CUW", "FLK", "SXM", "XCL"

# No adm2
iso_list_lam = "AIA", "ABW", "ANT", "ATG", "BHS", "BLZ", "BRB", "CUW", "CYM", "DMA", "FLK", "GRD", "JAM", "KNA", "LCA", "MSR", "PRI", "SXM", "TCA", "TTO", "UMI", "VCT", "VGB", "XCL"


inshape = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
fieldname = sys.argv[3]
level = sys.argv[4]


os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox = "analysis"

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    CREATE SHAPEFILES   "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# cur = gp.SearchCursor(inshape,"","",fieldname,"")
# row = cur.Next()

for iso in iso_list_lam:

	print iso
	diroutshape = dirout 

	if not os.path.exists(diroutshape):
		os.system('mkdir ' + diroutshape)
		
	outshape = diroutshape + "\\gadm41_" + iso + "_" + level + ".shp"
	outshape_sp = diroutshape + "\\gadm41_" + iso + "_" + level + "_sp.shp"

	if not os.path.exists(outshape):
		gp.select_analysis(inshape, outshape, fieldname + " = '" + iso + "'")

	if not os.path.exists(outshape_sp):
		gp.MultipartToSinglepart(outshape, outshape_sp)

	if level == '0':
		outshape_buff = diroutshape + "\\gadm41_" + iso + "_" + level + "_buffer.shp"
		
		if not os.path.exists(outshape_buff):
			gp.buffer(outshape, outshape_buff, "0.1 DecimalDegrees", "FULL")
		

