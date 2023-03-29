# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 08/11/2011
# Pourpose: Create shapefiles using a selection by atributes
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob

gp = arcgisscripting.create(9.3)
gp.OverWriteOutput = 1

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python 02_MergeShapefiles.py <dirbase> <dirout> <region>"
	print "    - ie: python 02_MergeShapefiles.py F:\yapu_climate_risk\indices F:\yapu_climate_risk\continental\latinamerica LAC cdd historical"
	sys.exit(1)


dirin = sys.argv[1]
dirout = sys.argv[2]
region = sys.argv[3]
index = sys.argv[4]
scen = sys.argv[5]

# iso_list = "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", "REU", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE" 
iso_list = "ABW", "AIA", "ARG", "ATG", "BHS", "BLZ", "BOL", "BRA1", "BRA2", "BRA3", "BRA4", "BRB", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GLP", "GRD", "GTM", "GUF", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MEX", "MSR", "MTQ", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", "URY", "VCT", "VEN", "VGB", "VIR"

enosCond = "elnino", "lanina", "normal"
# indices_list = "cdd", "drd", "fld", "frd", "hwd", "p95"
scen_list = "historical", "recent-past"


if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox = "analysis"

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    MERGE SHAPEFILES    "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# for scen in scen_list: 

    # for index in indices_list: 
    
diroutshape = dirout + "\\" + scen + "\\" + index
if not os.path.exists(diroutshape):
    os.system('mkdir ' + diroutshape)
            
for enos in enosCond:

    for month in range(1, 12 + 1, 1): 

        print ". Merging", region, scen, enos, index, month
        
        inshapes = '""'

        for iso in iso_list:
            
            if str(iso) == str(iso_list[len(iso_list) - 1]):
                inshapes += dirin + "\\" + str(iso) + "\\" + scen  + "\\" + index  + "\\" + index + "_" + str(iso) + "_" + str(month) + "_" + enos + "_" + "mun.shp" + '""'
            else: 
                inshapes += dirin + "\\" + str(iso) + "\\" + scen  + "\\" + index  + "\\" + index + "_" + str(iso) + "_" + str(month) + "_" + enos + "_" + 'mun.shp";"'

        # print inshapes
            
        outshape = diroutshape + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_" + "mun.shp"
        
        if not os.path.exists(outshape):
            gp.Merge(inshapes, outshape)
        
        print "  done!"
                
print "Process", region, "done!"
                