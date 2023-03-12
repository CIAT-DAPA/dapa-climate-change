# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 08/11/2011
# Pourpose: Create shapefiles using a selection by atributes
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob

gp = arcgisscripting.create(9.3)
gp.OverWriteOutput = 1

if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python 02_MergeShapefiles_grid.py <dirbase> <dirout> <region>"
	print "    - ie: python 02_MergeShapefiles_grid.py D:\cenavarro\yapu_climate_risk\indices D:\cenavarro\yapu_climate_risk\continental\africa E:\yapu_climate_risk\admin_boundaries AF cdd historical elnino"
	sys.exit(1)


dirin = sys.argv[1]
dirout = sys.argv[2]
dirmsk = sys.argv[3]
region = sys.argv[4]
index = sys.argv[5]
scen = sys.argv[6]
enos = sys.argv[7]

iso_list = "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", "REU", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE" 
# iso_list = "ABW", "AIA", "ARG", "ATG", "BHS", "BLZ", "BOL", "BRA1", "BRA2", "BRA3", "BRA4", "BRB", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GLP", "GRD", "GTM", "GUF", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MEX", "MSR", "MTQ", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", "URY", "VCT", "VEN", "VGB", "VIR"

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


## Cut shapefiles and raster

# for iso in iso_list:

    # mask = dirmsk  + "\\gadm41_" + str(iso) + "_0_grid.shp"
    
    # # for enos in enosCond:
    
    # for month in range(1, 12 + 1, 1):

        # print ". Cutting", iso, scen, index, enos, month

        # indat = dirin + "\\" + str(iso) + "\\" + scen  + "\\" + index  + "\\" + index + "_" + str(iso) + "_" + str(month) + "_" + enos 

        # if not os.path.exists(indat + "_mag_msk.tif"):
                                
            # if index != "fld": 
                # gp.Clip_analysis(indat + ".shp", mask, indat + "_msk.shp")
                # gp.ExtractByMask_sa(indat + ".tif", mask, indat + "_msk.tif")
                
            # gp.Clip_analysis(indat + "_mag.shp", mask, indat + "_mag_msk.shp")
            # gp.ExtractByMask_sa(indat + "_mag.tif", mask, indat + "_mag_msk.tif")                    

    
diroutdtset = dirout + "\\" + scen + "\\" + index
if not os.path.exists(diroutdtset):
    os.system('mkdir ' + diroutdtset)
            
# for enos in enosCond:

	# for index in indices_list: 

for month in range(1, 12 + 1, 1): 

	print ". Merging", region, scen, enos, index, month
	
	inshapes = '""'
	inrasters = '""'
	inshapes_mag = '""'
	inrasters_mag = '""'
	
	for iso in iso_list:
		
		prefix = str(iso) + "\\" + scen  + "\\" + index  + "\\" + index + "_" + str(iso) + "_" + str(month) + "_" + enos

		if str(iso) == str(iso_list[len(iso_list) - 1]):
		
			inshapes += dirin + "\\" + prefix + "_msk.shp" + '""'
			inrasters += dirin + "\\" + prefix + "_msk.tif" + '""'
			inshapes_mag += dirin + "\\" + prefix + "_mag_msk.shp" + '""'
			inrasters_mag += dirin + "\\" + prefix + "_mag_msk.tif" + '""'
			
		else: 
		
			inshapes += dirin + "\\" + prefix + '_msk.shp";"'
			inrasters += dirin + "\\" + prefix + '_msk.tif";"'
			inshapes_mag += dirin + "\\" + prefix + '_mag_msk.shp";"'
			inrasters_mag += dirin + "\\" + prefix + '_mag_msk.tif";"' #issue
	
	outshape = diroutdtset + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + ".shp"
	outraster = index + "_" + region + "_" + str(month) + "_" + enos + ".tif"	
	outshape_mag = diroutdtset + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.shp"
	outraster_mag = index + "_" + region + "_" + str(month) + "_" + enos + "_mag.tif"

	
	# if not os.path.exists(outraster_mag):
	
	if index != "fld": 
		# gp.Merge(inshapes, outshape)
		gp.MosaicToNewRaster_management(inrasters, diroutdtset, outraster, "#", "#", "#","1", "#", "#")
	
	gp.Merge(inshapes_mag, outshape_mag)
	gp.MosaicToNewRaster_management(inrasters_mag, diroutdtset, outraster_mag, "#", "#", "#","1", "#", "#")

	print "  done!"
					
print "Process", region, "done!"
                