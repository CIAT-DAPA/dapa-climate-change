# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 08/11/2011
# Pourpose: Create shapefiles using a selection by atributes
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob

gp = arcgisscripting.create(9.3)
# gp.OverWriteOutput = 1

if len(sys.argv) < 9:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python 02_MergeShapefiles_grid.py <dirbase> <dirout> <region>"
	print "    - ie: C:\Python27\ArcGIS10.8\python.exe 02_MergeShapefiles_fix_missing_pixels.py E:\yapu_climate_risk\indices D:\cenavarro\yapu_climate_risk\continental\latinamerica E:\yapu_climate_risk\admin_boundaries\missing_pixels LAC fld historical elnino 5"
	sys.exit(1)

dirin = sys.argv[1]
dirout = sys.argv[2]
dirmsk = sys.argv[3]
region = sys.argv[4]
index = sys.argv[5]
scen = sys.argv[6]
enos = sys.argv[7]
month = sys.argv[8]

# iso_list = "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", "REU", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE" 
# iso_list = "ABW", "AIA", "ARG", "ATG", "BHS", "BLZ", "BOL", "BRA1", "BRA2", "BRA3", "BRA4", "BRB", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GLP", "GRD", "GTM", "GUF", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MEX", "MSR", "MTQ", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", "URY", "VCT", "VEN", "VGB", "VIR"
iso_list = "BRA2", "BRA3"
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

# for enos in enosCond:

	# for index in indices_list:                    

    
diroutdtset = dirout + "_v1_1\\" + scen + "\\" + index
if not os.path.exists(diroutdtset):
    os.system('mkdir ' + diroutdtset)

dirinreg = dirout + "\\" + scen + "\\" + index

dirouttemp = dirout + "_v1_1\\" + scen + "\\" + index + "\\tmp" 
if not os.path.exists(dirouttemp):
    os.system('mkdir ' + dirouttemp)

if index == "cdd" or index == "drd": 

    for month in range(1, 12 + 1, 1): 

        # month = 1
        print ". Fixing ", region, scen, enos, index, month

        inshape_reg = dirinreg + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.shp"
        mask_shp = dirmsk  + "\\" + region + "_missing_pixels.shp"
        outshape_reg = diroutdtset + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.shp"
        
        if not os.path.exists(outshape_reg):
        
            gp.erase(inshape_reg, mask_shp, outshape_reg)


else: 
            
    # for month in range(1, 12 + 1, 1): 

        # month = 1
    print ". Fixing ", region, scen, enos, index, month

    outshapes_mag = '""'
    outrasters_mag = '""'

    inshape_reg = dirinreg + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.shp"
    inraster_reg = dirinreg + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.tif"

    outshape_reg = diroutdtset + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.shp"
    outraster_reg = diroutdtset + "\\" + index + "_" + region + "_" + str(month) + "_" + enos + "_mag.tif"
    
    outshapemerged = dirouttemp + "\\" + index + "_BRA_" + str(month) + "_" + enos + "_mag_miss.shp"
    
    if not os.path.exists(outshape_reg):
    
        if not os.path.exists(outshapemerged):

            print ". Extracting missing pixels"
            
            for iso in iso_list:

                prefix = str(iso) + "\\" + scen  + "\\" + index  + "\\" + index + "_" + str(iso) + "_" + str(month) + "_" + enos
                
                mask = dirmsk  + "\\" + str(iso) + "_missing_pixels_pts.shp"
                inshape_mag = dirin + "\\" + prefix + "_mag.shp" 
                inraster_mag = dirin + "\\" + prefix + "_mag.tif"
                
                outshape_mag = dirouttemp + "\\" + index + "_" + iso + "_" + str(month) + "_" + enos + "_mag_miss.shp"
                outraster_mag = dirouttemp + "\\" + index + "_" + iso + "_" + str(month) + "_" + enos + "_mag_miss.tif"
                lyr = scen + "_" + index + "_" + iso + "_" + str(month) + "_" + enos + "_mag"
                
                gp.MakeFeatureLayer(inshape_mag, lyr) 
                gp.SelectLayerByLocation(lyr, "intersect", mask)
                gp.CopyFeatures(lyr, outshape_mag)
                gp.delete_management(lyr)

                # gp.Clip_analysis(inshape_mag, mask, outshape_mag)
                gp.ExtractByMask_sa(inraster_mag, mask, outraster_mag)
                
                if str(iso) == str(iso_list[len(iso_list) - 1]):
                    outshapes_mag += outshape_mag + '""'
                    outrasters_mag += outraster_mag + '""'
                else: 
                    outshapes_mag += outshape_mag + '";"'
                    outrasters_mag += outraster_mag + '";"' 

            gp.Merge(outshapes_mag, outshapemerged)
            gp.MosaicToNewRaster_management(outrasters_mag, dirouttemp, index + "_BRA_" + str(month) + "_" + enos + "_mag_miss_tmp.tif", "#", "#", "#","1", "#", "#")
            gp.ExtractByMask_sa(dirouttemp + "\\" + index + "_BRA_" + str(month) + "_" + enos + "_mag_miss_tmp.tif", dirmsk  + "\\BRA_missing_pixels_pts.shp", dirouttemp + "\\" + index + "_BRA_" + str(month) + "_" + enos + "_mag_miss.tif")
            # gp.Merge(outrasters_mag, dirouttemp + "\\" + index + "_BRA_" + str(month) + "_" + enos + "_mag_miss.tif")
            
        print ". Merging to continental layers"

        inFeatures = '""' + inshape_reg + '";"' + outshapemerged + '""'
        inRasters = '""' + inraster_reg + '";"' + dirouttemp + "\\" + index + "_BRA_" + str(month) + "_" + enos + "_mag_miss.tif" + '""' 
        
        if not os.path.exists(outshape_reg):
            gp.Merge(inFeatures, outshape_reg)
            
        if not os.path.exists(outraster_reg):   
            gp.MosaicToNewRaster_management(inRasters, diroutdtset, index + "_" + region + "_" + str(month) + "_" + enos + "_mag.tif", "#", "#", "#","1", "#", "#")
                            
        print "Process", region, "done!"
