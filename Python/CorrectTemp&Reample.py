# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/09/2010
# Pourpose: Resample Downscaling Disaggregated
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Resample_Disaggregated.py <dirbase> <scenario> <dirout> <resolution> <method>"
	print "    - ie: python Resample_Disaggregated.py D:\climate_change\IPCC_CMIP3\Nepal_Extract A1B D:\climate_change\IPCC_CMIP3\NepalCorrected_Extract D:\climate_change\IPCC_CMIP3\tmp"
	sys.exit(1)

# ---------------------------------------------------------------------------
# Notes
# Units Resolution: arcminutes
# Resample types:   NEAREST Nearest neighbor assignment This is the default 
#                   BILINEAR Bilinear interpolation 
#                   CUBIC Cubic convolution 
#                   MAJORITY Majority resampling
# ---------------------------------------------------------------------------

dirbase = sys.argv[1]
scenario = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)
# resolution = sys.argv[4]
# method = sys.argv[5]
dirtemp = sys.argv[4]
if not os.path.exists(dirtemp):
    os.system('mkdir ' + dirtemp)

os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " RESAMPLE DISAGGREGATED "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

# Get a list of periods and models
periodlist = "2020_2049", "2010_2039"#"2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = os.listdir(dirbase + "SRES_" + scenario + "\disaggregated")

# Correct resolution
res = "0.0125000007"


for model in modellist:
       
    for period in periodlist:

        # Create Dir Resample folders
        diroutResample = dirout + "\\" + model + "\\" + period
        if not os.path.exists(diroutResample):
            os.system('mkdir ' + diroutResample)
		
        # Set workspace
        gp.workspace = dirbase + "\\SRES_" + scenario + "\\downscaled\\Global_30s\\" + model + "\\" + period
        print "\n---> Processing: " + model + "  " + period
		
		for month in range (1, 12 + 1, 1):
			print "dtr_" + str(month)
			
			OutDtrRes = diroutResample + "\\dtr_" + str(month)  
			OutDtr = dirtemp + "\\dtr_" + str(month)
			OutTmeanRes = diroutResample + "\\tmean_" + str(month)  
			OutTmean = dirtemp + "\\tmean_" + str(month)
			OutPrecRes = diroutResample + "\\prec_" + str(month)  
			
			if not gp.Exists(OutDtrRes):
				print "dtr_" + str(month)
				InExpression = (gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)) + " * 0.1"
				gp.SingleOutputMapAlgebra_sa(InExpression, OutDtr)          
                gp.Resample_management(OutDtr, OutDtrRes , res, "NEAREST")			
			
			if not gp.Exists(OutTmeanRes):
				print "tmean_" + str(month)
				InExpression = gp.workspace + "\\tmean_" + str(month) + " * 0.1"
				gp.SingleOutputMapAlgebra_sa(InExpression, OutTmean)          
                gp.Resample_management(OutTmean, OutTmeanRes , res, "NEAREST")	
			
			if not gp.Exists(OutPrecRes):
				print "prec_" + str(month)
                gp.Resample_management(gp.workspace + "\\prec_" + str(month), OutPrecRes , res, "NEAREST")		
				
        # Get a list of rasters in workspace    
        rasters = gp.ListRasters("bio*", "GRID")

        for raster in rasters:
			if os.path.basenme(raster) == "bio_1" or os.path.basenme(raster) == "bio_4" or os.path.basenme(raster) == "bio_5" or os.path.basenme(raster) == "bio_6" or os.path.basenme(raster) == "bio_7" or os.path.basenme(raster) == "bio_8" or os.path.basenme(raster) == "bio_9" or os.path.basenme(raster) == "bio_10" or os.path.basenme(raster) == "bio_11":
				print raster
				InExpression = raster + " * 0.1"
				gp.SingleOutputMapAlgebra_sa(InExpression, dirtemp + "\\" + raster)          
                gp.Resample_management(dirtemp + "\\" + raster, diroutResample + "\\" + raster , res, "NEAREST")	

			else: 
				print raster
				gp.Resample_management(raster, diroutResample + "\\" + raster , res, "NEAREST")

print "done!!!"    
