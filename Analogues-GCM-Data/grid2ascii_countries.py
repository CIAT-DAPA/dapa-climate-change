# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# 		First, cut the climate data with cut_process.aml and cut_GCM.aml scripts. 
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python grid2ascii_countries.py F:\Analogues_GCM_data\ExtractByCountry\SRES_A1B\downscaled F:\Analogues_GCM_data\asciis_tiffs_by_countries 30s"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <scenario>, <mask>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
# dirtemp = sys.argv[2]
# if not os.path.exists(dirtemp):
    # os.system('mkdir ' + dirtemp)
dirout = sys.argv[2]
resolution = sys.argv[3]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

# countryDc = {"ind": "67 6 99 37 ", "bgd": "87 19 94 28 ", "mli": "-13 8 6 27 ", "sen": "-19 11 -10 18 ", "ner": "-1 10 17 25 ",
			 # "bfa": "-6.5 8 3 17 ", "gha": "-4 3 2 13 ", "nep": "79 24 89 32 ", "eth": "32 0 49 17 ", "uga": "28 -3 36 6 ", "ken": "33 -7 43 7 ", "tza": "28 -14 41 1 ", }
period = "2020_2049"
# res = "0.016667"

countrytilelist_1 = "ala", "alb", "and", "are", "arm", "ata", "atg", "aut", "aze", "bdi", "bel", "ben", "bgr", "bhr", "bih", "blz", "bmu", "brb", "brn", "btn", "che", "com",
				"cpv", "cri", "cyp", "cze", "dji", "dma", "dnk", "dom", "est", "fro", "geo", "glp", "gmb", "gnb", "grd", "gtm", "guf", "hnd", "hti", "hun", "irl", "isr", "jam",
				"jor", "khm", "kwt", "lbn", "lbr", "lca", "lie", "lka", "lso", "ltu", "lux", "lva", "mac", "mco", "mda", "mkd", "mnp", "mtq", "nic", "nld", "pan", "pri", "qat", 
				"reu", "rwa", "sle", "slv", "smr", "stp", "sur", "svk", "svn", "swz", "syc", "tgo", "ton", "twn", "vct", "wsm"
# countrytilelist_4 = "bfa", "bgd", "bhs", "blr", "bwa", "civ", "cmr", "cog", "cub", "deu", "eri", "esh", "gab", "gha", "gin", "gnq", "grc", "guy", "hrv", "irq", "isl", "ken", 
				# "kgz", "kir", "kor", "lao", "mus", "mwi", "ncl", "npl", "omn", "pol", "prk", "pry", "pyf", "rom", "sen", "sgp", "slb", "syr", "tca", "tjk", "tun", "uga", "ury", 
				# "vut", "yem", "zwe"
# countrytilelist_9 = "afg", "ago", "bol", "caf", "ecu", "egy", "eth", "fin", "fra", "gbr", "ita", "lby", "mar", "mdg", "mdv", "mmr", "moz", "mrt", "mys", "nam", "ner", "nga", 
				# "phl", "png", "som", "swe", "tcd", "tha", "tkm", "tur", "tza", "ukr", "uzb", "ven", "vnm", "zmb"
# countrytilelist_16 = "col", "dza", "esp", "irn", "mli", "mlt", "mng", "nor", "pak", "per", "prt", "sau", "sdn"
# countrytilelist_25 = "mex", "sjm", "zaf"
# countrytilelist_36 = "arg", "idn", "ind", "jpn", "kaz"
# countrytilelist_81 = "aus", "bra", "chl", "grl"
# countrytilelist_100 = "chn"
# countrytilelist_121 =  "fji" 
# countrytilelist_169 = "can"
# countrytilelist_289 = "nzl"
# countrytilelist_676 = "rus"
# countrytilelist_841 = "usa"

for country in countrytilelist_1:
	
	modellist = sorted(os.listdir(dirbase + "\\" + country + "_" + str(resolution)))
	
	print "~~~~~~~~~~~~~~~~~~~~~~"
	print "  CONVERT TO ASCII	 "
	print "~~~~~~~~~~~~~~~~~~~~~~"

	for model in modellist:
		if model == "current":
			# for country in countrylist:
				
			gp.workspace = dirbase + "\\" + country + "_" + str(resolution) + "\\" + model
			print "\n---> Processing: " + model
			print "\n---> Processing: " + model, period
			
			diroutAscii = dirout + "\\" + country + "_" + str(resolution)
			if not os.path.exists(diroutAscii):
				os.system('mkdir ' + diroutAscii)
			
			rasters = gp.ListRasters("*", "GRID")

			for raster in rasters:
				if not os.path.basename(raster)[0:4] == "tmin" or not os.path.basename(raster)[0:4] == "tmax" or not os.path.basename(raster)[0:4] == "cons":
					
					if os.path.basename(raster)[0:3] == "bio":
						OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
					elif os.path.basename(raster)[0:4] == "prec" or os.path.basename(raster)[0:3] == "dtr" or os.path.basename(raster)[0:5] == "tmean":
						OutAscii = diroutAscii + "\\" + model + "_" + raster + ".asc"
					
					print "Converting " + os.path.basename(OutAscii)
					gp.RasterToASCII_conversion(raster, OutAscii)

		else:
			gp.workspace = dirbase + "\\" + country + "_" + str(resolution) + "\\" + model + "\\" + period
			print "\n---> Processing: " + model, period
			
			diroutAscii = dirout + country + "_" + str(resolution)
			if not os.path.exists(diroutAscii):
				os.system('mkdir ' + diroutAscii)
			
			rasters = gp.ListRasters("*", "GRID")

			for raster in rasters:
				if not os.path.basename(raster)[0:4] == "tmin" or not os.path.basename(raster)[0:4] == "tmax" or not os.path.basename(raster)[0:4] == "cons":
					
					if os.path.basename(raster)[0:3] == "bio":
						OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + "_1.asc"
					elif os.path.basename(raster)[0:4] == "prec" or os.path.basename(raster)[0:3] == "dtr" or os.path.basename(raster)[0:5] == "tmean":
						OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + ".asc"
					
					print "Converting " + os.path.basename(OutAscii)
					gp.RasterToASCII_conversion(raster, OutAscii)
print "done!!!"    







