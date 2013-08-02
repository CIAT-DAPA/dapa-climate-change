# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Check the tiles files created
# ----------------------------------------------------------------------------------

import os, sys, string

#Syntax
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python check_files.py D:\cenavarro\Analogues_GCM_data\TilesByCountry a2 2020_2049"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
sres = sys.argv[2]
# resolution = sys.argv[3]
period = sys.argv[3]

os.system('cls')
reslist = "30s", "2_5min" #, "5min", "10min"

for resolution in reslist:

	if resolution == "30s":
		countrytilelist = "cod", "fji" #, "kna", "mne", "nru", "nzl", "plw", "pse", "srb", "ssd", "tls", "tto", "tuv", "pan"
		countryDic = {"cod": "6 6 ", "fji": "1 1 ", "kna": "1 1 ", "mne": "1 1 ", "nru": "1 1 ", "nzl": "5 5 ", "plw": "1 1 ", "pse": "1 1 ", "srb": "1 1 ", "ssd": "3 3 ", "tls": "1 1 ", "tto": "1 1 ", "tuv": "1 1 ", "pan":"2 2 "}

		# countrytilelist =   "afg", "ago", "arg", "aus", "bfa", "bgd", "bhs", "blr", "bol", "bra", "bwa", "caf", "can", "chl", "chn", "civ", "cmr", "cog", "col",\
							# "cub", "deu", "dnk", "dza", "ecu", "egy", "eri", "esh", "esp", "fin", "fji", "fra", "gab", "gbr", "gha", "gin", "gnq", "grc", "grl",\
							# "guy", "hrv", "idn", "ind", "irn", "irq", "isl", "ita", "jpn", "kaz", "kgz", "khm", "kir", "kor", "lao", "lby", "mar", "mdg",\
							# "mdv", "mex", "mli", "mlt", "mmr", "mng", "moz", "mrt", "mus", "mwi", "mys", "nam", "ncl", "ner", "nic", "nga", "nor", "npl",  "omn",\
							# "pak", "per", "phl", "png", "pol", "prk", "prt", "pry", "pyf", "rom",  "sau", "sdn", "sen", "sgp", "sjm", "slb", "som", "swe",\
							# "syr", "tca", "tcd", "tha", "tjk", "tkm", "tun", "tur", "ukr", "ury",  "uzb", "ven", "vnm", "vut", "yem", "zaf",\
							# "zmb", "zwe", "nzl", "rus", "usa", "eth", "tza", "ken", "uga", "ala", "alb", "and", "are", "arm", "ata", "atg", "aut", "aze", "bdi", "bel", "ben", "bgr", "bhr", "bih", "blz", "bmu", "brb", "brn", "btn", "che", "com",\
							# "cpv", "cri", "cyp", "cze", "dji", "dma", "dom", "est", "fro", "geo", "glp", "gmb", "gnb", "grd", "gtm", "guf", "hnd", "hti", "hun", "irl", "isr", "jam",\
							# "jor", "kwt", "lbn", "lbr", "lca", "lie", "lka", "lso", "ltu", "lux", "lva", "mac", "mco", "mda", "mkd", "mnp", "mtq", "nld", "pan", "pri", "qat", "reu",\
							# "rwa", "sle", "slv", "smr", "stp", "sur", "svk", "svn", "swz", "syc", "tgo", "ton", "twn", "vct", "wsm"
							# "alb", "aze", "bgr", "can", "dom", "esp", "fji", "fra", "gab", "gbr", "geo", "grl", "guf", "hti", "hun", "kaz", "mda", "mkd", "sur", "svn", "zaf", "zmb",\ 
							# "zwe", "arm", "pak", "prk", "rom", "rwa", "sau", "ton", "tza", "vct" #a2 = "cri", "cyp", "cze", "lao", "sdn", "tza", "vct", "zaf"  #"arm", "chn", "cri", "cyp",\
							# "cze", "grl", "idn", "ind", "irn", "ita", "jpn", "kaz", "lao", "lby", "mar", "mdg", "pak", "per", "pol", "prk", "prt", "pry", "rom", "rwa", "sau", "sdn", "sjm", "slb", "som", "tha", "tkm", "ton", "tur", "tza", "ukr", "vct", "zaf"
						
		# countryDic = {"bfa": "2 2 ", "bgd": "2 2 ", "bhs": "2 2 ", "blr": "2 2 ", "bwa": "2 2 ", "civ": "2 2 ", "cmr": "2 2 ", "cog": "2 2 ", "cub": "2 2 ",\
					# "deu": "2 2 ", "dnk": "2 2 ", "eri": "2 2 ", "esh": "2 2 ", "gab": "2 2 ", "gha": "2 2 ", "gin": "2 2 ", "gnq": "2 2 ", "grc": "2 2 ", "guy": "2 2 ",\
					# "hrv": "2 2 ", "irq": "2 2 ", "isl": "2 2 ", "ken": "2 2 ", "kgz": "2 2 ", "khm": "2 2 ", "kir": "2 2 ", "kor": "2 2 ", "lao": "2 2 ", "mus": "2 2 ",\
					# "mwi": "2 2 ", "ncl": "2 2 ", "nic": "2 2 ", "npl": "2 2 ", "omn": "2 2 ", "pol": "2 2 ", "prk": "2 2 ", "pry": "2 2 ", "pyf": "2 2 ", "rom": "2 2 ",\
					# "sen": "2 2 ", "sgp": "2 2 ", "slb": "2 2 ", "syr": "2 2 ", "tca": "2 2 ", "tjk": "2 2 ", "tun": "2 2 ", "uga": "2 2 ", "ury": "2 2 ",\
					# "vut": "2 2 ", "yem": "2 2 ", "zwe": "2 2 ", "afg": "3 3 ", "ago": "3 3 ", "bol": "3 3 ", "caf": "3 3 ", "ecu": "3 3 ", "egy": "3 3 ",\
					# "eth": "3 3 ", "fin": "3 3 ", "fra": "3 3 ", "gbr": "3 3 ", "ita": "3 3 ", "mar": "3 3 ", "mdg": "3 3 ", "mdv": "3 3 ",\
					# "mmr": "3 3 ", "moz": "3 3 ", "mrt": "3 3 ", "mys": "3 3 ", "nam": "3 3 ", "ner": "3 3 ", "nga": "3 3 ", "phl": "3 3 ", "png": "3 3 ",\
					# "som": "3 3 ", "swe": "3 3 ", "tcd": "3 3 ", "tha": "3 3 ", "tkm": "3 3 ", "tur": "3 3 ", "tza": "3 3 ", "ukr": "3 3 ", "uzb": "3 3 ",\
					# "ven": "3 3 ", "vnm": "3 3 ", "zmb": "3 3 ", "col": "4 4 ", "esp": "4 4 ", "irn": "4 4 ", "lby": "4 4 ", "mli": "4 4 ", "mlt": "4 4 ",\
					# "mng": "4 4 ", "nor": "4 4 ", "pak": "4 4 ", "per": "4 4 ", "prt": "4 4 ", "sau": "4 4 ", "sdn": "4 4 ", "dza": "5 5 ", "mex": "5 5 ", "sjm": "5 5 ",\
					# "zaf": "5 5 "}
		
	elif resolution == "2_5min":
		countrytilelist = "arg", "idn" #, "aus", "bra", "can", "chl", "chn", "grl", , "ind", "jpn", "kaz", "rus", "usa", "af", "as", "eu", "na", "oc", "sa"
		countryDic = {"arg": "2 2 ", "aus": "3 3 ", "bra": "3 3 ", "can": "4 4 ", "chl": "3 3 ", "chn": "3 3 ", "grl": "3 3 ", "idn": "2 2 ", "ind": "2 2 ", "jpn": "1 1 ", "kaz": "2 2 ", "rus": "7 7 ", "usa": "8 8 ", "af": "4 4 ", "as": "6 6 ", "eu": "3 3 ", "na": "5 5 ", "oc": "3 3 ", "sa": "4 4 "}
	# elif resolution == "5min":
		# countrytilelist = "af", "as", "eu", "na", "oc", "sa"
		# countryDic = {"af": "2 2 ", "as": "3 3 ", "eu": "2 2 ", "na": "3 3 ", "oc": "2 2 ", "sa": "2 2 "}
	# elif resolution == "10min":
		# countrytilelist = "af", "as", "eu", "na", "oc", "sa"
		# countryDic = {"af": "1 1 ", "as": "2 2 ", "eu": "1 1 ", "na": "1 1 ", "oc": "1 1 ", "sa": "1 1 "}
		
		
	monthDic = {"bio": "19", "tmean": "12", "dtr": "12", "prec": "12"}

	if sres == "baseline":
		
		model = "current"
		
		for country in countrytilelist:

			for var in monthDic:
				
				print "\n --> Check files: " + country,sres,model,period,var,"\n"
				tifdir = dirbase + "\\" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"

				for tile in range(0, int(int(str(countryDic [country]).split(" ")[0]) * int(str(countryDic [country]).split(" ")[1]) - 1) + 1, 1):
				
					for month in range(1, int(monthDic [var]) + 1, 1):
						
						tiff = tifdir + "\\" + sres + "_" + period + "_" + model + "_" + var + "_" + str(month) + "_" + str(tile) + ".tif"
						
						if not os.path.exists(tiff):
							print country, model, os.path.basename(tiff), "NO"
							outFile = open(dirbase + "\\" + sres + "_check.txt", "a")
							outFile.write(country + "\t" + model + "\t" + resolution + "\n")
							outFile.close()
							
						else:
							print country, model, os.path.basename(tiff), "OK"
						
	else:
					
		for country in countrytilelist:

			if sres == "a2":
				modellist = "bccr_bcm2_0", "cccma_cgcm3_1_t47", "cnrm_cm3", "csiro_mk3_0", "csiro_mk3_5", "ensemble", "gfdl_cm2_0", "gfdl_cm2_1", "giss_model_er", "ingv_echam4", "inm_cm3_0", "ipsl_cm4", "miroc3_2_medres", "miub_echo_g", "mpi_echam5", "mri_cgcm2_3_2a", "ncar_ccsm3_0", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1"
			if sres == "a1b":
				modellist = "bccr_bcm2_0", "cccma_cgcm3_1_t47", "cccma_cgcm3_1_t63", "cnrm_cm3", "csiro_mk3_0", "csiro_mk3_5", "ensemble", "gfdl_cm2_0", "gfdl_cm2_1", "giss_aom", "giss_model_eh", "giss_model_er", "iap_fgoals1_0_g", "ingv_echam4", "inm_cm3_0", "ipsl_cm4", "miroc3_2_hires", "miroc3_2_medres", "miub_echo_g", "mpi_echam5", "mri_cgcm2_3_2a", "ncar_ccsm3_0", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1"
			if sres == "b1":
				modellist = "bccr_bcm2_0", "cccma_cgcm3_1_t47", "cccma_cgcm3_1_t63", "cnrm_cm3", "csiro_mk3_0", "csiro_mk3_5", "ensemble", "gfdl_cm2_0", "gfdl_cm2_1", "giss_aom", "giss_model_er", "iap_fgoals1_0_g", "inm_cm3_0", "ipsl_cm4", "miroc3_2_hires", "miroc3_2_medres", "miub_echo_g", "mpi_echam5", "mri_cgcm2_3_2a", "ncar_ccsm3_0", "ukmo_hadcm3"
			
			for model in modellist:
				
				for var in monthDic:
					
					print "\n --> Check files: " + country,sres,model,period,var,"\n"
					tifdir = dirbase + "\\sres_" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"

					for tile in range(0, int(int(str(countryDic [country]).split(" ")[0]) * int(str(countryDic [country]).split(" ")[1]) - 1) + 1, 1):
					
						for month in range(1, int(monthDic [var]) + 1, 1):
							
							tiff = tifdir + "\\" + sres + "_" + period + "_" + model + "_" + var + "_" + str(month) + "_" + str(tile) + ".tif"
							
							if not os.path.exists(tiff):
								print country,model,os.path.basename(tiff),"NO"
								outFile = open(dirbase + "\\" + sres + "_check_final.txt", "a")
								outFile.write(country + "\t" + model + "\t" + resolution + "\n")
								outFile.close()
								
							else:
								print country,model,os.path.basename(tiff),"OK"
							
			print "\n",country," checked!"
		
print "\n Checking " + sres + " done!"
