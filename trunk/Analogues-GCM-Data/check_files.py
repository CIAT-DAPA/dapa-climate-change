# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Check the tiles files created
# ----------------------------------------------------------------------------------

import os, sys, string

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python check_files.py S:\data\portals\analogues-gcm-data\TilesByCountry a2 30s 2020_2049"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
sres = sys.argv[2]
resolution = sys.argv[3]
period = sys.argv[4]

os.system('cls')

countrytilelist = "alb", "aze", "bgr", "can", "dom", "esp", "fji", "fra", "gab", "gbr", "geo", "grl", "guf", "hti", "hun", "kaz", "mda", "mkd", "sur", "svn", "zaf", "zmb", "zwe" # a1b = "arm", "pak", "prk", "rom", "rwa", "sau", "ton", "tza", "vct" #a2 = "cri", "cyp", "cze", "lao", "sdn", "tza", "vct", "zaf"  #"arm", "chn", "cri", "cyp", "cze", "grl", "idn", "ind", "irn", "ita", "jpn", "kaz", "lao", "lby", "mar", "mdg", "pak", "per", "pol", "prk", "prt", "pry", "rom", "rwa", "sau", "sdn", "sjm", "slb", "som", "tha", "tkm", "ton", "tur", "tza", "ukr", "vct", "zaf"

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
			
checkDic = {"bfa": "3", "bgd": "3", "bhs": "3", "blr": "3", "bwa": "3", "civ": "3", "cmr": "3", "cog": "3", "cub": "3",\
			"deu": "3", "dnk": "3", "eri": "3", "esh": "3", "gab": "3", "gha": "3", "gin": "3", "gnq": "3", "grc": "3", "guy": "3",\
			"hrv": "3", "irq": "3", "isl": "3", "ken": "3", "kgz": "3", "khm": "3", "kir": "3", "kor": "3", "lao": "3", "mus": "3",\
			"mwi": "3", "ncl": "3", "nic": "3", "npl": "3", "omn": "3", "pol": "3", "prk": "3", "pry": "3", "pyf": "3", "rom": "3",\
			"sen": "3", "sgp": "3", "slb": "3", "syr": "3", "tca": "3", "tjk": "3", "tun": "3", "uga": "3", "ury": "3",\
			"vut": "3", "yem": "3", "zwe": "3", "afg": "8", "ago": "8", "bol": "8", "caf": "8", "ecu": "8", "egy": "8",\
			"eth": "8", "fin": "8", "fra": "8", "gbr": "8", "ita": "8", "mar": "8", "mdg": "8", "mdv": "8",\
			"mmr": "8", "moz": "8", "mrt": "8", "mys": "8", "nam": "8", "ner": "8", "nga": "8", "phl": "8", "png": "8",\
			"som": "8", "swe": "8", "tcd": "8", "tha": "8", "tkm": "8", "tur": "8", "tza": "8", "ukr": "8", "uzb": "8",\
			"ven": "8", "vnm": "8", "zmb": "8", "col": "15", "esp": "15", "irn": "15", "lby": "15", "mli": "15", "mlt": "15",\
			"mng": "15", "nor": "15", "pak": "15", "per": "15", "prt": "15", "sau": "15", "sdn": "15", "dza": "24", "mex": "24", "sjm": "24",\
			"zaf": "24", "arg": "35", "idn": "35", "jpn": "35", "kaz": "35", "ind": "48", "bra": "80", "chl": "80",\
			"grl": "80", "aus": "99", "chn": "99", "fji": "120", "can": "168", "nzl": "168", "rus": "168", "usa": "168",\
			"ala": "0", "alb": "0", "and": "0", "are": "0", "arm": "0", "ata": "0", "atg": "0", "aut": "0", "aze": "0", "bdi": "0", "bel": "0", "ben": "0", "bgr": "0", "bhr": "0", "bih": "0", "blz": "0", "bmu": "0", "brb": "0", "brn": "0", "btn": "0", "che": "0", "com": "0",\
			"cpv": "0", "cri": "0", "cyp": "0", "cze": "0", "dji": "0", "dma": "0", "dom": "0", "est": "0", "fro": "0", "geo": "0", "glp": "0", "gmb": "0", "gnb": "0", "grd": "0", "gtm": "0", "guf": "0", "hnd": "0", "hti": "0", "hun": "0", "irl": "0", "isr": "0", "jam": "0",\
			"jor": "0", "kwt": "0", "lbn": "0", "lbr": "0", "lca": "0", "lie": "0", "lka": "0", "lso": "0", "ltu": "0", "lux": "0", "lva": "0", "mac": "0", "mco": "0", "mda": "0", "mkd": "0", "mnp": "0", "mtq": "0", "nld": "0", "pan": "0", "pri": "0", "qat": "0", "reu": "0",\
			"rwa": "0", "sle": "0", "slv": "0", "smr": "0", "stp": "0", "sur": "0", "svk": "0", "svn": "0", "swz": "0", "syc": "0", "tgo": "0", "ton": "0", "twn": "0", "vct": "0", "wsm": "0"}
			
monthDic = {"bio": "19", "tmean": "12", "dtr": "12", "prec": "12"}

if sres == "baseline":
	
	model = "current"
	
	for country in countrytilelist:

		for var in monthDic:
			
			print "\n --> Check files: " + country,sres,model,period,var,"\n"
			tifdir = dirbase + "\\" + sres + "\\" + country + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"

			for tile in range(0, int(checkDic [country]) + 1, 1):
			
				for month in range(1, int(monthDic [var]) + 1, 1):
					
					tiff = tifdir + "\\" + sres + "_" + period + "_" + model + "_" + var + "_" + str(month) + "_" + str(tile) + ".tif"
					
					if not os.path.exists(tiff):
						print country, model, os.path.basename(tiff), "NO"
						outFile = open(dirbase + "\\" + sres + "_check.txt", "a")
						outFile.write(country + "\t" + model + "\n")
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

				for tile in range(0, int(checkDic [country]) + 1, 1):
				
					for month in range(1, int(monthDic [var]) + 1, 1):
						
						tiff = tifdir + "\\" + sres + "_" + period + "_" + model + "_" + var + "_" + str(month) + "_" + str(tile) + ".tif"
						
						if not os.path.exists(tiff):
							print country,model,os.path.basename(tiff),"NO"
							outFile = open(dirbase + "\\" + sres + "_check.txt", "a")
							outFile.write(country + "\t" + model + "\t" + "\n")
							outFile.close()
							
						else:
							print country,model,os.path.basename(tiff),"OK"
						
		print "\n",country," checked!"
		
print "\n Checking " + sres + " done!"
