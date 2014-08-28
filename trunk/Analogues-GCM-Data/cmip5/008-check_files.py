# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Check the tiles files created
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string, glob, grid2asciitiff_tiles
from arcpy import env


#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 008-check_files.py U:\portals\ccafs-analogues\TilesByCountry U:\portals\ccafs-analogues\TilesByCountry rcp26 30s 2020_2049 ckeck"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
rcp = sys.argv[3]
resolution = sys.argv[4]
period = sys.argv[5]
mode = sys.argv[6]

os.system('cls')
reslist = "30s", "2_5min" #, "5min", "10min"

# for resolution in reslist:

l = open("U:\portals\ccafs-analogues\mask\\"+"listcountries_"+resolution+".list", "r")	
countrytilelist = [i for i in l.readlines()]

if resolution == "30s":
	countryDic = {"ala": "1 1 ","alb": "1 1 ","and": "1 1 ","are": "1 1 ","arm": "1 1 ","atg": "1 1 ","aut": "1 1 ","aze": "1 1 ","cod": "6 6 ", "kna": "1 1 ", "mne": "1 1 ", "nru": "1 1 ", "plw": "1 1 ", "pse": "1 1 ", "srb": "1 1 ", "ssd": "3 3 ", "tls": "1 1 ", "tto": "1 1 ", "tuv": "1 1 ",\
				"btn": "1 1 ","brn": "1 1 ","brb": "1 1 ","bmu": "1 1 ","bih": "1 1 ","blz": "1 1 ","bhi": "1 1 ","bhr": "1 1 ","bhr": "1 1 ","bdi": "1 1 ", "bel": "1 1 ", "ben": "1 1 ", "bfa": "2 2 ", "bgd": "2 2 ", "bhs": "2 2 ", "blr": "2 2 ", "bwa": "2 2 ","bgr": "2 2 ", "civ": "2 2 ", "cmr": "2 2 ", "cog": "2 2 ", "cub": "2 2 ",\
				"che": "1 1 ","deu": "2 2 ", "dnk": "2 2 ", "eri": "2 2 ", "esh": "2 2 ", "gab": "2 2 ", "gha": "2 2 ", "gin": "2 2 ", "gnq": "2 2 ", "grc": "2 2 ", "guy": "2 2 ",\
				"hrv": "2 2 ", "irq": "2 2 ", "isl": "2 2 ", "ken": "2 2 ", "kgz": "2 2 ", "khm": "2 2 ", "kir": "2 2 ", "kor": "2 2 ", "lao": "2 2 ", "mus": "2 2 ",\
				"mwi": "2 2 ", "ncl": "2 2 ", "nic": "2 2 ", "npl": "2 2 ", "omn": "2 2 ", "pol": "2 2 ", "prk": "2 2 ", "pry": "2 2 ", "pyf": "2 2 ", "rom": "2 2 ",\
				"sen": "2 2 ", "sgp": "2 2 ", "slb": "2 2 ", "syr": "2 2 ", "tca": "2 2 ", "tjk": "2 2 ", "tun": "2 2 ", "uga": "2 2 ", "ury": "2 2 ",\
				"vut": "2 2 ", "yem": "2 2 ", "zwe": "2 2 ", "afg": "3 3 ", "ago": "3 3 ", "bol": "3 3 ", "caf": "3 3 ", "ecu": "3 3 ", "egy": "3 3 ",\
				"eth": "3 3 ", "fin": "3 3 ", "fra": "3 3 ", "gbr": "3 3 ", "ita": "3 3 ", "mar": "3 3 ", "mdg": "3 3 ", "mdv": "3 3 ",\
				"mmr": "3 3 ", "moz": "3 3 ", "mrt": "3 3 ", "mys": "3 3 ", "nam": "3 3 ", "ner": "3 3 ", "nga": "3 3 ", "phl": "3 3 ", "png": "3 3 ",\
				"som": "3 3 ", "swe": "3 3 ", "tcd": "3 3 ", "tha": "3 3 ", "tkm": "3 3 ", "tur": "3 3 ", "tza": "3 3 ", "ukr": "3 3 ", "uzb": "3 3 ",\
				"ven": "3 3 ", "vnm": "3 3 ", "zmb": "3 3 ", "col": "4 4 ", "esp": "4 4 ", "irn": "4 4 ", "lby": "4 4 ", "mli": "4 4 ", "mlt": "4 4 ",\
				"mng": "4 4 ", "nor": "4 4 ", "pak": "4 4 ", "per": "4 4 ", "prt": "4 4 ", "sau": "4 4 ", "sdn": "4 4 ", "dza": "5 5 ", "mex": "5 5 ", "sjm": "5 5 ",\
				"zaf": "5 5 ", "arg": "2 2 ", "idn": "2 2 ", "jpn": "1 1 ", "kaz": "2 2 ", "ind": "2 2 ", "bra": "3 3 ", "chl": "3 3 ",\
				"grl": "3 3 ", "aus": "3 3 ", "chn": "3 3 ", "fji": "1 1 ", "can": "4 4 ", "nzl": "5 5 ", "rus": "7 7 ", "usa": "8 8 ",\
	"ala": "1 1 ", "alb": "1 1 ", "asm": "1 1 ", "and": "1 1 ", "aia": "1 1 ", "atg": "1 1 ", "arm": "1 1 ", "abw": "1 1 ", "bhs": "1 1 ", "bhr": "1 1 ", "brb": "1 1 ", "bel": "1 1 ", "blz": "1 1 ", "bmu": "1 1 ", "btn": "1 1 ", "bih": "1 1 ", "bvt": "1 1 ", "iot": "1 1 ", "vgb": "1 1 ", "brn": "1 1 ", "bdi": "1 1 ", "cpv": "1 1 ", "cym": "1 1 ", "cxr": "1 1 ", "cck": "1 1 ", "com": "1 1 ", "cok": "1 1 ", "cri": "1 1 ", "cyp": "1 1 ", "dji": "1 1 ", "dma": "1 1 ", "slv": "1 1 ", "imn": "1 1 ", "isr": "1 1 ", "gnq": "1 1 ", "flk": "1 1 ", "fji": "1 1 ", "fro": "1 1 ", "pyf": "1 1 ", "atf": "1 1 ", "gmb": "1 1 ", "gib": "1 1 ", "grd": "1 1 ", "glp": "1 1 ", "gum": "1 1 ", "ggy": "1 1 ", "gnb": "1 1 ", "hti": "1 1 ", "hmd": "1 1 ", "hkg": "1 1 ", "jam": "1 1 ", "jey": "1 1 ", "kir": "1 1 ", "kwt": "1 1 ", "lbn": "1 1 ", "lso": "1 1 ", "lie": "1 1 ", "lux": "1 1 ", "mac": "1 1 ", "mkd": "1 1 ", "mdv": "1 1 ", "mlt": "1 1 ", "mhl": "1 1 ", "mtq": "1 1 ", "mus": "1 1 ", "myt": "1 1 ", "fsm": "1 1 ", "mda": "1 1 ", "mco": "1 1 ", "mne": "1 1 ", "msr": "1 1 ", "ncl": "1 1 ", "niu": "1 1 ", "nfk": "1 1 ", "mnp": "1 1 ", "plw": "1 1 ", "pse": "1 1 ", "pcn": "1 1 ", "pri": "1 1 ", "qat": "1 1 ", "nru": "1 1 ", "nld": "1 1 ", "ant": "1 1 ", "dom": "1 1 ", "reu": "1 1 ", "rwa": "1 1 ", "shn": "1 1 ", "kna": "1 1 ", "spm": "1 1 ", "sgs": "1 1 ", "sp-": "1 1 ", "lka": "1 1 ", "che": "1 1 ", "swz": "1 1 ", "twn": "1 1 ", "tls": "1 1 ", "tgo": "1 1 ", "tkl": "1 1 ", "ton": "1 1 ", "tto": "1 1 ", "tca": "1 1 ", "tuv": "1 1 ", "umi": "1 1 ", "vut": "1 1 ", "vat": "1 1 ", "vir": "1 1 ", "wlf": "1 1 ", "ko-": "1 1 ", "vct": "1 1 ", "wsm": "1 1 ", "smr": "1 1 ", "lca": "1 1 ", "stp": "1 1 ", "syc": "1 1 ", "sle": "1 1 ", "sgp": "1 1 ", "svk": "1 1 ", "svn": "1 1 ", "slb": "1 1 ",\
	"cze": "2 2 ","est": "2 2 ","geo": "2 2 ","gtm": "2 2 ","hnd": "2 2 ","guf": "2 2 ","hun": "2 2 ","irl": "2 2 ","jor": "2 2 ","lbr": "2 2 ","ltu": "2 2 ","pan": "1 1 ","lva": "2 2 ",
				}
	
elif resolution == "2_5min":
	countryDic = {"af": "4 4 ", "as": "6 6 ", "eu": "3 3 ", "na": "5 5 ", "oc": "3 3 ", "sa": "4 4 "}
elif resolution == "5min":
	countryDic = {"af": "2 2 ", "as": "3 3 ", "eu": "2 2 ", "na": "3 3 ", "oc": "2 2 ", "sa": "2 2 "}
elif resolution == "10min":
	countryDic = {"af": "1 1 ", "as": "2 2 ", "eu": "1 1 ", "na": "1 1 ", "oc": "1 1 ", "sa": "1 1 "}
	
	
monthDic = {"bio": "19", "tmean": "12", "dtr": "12", "prec": "12"}

if mode =="check":
	if rcp == "baseline":
		
		model = "baseline"
		period = "1960_1990"
		for country in countrytilelist:

			for var in monthDic:
				
				print "\n --> Check files: " + country,rcp,model,period,var,"\n"
				tifdir = dirbase + "\\" + rcp + "\\" + country[:-5] + "_" + resolution + "\current\\" + "\\" + var + "_tif"
				for tile in range(0, int(int(str(countryDic [country[:-5]]).split(" ")[0]) * int(str(countryDic [country[:-5]]).split(" ")[1]) - 1) + 1, 1):
				
					for month in range(1, int(monthDic [var]) + 1, 1):
						
						tiff = tifdir + "\\" + rcp + "_" + period + "_current_" + var + "_" + str(month) + "_" + str(tile) + ".tif"
						print tiff
						if not os.path.exists(tiff):
							print country, model, os.path.basename(tiff), "NO"
							outFile = open(dirbase + "\\" + rcp +'_'+resolution+"_check.txt", "a")
							outFile.write(country.split("\n")[0] + "\t" + model + "\t" + resolution + "\t" + var+ "_" + str(month) + "\n")
							outFile.close()
							
						else:
							print country, model, os.path.basename(tiff), "OK"
						
	else:
		for country in countrytilelist:

			if rcp=='rcp26':
				modellist = ['ensemble','bcc_csm1_1', 'bcc_csm1_1_m', 'bnu_esm', 'cccma_canesm2', 'cesm1_cam5', 'csiro_mk3_6_0', 'fio_esm', 'gfdl_cm3', 'gfdl_esm2g', 'gfdl_esm2m', 'giss_e2_h', 'giss_e2_r', 'ipsl_cm5a_lr', 'ipsl_cm5a_mr', 'lasg_fgoals_g2', 'miroc_esm', 'miroc_esm_chem', 'miroc_miroc5', 'mohc_hadgem2_es', 'mpi_esm_lr', 'mpi_esm_mr', 'mri_cgcm3', 'ncar_ccsm4', 'ncc_noresm1_m', 'nimr_hadgem2_ao']
			elif rcp=='rcp45':
				modellist = ['ensemble','bcc_csm1_1', 'bcc_csm1_1_m', 'bnu_esm', 'cccma_canesm2', 'cesm1_bgc', 'cesm1_cam5', 'csiro_access1_0', 'csiro_access1_3', 'csiro_mk3_6_0', 'fio_esm', 'gfdl_cm3', 'gfdl_esm2g', 'gfdl_esm2m', 'giss_e2_h_cc', 'giss_e2_r', 'giss_e2_r_cc', 'inm_cm4', 'ipsl_cm5a_lr', 'ipsl_cm5a_mr', 'lasg_fgoals_g2', 'miroc_esm', 'miroc_esm_chem', 'miroc_miroc5', 'mohc_hadgem2_cc', 'mohc_hadgem2_es', 'mpi_esm_lr', 'mri_cgcm3', 'ncar_ccsm4', 'ncc_noresm1_m', 'nimr_hadgem2_ao']	
			elif rcp=='rcp60':
				modellist = ['ensemble','bcc_csm1_1', 'bcc_csm1_1_m', 'cesm1_cam5', 'csiro_mk3_6_0', 'fio_esm', 'gfdl_cm3', 'gfdl_esm2g', 'gfdl_esm2m', 'giss_e2_h', 'giss_e2_r', 'ipsl_cm5a_lr', 'miroc_esm', 'miroc_esm_chem', 'miroc_miroc5', 'mohc_hadgem2_es', 'mri_cgcm3', 'ncar_ccsm4', 'ncc_noresm1_m', 'nimr_hadgem2_ao']	
			else:
				modellist = ['ensemble','bcc_csm1_1', 'bcc_csm1_1_m', 'bnu_esm', 'cccma_canesm2', 'cesm1_bgc', 'cesm1_cam5', 'csiro_access1_0', 'csiro_access1_3', 'csiro_mk3_6_0', 'fio_esm', 'ec_earth', 'gfdl_cm3', 'gfdl_esm2g', 'gfdl_esm2m', 'giss_e2_h', 'giss_e2_r', 'inm_cm4', 'ipsl_cm5a_lr', 'ipsl_cm5a_mr', 'lasg_fgoals_g2', 'miroc_esm', 'miroc_esm_chem', 'miroc_miroc5', 'mohc_hadgem2_cc', 'ipsl_cm5b_lr', 'mohc_hadgem2_es', 'mpi_esm_lr', 'mpi_esm_mr', 'mri_cgcm3', 'ncar_ccsm4', 'ncc_noresm1_m', 'nimr_hadgem2_ao']
			ens= 'r1i1p1'
			
			for model in modellist:
				
				for var in monthDic:
					
					print "\n --> Check files: " + country,rcp,model,period,var,"\n"
					tifdir = dirbase + "\\" + rcp + "\\" + country[:-5] + "_" + resolution + "\\" + model + "\\" + period + "\\" + var + "_tif"
					for tile in range(0, int(int(str(countryDic [country[:-5]]).split(" ")[0]) * int(str(countryDic [country[:-5]]).split(" ")[1]) - 1) + 1, 1):
					
						for month in range(1, int(monthDic [var]) + 1, 1):
							
							tiff = tifdir + "\\" + rcp + "_" + period + "_" + model + "_" + var + "_" + str(month) + "_" + str(tile) + ".tif"
							
							if not os.path.exists(tiff):
								print country,model,os.path.basename(tiff),"NO"
								outFile = open(dirbase + "\\" + rcp +'_'+resolution+"_check.txt", "a")
								outFile.write(country.split("\n")[0] + "\t" + model + "\t" + resolution + "\t" + var+ "_" + str(month) + "\n")
								outFile.close()
								
							else:
								print country,model,os.path.basename(tiff),"OK"
							
			print "\n",country," checked!"
		
	print "\n Checking " + rcp + " done!"

else:	
	l = open(dirbase + "\\" + rcp +'_'+resolution+"_check.txt", "r")	
	check = [i for i in l.readlines()]

	print '----------- Fix check -----------------------\n'
	print list(set(check))
	print '\n-------------------------------------\n'

	for item in  list(set(check)):
		country = (item.split('\t')[0]).split("_")[0]
		model = item.split('\t')[1]
		resolution = item.split('\t')[2]
		var = item.split('\t')[3].split("\n")[0]
		
		print " fixing...", rcp, country, resolution, model, var
		input = "U:\portals\ccafs-analogues\grid_files\\" + rcp +"\\"+ country + "_" + str(resolution)
		out = dirout + "\\" + rcp +"\\"+ country + "_" + str(resolution)
		
		if model == "current":
			arcpy.env.workspace = input + "\\" + model
			diroutGrids = dirout + "\\baseline_new\\" + country + "_" + str(resolution) + "\\" + model + "\\1960_1990"
		else:
			arcpy.env.workspace = input + "\\" + model + "\\" + str(period)
			diroutGrids = out + "\\" + model + "\\" + str(period)
			
		# print "\nProcessing",country,model,period,"\n"
		
		raster = input + "\\" + model + "\\" + str(period)+ "\\" +var
		
		# rasterList = arcpy.ListRasters(var+"*", "GRID")
		# for raster in rasterList:
		if os.path.basename(raster).split("_")[0] == "bio" or os.path.basename(raster).split("_")[0] == "prec" or os.path.basename(raster).split("_")[0] == "tmean" or os.path.basename(raster).split("_")[0] == "dtr":
			diroutGridsVar = diroutGrids + "\\" + os.path.basename(raster).split("_")[0]+"_tif"
			if not os.path.exists(diroutGridsVar):
				os.system('mkdir ' + diroutGridsVar)
			tileTif= rcp+"_"+period+"_"+model+"_"+raster + "_" + str(int(str(countryDic [country]).split(" ")[0]) * int(str(countryDic [country]).split(" ")[1]) - 1)+".tif"
			if not arcpy.Exists(diroutGridsVar + "\\" + tileTif):
				print "\tspliting .. ",raster
				arcpy.SplitRaster_management(raster, diroutGridsVar, rcp+"_"+period+"_"+model+"_"+raster + "_", "NUMBER_OF_TILES", "TIFF", "#", str(countryDic [country]), "#", "0", "PIXELS", "#", "#")
				print "\t" + raster,"tiled"					
			else:
				print "\t" + raster,"tiled"		
		
		
		
		
