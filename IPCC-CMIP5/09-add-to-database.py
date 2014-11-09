# python 09-add-to-database.py S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled D:\downscaled-tiles-files.txt
# python 09-add-to-database.py T:\gcm\cmip5\ipcc_5ar_ciat_tiled G:\jetarapues\downscaled-tiles-files.txt

import sys, os, string,glob, shutil

dirbase = sys.argv[1]
dirouttxt = sys.argv[2]

format = "2" ## asc
method = "1" ##Delta Method
fileset = "12" ##IPCC 5AR (CIAT)
region = "2" ##tile
count = 108103 ##Last file added + 1

if not os.path.exists(dirouttxt):
	dirouttxt = open(dirouttxt,'w')
	dirouttxt.write("id"+ "\t"+ "name"+ "\t"+ "method_id"+ "\t"+ "scenario_id"+ "\t"+ "period_id"+ "\t"+ "model_id"+ "\t"+ "variable_id"+ "\t"+ "resolution_id"+ "\t"+ "format_id"+ "\t"+ "file_set_id"+ "\t"+ "region_id"+ "\t"+ "tiles"+ "\t"+ "local_url"+ "\t"+ "availability_status_id"+ "\n")
	dirouttxt.close()



resDc = {"30s": "1"} #{"30s": "1", "2_5min": "2", "5min": "3", "10min": "4"} #{"2_5min": "2", "5min": "3", "10min": "4", "30min": "5", "25min": "6", "20min": "7"}#{"30s": "1", "2_5min": "2", "5min": "3", "10min": "4", "30min": "5", "25min": "6", "20min": "7"}

sresDc = {"rcp2_6": "7", "rcp4_5": "8", "rcp6_0": "9", "rcp8_5": "10"}

periodDc = {"2030s" : "4", "2050s" : "6", "2070s" : "8" , "2080s" : "9"}

varlist = "bio", "cons", "prec", "tmax", "tmean", "tmin"

zoneDc = {"A1" : "1", "A2" : "2", "A3" : "3", "A4" : "4", "A5" : "5", "A6" : "6", "B1" : "7", "B2" : "8", "B3" : "9", 
	"B4" : "10", "B5" : "11", "B6" : "12", "C1" : "13", "C2" : "14", "C3" : "15", "C4" : "16", "C5" : "17", "C6" : "18"}

modelDc = {"baseline" : "1", "bccr_bcm2_0" : "2", "cccma_cgcm2_0" : "3", "cccma_cgcm3_1_t47" : "4", "cccma_cgcm3_1_t63" : "5", 
	"cnrm_cm3" : "6", "csiro_mk2" : "7", "csiro_mk3_0" : "8", "gfdl_cm2_0" : "9", "gfdl_cm2_1" : "10", "giss_aom" : "11", 
	"hccpr_hadcm3" : "12", "iap_fgoals1_0_g" : "13", "ipsl_cm4" : "14", "miroc3_2_hires" : "15", "miroc3_2_medres" : "16", 
	"miub_echo_g" : "17", "mpi_echam5" : "18", "mri_cgcm2_3_2a" : "19", "ncar_ccsm3_0" : "20", "ncar_pcm1" : "21", 
	"nies99" : "22", "ukmo_hadcm3" : "23", "ukmo_hadgem1" : "24", "csiro_mk3_5" : "25", "giss_model_eh" : "26", 
	"giss_model_er" : "27", "ingv_echam4" : "28", "mpi_echam4" : "29", "mpi_echam5" : "30", "mohc_hadcm3q0" : "31", 
	"mohc_hadcm3q3" : "32", "mohc_hadcm3q16" : "33", "mohc_hadam3p_2" : "34", "mohc_hadam3p_3" : "35", "ncep_r2" : "36", "inm_cm3_0" : "37",
	
	"hadcm_cntrl" : "38", "hadcm_high" : "39", "hadcm_low" : "40", "hadcm_midi" : "41", "bcc_csm1_1" : "42", "bcc_csm1_1_m" : "43", "bnu_esm" : "44",
	 "cccma_cancm4" : "45", "cccma_canesm2" : "46", "cesm1_bgc" : "47", "cesm1_cam5" : "48", "cesm1_cam5_1_fv2" : "49", "cesm1_fastchem" : "50", 
	 "cesm1_waccm" : "51", "cmcc_cesm" : "52", "cmcc_cm" : "53", "cmcc_cms" : "54", "cnrm_cm5" : "55", "csiro_access1_0" : "56", "csiro_access1_3" : "57", 
	 "csiro_mk3_6_0" : "58", "ec_earth" : "59", "fio_esm" : "60", "gfdl_cm2_1" : "61", "gfdl_cm3" : "62", "gfdl_esm2g" : "63", "gfdl_esm2m" : "64", 
	 "giss_e2_h" : "65", "giss_e2_h_cc" : "66", "giss_e2_r" : "67", "giss_e2_r_cc" : "68", "inm_cm4" : "69", "ipsl_cm5a_lr" : "70", "ipsl_cm5a_mr" : "71", 
	 "ipsl_cm5b_lr" : "72", "lasg_fgoals_g2" : "73", "lasg_fgoals_s2" : "74", "miroc_esm" : "75", "miroc_esm_chem" : "76", "miroc_miroc4h" : "77", 
	 "miroc_miroc5" : "78", "mohc_hadcm3" : "79", "mohc_hadgem2_cc" : "80", "mohc_hadgem2_es" : "81", "mpi_esm_lr" : "82", "mpi_esm_mr" : "83", 
	 "mpi_esm_p" : "84", "mri_cgcm3" : "85", "ncar_ccsm4" : "86", "ncc_noresm1_m" : "87", "ncc_noresm1_me" : "88", "nimr_hadgem2_ao" : "89"	}
	 

varDc = {"bio" : "1", "prec" : "2", "tmax" : "3", "tmean" : "4", "tmin" : "5", "dtr" : "6", "solar_radiation" : "7", 
	"rainydays" : "8", "cloudam" : "9", "evcr" : "10", "evpotf1" : "11", "evpotf2" : "12", "evpotr" : "13", 
	"evss" : "14", "press" : "15", "rhum" : "16", "shum" : "17", "slheat" : "18", "soilmaf" : "19", "soilmrz" : "20", 
	"subsr" : "21", "transr" : "22", "tsmean" : "23", "tsmmax" : "24", "tsmmin" : "25", "wsmean" : "26", "wsmmax" : "27", "cons" : "28"}


### Global
for sres in sorted(sresDc):
	for period in sorted(periodDc):
		modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\" + period))
		for model in modellist:
			for res in sorted(resDc):		
				for var in varlist:
					for zone in zoneDc:

						indir = dirbase + "\\" + sres + "\\" + period + "\\" + model + "\\" + res
					
						outfilename = model + "_" + sres + "_" + period  + "_" + var + "_30s_tile_" + zone.lower() + "_asc.zip"
						# outfilename = model + "_" + sres + "_" + period  + "_" + var + "_" + res  + "_r1i1p1_no_tile" + "_grd.zip"
						localurl = "/data/ipcc_5ar_ciat_tiled/" + sres + "/" + period + "/" + model + "/" + res
						if os.path.exists(indir + "\\" + outfilename):
							availstatus = "2"
						else: 						
							availstatus = "1"
						print outfilename
						txt = open(dirouttxt,'a')
						# txt.write(str(count)+ "\t"+ outfilename+ "\t"+ method+ "\t"+ sresDc[sres]+ "\t"+ periodDc[period]+ "\t"+ modelDc[model]+ "\t"+ varDc[var]+ "\t"+ resDc[res]+ "\t"+ format+ "\t"+ fileset+ "\t"+ region+ "\t0"+ "\t"+ localurl+ "\t"+ availstatus + "\n")
						txt.write(str(count)+ "\t"+ outfilename+ "\t"+ method+ "\t"+ sresDc[sres]+ "\t"+ periodDc[period]+ "\t"+ modelDc[model]+ "\t"+ varDc[var]+ "\t"+ resDc[res]+ "\t"+ format+ "\t"+ fileset+ "\t"+ region+ "\t"+zoneDc[zone]+ "\t"+ localurl+ "\t"+ availstatus + "\n")
						count = count + 1					
					
					
# txt.close()
print "Add to database ccafs-climate process done!"