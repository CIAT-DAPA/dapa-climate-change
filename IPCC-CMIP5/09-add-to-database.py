# python AddToDatabase.py S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled D:\CIAT\Portals\CCAFS_Climate\v5\downscaled-tiles-files.txt

dirbase = sys.argv[1]
dirouttxt = sys.argv[2]

format = "1" ## asc
method = "1" ##Delta Method
fileset = "4" ##IPCC 4AR (CIAT)
region = "1" ##global
count = 90296 ##Last file added + 1

resDc = {"30s": "1", "2_5min": "2", "5min": "3", "10min": "4", "30min": "5", "25min": "6", "20min": "7"}

sresDc = {"sres_a1b": "1", "sres_a2": "5", "sres_b1": "6"}

periodDc = {"2020s" : "3", "2030s" : "4", "2040s": "5", "2050s" : "6", "2060s" : "7", "2070s" : "8" , "2080s" : "9"}

varlist = "bio", "cons", "prec", "tmax", "tmean", "tmin"

zoneDc = {"A1" : "1", "A2" : "2", "A3" : "3", "A4" : "4", "A5" : "5", "A6" : "6", "B1" : "7", "B2" : "8", "B3" : "9", 
	"B4" : "10", "B5" : "11", "B6" : "12", "C1" : "13", "C2" : "14", "C3" : "15", "C4" : "16", "C5" : "17", "C6" : "18"}

modelDc = {"baseline" : "1", "bccr_bcm2_0" : "2", "cccma_cgcm2_0" : "3", "cccma_cgcm3_1_t47" : "4", "cccma_cgcm3_1_t63" : "5", 
	"cnrm_cm3" : "6", "csiro_mk2" : "7", "csiro_mk3_0" : "8", "gfdl_cm2_0" : "9", "gfdl_cm2_1" : "10", "giss_aom" : "11", 
	"hccpr_hadcm3" : "12", "iap_fgoals1_0_g" : "13", "ipsl_cm4" : "14", "miroc3_2_hires" : "15", "miroc3_2_medres" : "16", 
	"miub_echo_g" : "17", "mpi_echam5" : "18", "mri_cgcm2_3_2a" : "19", "ncar_ccsm3_0" : "20", "ncar_pcm1" : "21", 
	"nies99" : "22", "ukmo_hadcm3" : "23", "ukmo_hadgem1" : "24", "csiro_mk3_5" : "25", "giss_model_eh" : "26", 
	"giss_model_er" : "27", "ingv_echam4" : "28", "mpi_echam4" : "29", "mpi_echam5" : "30", "mohc_hadcm3q0" : "31", 
	"mohc_hadcm3q3" : "32", "mohc_hadcm3q16" : "33", "mohc_hadam3p_2" : "34", "mohc_hadam3p_3" : "35", "ncep_r2" : "36", "inm_cm3_0" : "37"}

varDc = {"bio" : "1", "prec" : "2", "tmax" : "3", "tmean" : "4", "tmin" : "5", "dtr" : "6", "solar_radiation" : "7", 
	"rainydays" : "8", "cloudam" : "9", "evcr" : "10", "evpotf1" : "11", "evpotf2" : "12", "evpotr" : "13", 
	"evss" : "14", "press" : "15", "rhum" : "16", "shum" : "17", "slheat" : "18", "soilmaf" : "19", "soilmrz" : "20", 
	"subsr" : "21", "transr" : "22", "tsmean" : "23", "tsmmax" : "24", "tsmmin" : "25", "wsmean" : "26", "wsmmax" : "27", "cons" : "28"}

if not os.path.exists(dirouttxt):
	dirouttxt = open(dirouttxt,'w')
	dirouttxt.write("id", "\t", "name", "\t", "method_id", "\t", "scenario_id", "\t", "period_id", "\t", "model_id", "\t", "variable_id", "\t", "resolution_id", "\t", "format_id", "\t", "file_set_id", "\t", "region_id", "\t", "tiles", "\t", "local_url", "\t", "availability_status_id")
	dirouttxt.close()

### Global
for sres in sresDc:
	for period in periodDc:
		modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\" + period))
		for model in modellist:
			indir = dirbase + "\\" + sres + "\\" + period + "\\" + model + "\\30s"
			for var in varlist:
				for zone in zoneDc:
					outfilename = model + "_" + sres + "_" + period  + "_" + var + "_30s_tile_" + zone.lower() + "_asc.zip"
					localurl = "/data/ipcc_4ar_ciat_tiled/" + sres + "/" + period + "/" + model + "/30s"
					
					if os.path.exists(indir + "\\" + infilename):
						availstatus = "2"
					else: 						
						availstatus = "nofile"
					
					print infilename
					dirouttxt = open(dirouttxt,'a')
					dirouttxt.write(count, "\t", outfilename, "\t", method, "\t", sresDc[sres], "\t", periodDc[period], "\t", modelDc[model], "\t", varDc[var], "\t", res, "\t", format, "\t", fileset, "\t", region, "\t", zoneDc[zone], "\t", localurl, "\t", availstatus)
					
					count = count + 1

print "Add to database ccafs-climate process done!"
