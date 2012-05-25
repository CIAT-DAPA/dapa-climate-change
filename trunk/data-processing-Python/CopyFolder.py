import os, sys, glob, string, shutil
#python ZipFolder.py F:\IPCC_CMIP3

dirbase = sys.argv[1]
scenarioList = "SRES_A1B", "SRES_A2", "SRES_B1"
modelList = "cccma_cgcm3_1_t47" , "csiro_mk3_5" , "gfdl_cm2_1", "giss_model_er", "ncar_ccsm3_0", "ukmo_hadcm3"
periodList = "2010_2039", "2040_2069", "2070_2099"

for scenario in scenarioList:

	for model in modelList:
	
		for period in periodList:
			
			dirinput = dirbase + "\\" + scenario + "\\downscaled\\Global_30s\\" + model 
			if not os.path.exists(dirinput + "\\" + period + ".zip"):
				print dirinput
				InZip = dirbase + "\\" + scenario + "\\downscaled\\Global_30s\\" + model + "\\" + period
				os.system("7za a " + InZip + ".zip" " " + InZip)
				os.system("del " + InZip)