import os, sys, glob, string, shutil
#python copy_website.py O:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_10min W:\data\data_requests\Donald_request\SRES_A1B\downscaled\Global_10min

dirbase = sys.argv[1]
dirout = sys.argv[2]

#modellist = sorted(os.listdir(dirbase))
modellist = "cccma_cgcm3_1_t47", "csiro_mk3_0", "csiro_mk3_5", "ipsl_cm4", "mpi_echam5", "ncar_ccsm3_0", "ukmo_hadcm3", "ukmo_hadgem1"

for model in modellist:
	print dirbase + "\\" + str(model) + "\n"
	periodlist = sorted(os.listdir(dirbase + "\\" + str(model)))
	for period in periodlist:
		print dirbase + "\\" + str(model) + "\\" + str(period) + "\n"
		if not os.path.exists(dirout + "\\" + str(model) + "\\" + str(period) + "\\_asciis"):
			os.system('mkdir ' + dirout + "\\" + str(model) + "\\" + str(period) + "\\_asciis")
		filelist = sorted(glob.glob(dirbase + "\\" + str(model) + "\\" + str(period) + "\\_asciis\\*_asc.zip"))
		for file in filelist:
			if not os.path.exists(dirout + "\\" + str(model) + "\\" + str(period) + "\\_asciis\\" + os.path.basename(file)):
				print file + " ---> " + dirout + "\\" + str(model) + "\\" + str(period) + "\\_asciis\\" + os.path.basename(file)
				shutil.copyfile(file, dirout + "\\" + str(model) + "\\" + str(period) + "\\_asciis\\" + os.path.basename(file))
		
		
		# shutil.copytree(dirbase + "\\" + str(model) + "\\" + str(period) + "\\_asciis", dirout + "\\" + str(model) + "\\" + str(period) + "\\_asciis")
		# # variablelist = sorted(os.listdir(dirbase + "\\" + str(model) + "\\" + str(period)))
		# for variable in variablelist:
			# if not variable == "_asciis" and not str(variable) == "*.aux":
				# shutil.copytree(dirbase + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable), dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable))
				
				# # filelist = sorted(glob.glob(dirbase + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\*"))
				# # if not os.path.exists(dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable)):
					# # os.system('mkdir ' + dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable))
					# # for file in filelist:
						# # if not os.path.exists(dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\" + os.path.basename(file)):
							# # print file + " ---> " + dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\" + os.path.basename(file)
							# # shutil.copyfile(file, dirout + "\\" + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\" + os.path.basename(file))