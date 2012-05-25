import os, sys, glob, string, shutil
# python ZipTheme4.py M:\climate_change\CCAFS_T4\D2_Derived_Monthly_Data M:\climate_change\CCAFS_T4\pattern_scaling_marksim M:\climate_change\CCAFS_T4\pattern_scaling_marksim
dirbase = sys.argv[1]
dirout = sys.argv[2]
finaldir = sys.argv[3]
# period = sys.argv[2]

# modellist = "cnr", "csi", "ech", "mir"
scenariolist = "a1b", "a2", "b1"
variableList = "prec", "solar_radiation", "tmax", "tmin"
# for model in modellist:
# for scenario in scenariolist:
	
	# # basename = model + "_" + scenario + "_" + period
	# # print "..compressing " + basename
	# dsList = glob.glob(dirbase + "\\SRES_" + scenario)
	# for ds in dsList:
		# modelList = glob.glob(ds + "\\*")
		# for model in modelList:
			# zipList = glob.glob(model + "\\*")
			# for zip in zipList:
				# print zip
				# if os.path.basename(zip)[-7:-4] == "asc":
					# print model
					# os.system('7za e -yo' + model + " " + zip)
					# # os.system("7za x " + zip + "-o" + ds)
					# diroutZip = dirout + "\\sres_" + scenario + "\\" + os.path.basename(model) + "\\" + os.path.basename(zip)[-12:-8] + "\\5min"
					# if not os.path.exists(diroutZip):
						# os.system('mkdir ' + diroutZip)
					# for variable in variableList:
						# ascList = glob.glob(model + "\\*" +  variable + "*.asc")
						# for asc in ascList:
							# print "Compressing.. " + asc
							# if variable == "rain" and not variable  == "rainydays":
								# os.system("7za a " + diroutZip + "\\" + os.path.basename(model) + "_" + scenario + "_" + os.path.basename(zip)[-12:-8] + "_prec_5min_no_tile_asc.zip " + asc)
								# os.remove(asc)
							# else:
								# os.system("7za a " + diroutZip + "\\" + os.path.basename(model) + "_" + scenario + "_" + os.path.basename(zip)[-12:-8] + "_" + variable + "_5min_no_tile_asc.zip " + asc)
								# os.remove(asc)

for scenario in scenariolist:
	dsList = glob.glob(finaldir + "\\SRES_" + scenario)
	for ds in dsList:
		modelList = glob.glob(ds + "\\*")
		for model in modelList:
			if not os.path.basename(model) == "2030s" or not os.path.basename(model) == "2050s" or not os.path.basename(model) == "2080s":
				periodList = glob.glob(model + "\\*")
				for period in periodList:
					variable = "rainydays"
					# for variable in variableList:
					inZip = finaldir + "\\sres_" + scenario + "\\" + os.path.basename(model) + "\\" + str(os.path.basename(period)) + "\\5min\\" + os.path.basename(model) + "_" + scenario + "_" + os.path.basename(period) + "_" + variable + "_5min_no_tile_asc.zip"
					if os.path.exists(inZip):
						print inZip
						if os.path.basename(model) == "echam5":
							outDir = finaldir + "\\sres_" + scenario + "\\" + str(os.path.basename(period)) + "\\mpi_" + os.path.basename(model) + "\\5min"
							outZip = outDir + "\\mpi_" + os.path.basename(model) + "_sres_" + scenario + "_" + os.path.basename(period) + "_" + variable + "_5min_no_tile_asc.zip"					
						else:
							outDir = finaldir + "\\sres_" + scenario + "\\" + str(os.path.basename(period)) + "\\" + os.path.basename(model) + "\\5min"
							outZip = outDir + "\\" + os.path.basename(model) + "_sres_" + scenario + "_" + os.path.basename(period) + "_" + variable + "_5min_no_tile_asc.zip"
						if not os.path.exists(outDir):
							os.system('mkdir ' + outDir)
						shutil.move(inZip, outZip)
						
						
							