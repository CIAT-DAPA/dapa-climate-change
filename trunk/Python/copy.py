import os, sys, glob, string, shutil
#python copy.py O:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_2_5min O:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_2_5min

dirbase = sys.argv[1]
dirout = sys.argv[2]

modellist = sorted(os.listdir(dirbase))

for model in modellist[:]:
	print dirbase + "\\" + str(model) + "\n"
	if not os.path.exists(dirout + "\\" + str(model) + "\\2020_2049"):
		os.system('mkdir ' + dirout + "\\" + str(model) + "\\2020_2049")
		os.system("xcopy /fe " +  dirbase + "\\" + str(model) + "\\2020_2049 " + dirout + "\\" + str(model)+ "\\2020_2049" )
	# periodlist = sorted(os.listdir(dirbase + "\\" + str(model)))
	# for period in periodlist:
		# print dirbase + "\\" + str(model) + "\\" + str(period) + "\n"
		# shutil.copytree(dirbase + "\\" + str(model) + "\\" + str(period), dirout + "\\" + str(model) + "\\" + str(period))
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