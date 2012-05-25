import os, sys, glob, string, shutil
#python copy.py O:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_2_5min H:\Downscaled_2_5min_A1B_2050s_allmodels_global

dirbase = sys.argv[1]
dirout = sys.argv[2]

modellist = sorted(os.listdir(dirbase))

for model in modellist:
	print dirbase + "\\" + str(model) + "\n"
	if not os.path.exists(dirout + "\\" + str(model) + "\\2040_2069\\_asciis"):
		os.system('mkdir ' + dirout + "\\" + str(model) + "\\2020_2049\\_asciis")
		os.system("robocopy " +  dirbase + "\\" + str(model) + "\\2020_2049\\_asciis " + dirout + "\\" + str(model)+ "\\2020_2049\\_asciis /Z /E /XF *grd.zip*")
	# # periodlist = sorted(os.listdir(dirbase + "\\" + str(model)))
	# # for period in periodlist:
		# # print dirbase + "\\" + str(model) + "\\" + str(period) + "\n"
		# # shutil.copytree(dirbase + "\\" + str(model) + "\\" + str(period), dirout + "\\" + str(model) + "\\" + str(period))
		# # # variablelist = sorted(os.listdir(dirbase + "\\" + str(model) + "\\" + str(period)))
		# # for variable in variablelist:
			# # if not variable == "_asciis" and not str(variable) == "*.aux":
				# # shutil.copytree(dirbase + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable), dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable))
				
				# # # filelist = sorted(glob.glob(dirbase + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\*"))
				# # # if not os.path.exists(dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable)):
					# # # os.system('mkdir ' + dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable))
					# # # for file in filelist:
						# # # if not os.path.exists(dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\" + os.path.basename(file)):
							# # # print file + " ---> " + dirout + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\" + os.path.basename(file)
							# # # shutil.copyfile(file, dirout + "\\" + "\\" + str(model) + "\\" + str(period) + "\\" + str(variable) + "\\" + os.path.basename(file))
							

							
# python copy.py K:\CCAFS\climate-data-assessment\comparisons\input-data\gsod-weather-stations D:\Workspace\Jagath\gsod_daily_stations

# dirbase = sys.argv[1]
# dirout = sys.argv[2]

# for year in range(1975, 1990 + 1, 1):
	# for file in filelist:
		# if os.path.exists(dirbase + "\\" + str(year) + "\\formatted\\stations\\" + str(file) + "-99999-" + str(year) + ".op.gz.csv"):
			# print "Copying " + str(year) + " " + file
			# shutil.copyfile(dirbase + "\\" + str(year) + "\\formatted\\stations\\" + str(file) + "-99999-" + str(year) + ".op.gz.csv", dirout + "\\" + str(year) + "_GSOD" + str(file) + ".csv")