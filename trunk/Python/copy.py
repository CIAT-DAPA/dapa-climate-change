import os, sys, glob, string, shutil
#python copy.py O:\climate_change\IPCC_CMIP3\SRES_B1\downscaled\Global_2_5min T:\data\data_requests\Downscaled_30s_B1_2050s_allmodels_global

dirbase = sys.argv[1]
dirout = sys.argv[2]

modellist = sorted(os.listdir(dirbase))

for model in modellist:
	print dirbase + "\\" + str(model) + "\n"
	if not os.path.exists(dirout + "\\" + str(model) + "\\2040_2069"):
		os.system('mkdir ' + dirout + "\\" + str(model) + "\\2020_2049")
		os.system("xcopy /fe " +  dirbase + "\\" + str(model) + "\\2020_2049 " + dirout + "\\" + str(model)+ "\\2020_2049" )
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

dirbase = sys.argv[1]
dirout = sys.argv[2]

filelist = "035570", "035590", "035660", "035671", "082840", "082850", "154910", "155350", "401010", "401020", "401760", "401790", "414520", "414660", "430690", "474120", "474790", "483260", "483270", "619310", "636540", "636800", "637050", "637260", "643110", "643830", "643840", "643870", "643900", "663180", "673310", "673410", "673460", "674130", "674750", "674760", "675800", "675810", "676610", "676670", "677740", "677750", "677850", "678770", "682674", "683500", "683510", "683630", "683910", "683920", "683950", "683960", "684150", "684963", "685800", "685880", "685930", "713520", "718330", "722125", "722159", "724697", "724769", "725155", "726379", "760754", "763501", "764120", "764125", "764580", "764591", "765480", "765490", "765560", "766123", "766130", "766250", "766534", "766535", "766910", "766920", "766980", "767980", "783290", "783320", "783970", "783990", "784090", "784092", "784370", "784570", "784600", "784645", "784700", "784786", "785830", "786150", "786250", "786310", "786372", "786390", "786400", "786475", "786500", "786520", "786550", "786620", "786630", "787080", "787140", "787200", "787280", "787310", "787320", "787340", "787400", "787410", "787440", "787500", "787605", "787606", "787620", "787740", "787930", "800930", "801100", "801481", "802100", "802590", "803080", "803420", "804100", "804103", "804130", "804220", "804233", "804270", "804413", "804720", "829900", "830970", "831820", "833760", "833980", "834190", "835310", "835500", "835970", "836170", "836180", "836420", "836460", "836723", "836810", "836870", "837210", "837290", "837320", "837410", "837480", "837660", "838130", "838260", "838370", "838510", "838570", "838830", "839480", "840120", "840260", "841110", "841250", "841290", "841400", "841890", "842260", "844520", "845010", "845645", "845700", "846285", "846290", "846700", "846730", "846860", "846910", "852050", "852230", "852440", "852450", "852640", "855740", "855770", "856720", "857040", "861840", "861850", "862000", "862170", "862200", "862210", "862230", "862340", "862470", "862680", "862940", "862970", "865750", "865850", "870160", "870460", "870470", "870520", "871200", "871210", "871870", "873440", "873450", "997542", "998003", "999999"

for year in range(1975, 1990 + 1, 1):
	for file in filelist:
		if os.path.exists(dirbase + "\\" + str(year) + "\\formatted\\stations\\" + str(file) + "-99999-" + str(year) + ".op.gz.csv"):
			print "Copying " + str(year) + " " + file
			shutil.copyfile(dirbase + "\\" + str(year) + "\\formatted\\stations\\" + str(file) + "-99999-" + str(year) + ".op.gz.csv", dirout + "\\" + str(year) + "_GSOD" + str(file) + ".csv")