import os, sys, glob, string, shutil
#python copy.py L:\PrecisData\archive N:\PrecisData\archive

dirbase = sys.argv[1]
dirout = sys.argv[2]

folderlist = sorted(os.listdir(dirbase))

for folder in folderlist:
	print dirbase + "\\" + str(folder) + "\n"
	# tmplist = sorted(glob.glob(dirbase + "\\" + str(folder) + "\\*"))
	if not os.path.exists(dirout + "\\" + str(folder)):
		os.system('mkdir ' + dirout + "\\" + str(folder))
		
	# for tmp in tmplist:

		# if not os.path.exists(dirout + "\\" + str(folder) + "\\" + os.path.basename(tmp)):
			# print file + " ---> " + dirout + "\\" + str(folder) + "\\" + os.path.basename(tmp)
			# shutil.copyfile(file, dirout + "\\" + str(folder) + "\\" + os.path.basename(tmp))	
		
		
	runidlist = sorted(os.listdir(dirbase + "\\" + str(folder)))
	for runid in runidlist:
		print dirbase + "\\" + str(folder) + "\\" + str(runid) + "\n"
		filelist = sorted(glob.glob(dirbase + "\\" + str(folder) + "\\" + str(runid) + "\\*"))
		if not os.path.exists(dirout + "\\" + str(folder) + "\\" + str(runid)):
			os.system('mkdir ' + dirout + "\\" + str(folder) + "\\" + str(runid))
		
		for file in filelist:

			if not os.path.exists(dirout + "\\" + str(folder) + "\\" + str(runid) + "\\" + os.path.basename(file)):
				print file + " ---> " + dirout + "\\" + str(folder) + "\\" + str(runid) + "\\" + os.path.basename(file)
				shutil.copyfile(file, dirout + "\\" + str(folder) + "\\" + str(runid) + "\\" + os.path.basename(file))