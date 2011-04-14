import os, sys, glob, string, shutil
#python copy.py L:\PrecisData\archive\genac N:\PrecisData\archive\genac

dirbase = sys.argv[1]
dirout = sys.argv[2]

runidlist = sorted(os.listdir(dirbase))
for runid in runidlist:
	print dirbase + "\\" + str(runid) + "\n"
	filelist = sorted(glob.glob(dirbase + "\\" + str(runid) + "\\*"))
	if not os.path.exists(dirout + "\\" + str(runid)):
		os.system('mkdir ' + dirout + "\\" + str(runid))
	
	for file in filelist:

		if not os.path.exists(dirout + "\\" + str(runid) + "\\" + os.path.basename(file)):
			print file + " ---> " + dirout + "\\" + str(runid) + "\\" + os.path.basename(file)
			shutil.copyfile(file, dirout + "\\" + str(runid) + "\\" + os.path.basename(file))