import os, sys, glob, string
# python CompressFolders.py D:\Workspace\Request_Jacob\SRES_A2_2_5min
dirbase = sys.argv[1]

dsList = glob.glob(dirbase + "\\*")
for ds in dsList:
	inZip = ds + ".zip"
	if not os.path.exists(inZip):
		print inZip
		os.system("7za a " + inZip + " " + ds)
