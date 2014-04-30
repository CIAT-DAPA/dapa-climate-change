import os, sys, glob, string
# python CompressFiles.py U:\rcm\precis\boundary_conditions
dirbase = sys.argv[1]

dsList = glob.glob(dirbase + "\\*")
for ds in dsList:
	flList = glob.glob(ds)
	for fl in flList:
		inZip = ds + "\\_Compiled_asciis.zip"
		if not os.path.exists(inZip):
			print inZip
			os.system("7za a -mmt=4 " + inZip + " " + fl)
	for fl in flList:
		os.remove(fl)
