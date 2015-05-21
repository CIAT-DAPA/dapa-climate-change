import os, sys, glob, string
# python CompressFolders.py D:\CIAT\CCAFS\Workshops\ecocrop-training-mozambique\Final_Course\day_5\climate_data\future
dirbase = sys.argv[1]

dsList = glob.glob(dirbase + "\\*")
for ds in dsList:
	
	zipList = glob.glob(ds + "\\2040_2069\\ascii\\*.zip")
	
	for zip in zipList:
	
		print zip
		os.system("7z e " + zip + " -o" + ds + "\\2040_2069\\_asciis")
		# os.system("rmdir /s/q " + zip)
