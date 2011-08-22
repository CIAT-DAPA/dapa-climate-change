import os, sys, string, shutil
#python CompressOutputs.py N:\PrecisData\archive

dirbase = sys.argv[1]

folderlist = sorted(os.listdir(dirbase))
for folder in folderlist:
	if str(folder) == "ajeac" or str(folder) == "ajead": 
		# and not str(folder) == "eraab" and not str(folder) == "eraad" and not str(folder) == "eraae" and not str(folder) == "gisap" and not str(folder) == "ajdab" and not str(folder) == "ajdac" and not str(folder) == "ajdad" and not str(folder) == "hadab" and not str(folder) == "hadac" and not str(folder) == "hadad" and not str(folder) == "gisar" and not str(folder) == "gisav" and not str(folder) == "sreaf"  and not str(folder) == "sream":
		print dirbase + "\\" + str(folder) + "\n"

		runidlist = sorted(os.listdir(dirbase + "\\" + str(folder)))
		for runid in runidlist:
			if not str(runid)[-4:] == ".zip":
				print "  Compressing.. " +  str(folder) + " " + str(runid) + "\n"
				InZip = dirbase + "\\" + str(folder) + "\\" + str(runid) + "_pp.zip"
				InFolder = dirbase + "\\" + str(folder) + "\\" + str(runid)
				os.system('7za a ' + InZip + " " + InFolder)
				shutil.rmtree(dirbase + "\\" + str(folder) + "\\" + str(runid))