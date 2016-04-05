import os, sys, glob, string, shutil
#python 06-MoveOutputsMS.py F:\cenavarro\ms\tmp X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\datos_diarios rcp85

dirbase = sys.argv[1]
dirout = sys.argv[2]
scenario = sys.argv[3]

for fold in range(1, 23+1, 1):

	print " .> Moving " + scenario + " fold-" + str(fold)
	
	outdir = dirout + "\\" + scenario + "\\fold-" + str(fold).zfill(2)
	if not os.path.exists(outdir):
		os.system('mkdir ' + outdir)
	
	for run in range(0, 8+1, 1):
	
		print "		Part " + str(run)
		
		if os.path.exists(dirbase + "\\" + scenario + "_fold-" + str(fold).zfill(2) + "\\data\\10" + str(run)):
		
			# dsList = glob.glob(dirbase + "\\" + scenario + "_fold-" + str(fold).zfill(2) + "\\data\\10" + str(run) + "\\*")
			# for ds in dsList:
			
			os.system('robocopy ' + dirbase + "\\" + scenario + "_fold-" + str(fold).zfill(2) + "\\data\\10" + str(run) + " " + outdir +' /MT:28 /NFL /NDL /E /MOVE')
			# try:
				# # shutil.copy(ds, outdir)
				
			# except WindowsError:
				# # can't copy file access times on Windows
				# pass
				
			
