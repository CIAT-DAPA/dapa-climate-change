# ---------------------------------------------------------
# Autor: Carlos Navarro
# ---------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Merge_txt_files.py D:\Worldclim_interpolations"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

print "\n~~~~ MERGE TXT FILES ~~~~\n"
periodlist = "outputs_1951_2010", "outputs_1971_2000", "outputs_1981_2010"
varlist = "rain", "tmin", "tmax"

for period in periodlist:
	for var in varlist:
		foldlist = sorted(os.listdir(dirbase + "\\" + period + "\\" + var))
		print "Merging " + period + "_" + var
		outFile = open(dirbase + "\\" + period + "_" + var + "_" + "metrics.csv", "a")
		outFile.write("MONTH,R2.FIT,P.VAL.FIT,RMSE.FIT,R2.TEST,P.TEST.FIT,RMSE.TEST\n")	
		for fold in foldlist:
			txt =  open(dirbase + "\\" + period + "\\" + var + "\\" + fold + "\\tile-1\\" + var + "_metrics.csv")
			txt.next()
			
			for line in txt:
				outFile.write(line)
			txt.close()
		outFile.close()

