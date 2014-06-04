# Fix corrupted downsacaled 30s files from ccafs-climate.org
# Carlos Navarro
# python D:\_scripts\dapa-climate-change\IPCC-CMIP5\TestCompressFiles_cmip5.py S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled D:\Temp_br rcp2_6

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")

dirbase = sys.argv[1]
txt = sys.argv[2]
sres = sys.argv[3]

# res = "30s"
reslist = ["10min"]#"30s", "2_5min", "5min", "10min"
varlist = ["bio","tmin","tmax","tmean","prec","cons"]
periodDc = {"2020_2049": "2030s", "2040_2069": "2050s", "2060_2089": "2070s", "2070_2099": "2080s"}
format = "grd"
extension = "zip"


descfile = txt+"\\"+str(reslist[0])+"_"+"testCompressFile_size_"+ sres +".txt"
# if not os.path.isfile(descfile):
outFile = open(descfile, "w")
outFile.write("EXIST" + "\t"+ "MODEL" + "\t" + "PERIOD" + "\t" + "VAR" + "\t" + "RES"+ "\t" + "SIZE" +"\t" + "PATH" + "\n")
outFile.close()	
		
for res in reslist:
	# periodlist = sorted(os.listdir(dirbase + "\\" + sres))

	for period in sorted(periodDc):
	
		modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\" + periodDc[period]))
		for model in modellist:		
			
			indir = dirbase + "\\" + sres + "\\" + periodDc[period] + "\\" + model + "\\" + res
			for var in varlist:
				
				file = indir + "\\" + model + "_" + sres + "_" + periodDc[period] + "_" + var + "_" + res + "_r1i1p1_no_tile_" + format + "." + extension
				print "Testing.. ", os.path.basename(file)
				if var == "bio" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 10:#35:#125:#2000:
					outFile = open(descfile, "a")
					outFile.write("YES\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\t" + str(os.path.getsize(file)/float(1000000))+ "\t" + file+"\n")
				if var == "tmin" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 7:#20:#80:#1400:
					outFile = open(descfile, "a")
					outFile.write("YES\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\t" + str(os.path.getsize(file)/float(1000000))+ "\t" + file+"\n")
				if var == "tmax" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 7:#20:#80:#1400:
					outFile = open(descfile, "a")
					outFile.write("YES\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\t" + str(os.path.getsize(file)/float(1000000))+ "\t" + file+"\n")
				if var == "tmean" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 7:#20:#80:#1400:
					outFile = open(descfile, "a")
					outFile.write("YES\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\t" + str(os.path.getsize(file)/float(1000000))+ "\t" + file+"\n")
				if var == "cons" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 0.1:#0.2:#0.5:#13:
					outFile = open(descfile, "a")
					outFile.write("YES\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\t" + str(os.path.getsize(file)/float(1000000))+ "\t" + file+"\n")
				if var == "prec" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 5:#15:#60:#800:
					outFile = open(descfile, "a")
					outFile.write("YES\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\t" + str(os.path.getsize(file)/float(1000000))+ "\t" + file+"\n")

				elif not os.path.exists(file):
					outFile = open(descfile, "a")
					outFile.write("NO\t" + model + "\t" + periodDc[period] + "\t" + var + "\t" + res + "\tNA\t" + file+"\n")
				
				
				# print "Testing.. ", os.path.basename(file)
				# os.system("7z t -mmt " + file + " *.asc -r >> " + dirbase + "\\" + sres + "_" + format + "_test_compress_files.txt")
				outFile.close()		
print "\n\tTest Compress Files done!"