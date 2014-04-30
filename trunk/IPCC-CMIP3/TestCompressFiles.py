# Fix corrupted downsacaled 30s files from ccafs-climate.org
# Carlos Navarro
# python TestCompressFiles.py S:\data\portals\ccafs_climate\download_data\files\data\ipcc_4ar_ciat_disaggregated sres_a1b

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")

dirbase = sys.argv[1]
# dirgrid = sys.argv[2]
sres = sys.argv[2]

# res = "30s"
reslist = "30s", "2_5min", "5min", "10min"
varDic = {"bio": 2000, "tmin": 1400, "tmax": 1400, "tmean": 1400, "prec": 800, "cons": 13}
periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
format = "asc"
extension = "zip"

for res in reslist:
	# periodlist = sorted(os.listdir(dirbase + "\\" + sres))
	for period in sorted(periodDc):
		
		modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\" + periodDc[period]))
		for model in modellist:		
			
			indir = dirbase + "\\" + sres + "\\" + periodDc[period] + "\\" + model + "\\" + res
			for var in sorted(varDic):
				
				file = indir + "\\" + model + "_" + sres + "_" + periodDc[period] + "_" + var + "_" + res + "_no_tile_" + format + "." + extension
				print "Testing.. ", os.path.basename(file)
				os.system("7z t -mmt " + file + " *.asc -r >> " + dirbase + "\\" + sres + "_" + format + "_test_compress_files.txt")
						
print "Test Compress Files done!"