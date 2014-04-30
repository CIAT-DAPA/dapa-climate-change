import os, sys, glob, string, shutil
# # # python CopyBatch.py S:\data\gcm\cmip3\disaggregated\sres_a1b\Global_30s S:\data\portals\ccafs_climate\download_data\files\data\ipcc_4ar_ciat_disaggregated\sres_a1b

# periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
# dirbase = sys.argv[1]
# dirout = sys.argv[2]
# modellist = sorted(os.listdir(dirbase))
# for model in modellist:
	# for period in periodDc:
		# indir = dirbase + "\\" + model + "\\" + period + "\\_asciis"
		# if os.path.exists(indir):
			# outdir = dirout + "\\" + periodDc[period] + "\\" + model + "\\30s" 
			# print indir, outdir
			# os.system("robocopy " + indir + " " + outdir + " /z /e")
			# shutil.rmtree(indir)
		
	
	
# Para renombrar los archivos
#python CopyBatch.py S:\data\portals\ccafs_climate\download_data\files\data\ipcc_4ar_ciat_disaggregated

dirbase = sys.argv[1]
sreslist = sorted(os.listdir(dirbase))
for sres in sreslist:
# sres = "sres_b1"
	periodlist = sorted(os.listdir(dirbase + "\\" + sres))
	for period in periodlist:
		modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\" + period))
		# modellist = "ipsl_cm4", "miroc3_2_hires", "miroc3_2_medres", "miub_echo_g", "mpi_echam5", "mri_cgcm2_3_2a", "ncar_ccsm3_0", "ukmo_hadcm3"
		for model in modellist:
			resolutions = sorted(os.listdir(dirbase + "\\" + sres + "\\" + period + "\\" + model))
			for res in resolutions:
			# res = "30s"
				indir = dirbase + "\\" + sres + "\\" + period + "\\" + model + "\\" + res
				filelist = sorted(glob.glob(indir + "\\*"))
				for file in filelist:
					var = os.path.basename(file).split("_")[0]
					format = os.path.basename(file).split("_")[1]
					filenamemod = model + "_" + sres + "_" + period + "_" + var + "_" + res + "_no_tile_" + format
					
					print "Renaming ", filenamemod
					os.rename(file, dirbase + "\\" + sres + "\\" + period + "\\" + model + "\\" + res + "\\" + filenamemod)
