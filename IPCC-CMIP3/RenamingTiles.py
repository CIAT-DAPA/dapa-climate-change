import os, sys, glob, string

# python RenamingTiles.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled_tiled \\dapadfs\data_cluster_4\portals\ccafs_climate\download_data\files\data\ipcc_4ar_ciat_tiled
# Examples
# bccr_bcm2_0_A1B_2020s_bio_ZoneA1_asc.zip
# bccr_bcm2_0_sres_a1b_2020s_bio_30s_no_tile_asc.zip

dirbase = sys.argv[1]
dirout = sys.argv[2]

periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
varlist = "bio", "cons", "prec", "tmax", "tmean", "tmin"
zonelist = "A1", "A2", "A3", "A4", "A5", "A6", "B1", "B2", "B3", "B4", "B5", "B6", "C1", "C2", "C3", "C4", "C5", "C6",

sreslist = sorted(os.listdir(dirbase))
for sres in sreslist:
	modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\global_30s"))
	for model in modellist:
		periodlist = sorted(os.listdir(dirbase + "\\" + sres + "\\global_30s\\" + model))
		for period in periodlist:
			indir = dirbase + "\\" + sres + "\\global_30s\\" + model + "\\" + period + "\\_tiles"
			outdir = dirout + "\\" + sres + "\\" + periodDc[period] + "\\" + model + "\\30s"	
			
			if not os.path.exists(outdir):
				os.system('mkdir ' + outdir)
				
			for var in varlist:
				# var = os.path.basename(file).split("_")[0]
				
				for zone in zonelist:
					infilename = model + "_" + sres.split("_")[1] + "_" + periodDc[period] + "_" + var + "_Zone" + zone + "_asc.zip"
					outfilename = model + "_" + sres + "_" + periodDc[period] + "_" + var + "_30s_tile_" + zone.lower() + "_asc.zip"
					
					if os.path.exists(indir + "\\" + infilename):
						print "Renaming ", indir + "\\" + infilename, outdir + "\\" + outfilename
						os.rename(indir + "\\" + infilename, outdir + "\\" + outfilename)
					
print "Moving process done!"