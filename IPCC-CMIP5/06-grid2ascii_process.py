# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Copy rasters to another location
# ---------------------------------------------------------
# python D:\jetarapues\_scripts\06-grid2ascii_process.py S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled rcp8_5


import arcgisscripting, os, sys, string, glob, subprocess
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")

dirbase = sys.argv[1]
rcp = sys.argv[2]

dirGCM='T:\\gcm\\cmip5\\downscaled'

# res = "30s"
reslist = ["30s"] #, "2_5min", "5min", "10min" #["10min"]#
varlist = ["bio","tmin","tmax","tmean","prec","cons"]
periodDc = {"2020_2049": "2030s", "2040_2069": "2050s", "2060_2089": "2070s", "2070_2099": "2080s"}
format = 'asc'#"grd"
extension = "zip"

if rcp== "rcp2_6":
	rcpmod ="rcp26"
elif rcp== "rcp4_5":
	rcpmod ="rcp45"
elif rcp== "rcp6_0":
	rcpmod ="rcp60"
else:
	rcpmod= "rcp85"

for res in reslist:
	# periodlist = sorted(os.listdir(dirbase + "\\" + rcp))

	for period in sorted(periodDc):
	
		modellist = sorted(os.listdir(dirGCM + "\\" + rcpmod + "\\global_" + res))
		for model in modellist:		
			
			indir = dirbase + "\\" + rcp + "\\" + periodDc[period] + "\\" + model + "\\" + res
			inmoddir=dirGCM + "\\" + rcpmod + "\\global_" + res+ "\\" +model+ "\\r1i1p1\\"+period 
			outmoddir=dirbase+ "\\" +rcp+ "\\" +periodDc[period]+ "\\" +model+ "\\" +res
			for var in varlist:
				cmd =''
				file = indir + "\\" + model + "_" + rcp + "_" + periodDc[period] + "_" + var + "_" + res + "_r1i1p1_no_tile_" + format + "." + extension
				# print "Testing.. ", os.path.basename(file)
				if var == "bio" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 10:#35:#125:#2000:
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				if var == "tmin" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 7:#20:#80:#1400:
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				if var == "tmax" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 7:#20:#80:#1400:
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				if var == "tmean" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 7:#20:#80:#1400:
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				if var == "cons" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 0.1:#0.2:#0.5:#13:
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				if var == "prec" and os.path.exists(file) and os.path.getsize(file)/float(1000000) < 5:#15:#60:#800:
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				if not os.path.exists(file):
					cmd = 'arc "'+'&run 06-grid2ascii_GCM.aml '+ inmoddir+' '+ outmoddir+' '+model+' '+rcp+' '+period+' '+res+' '+var+' YES"'
				
				# print("Loading ARC...")
				if cmd!='': 
					print '... processing: '+os.path.basename(file)
					proc = subprocess.call(cmd,shell=True)
					# env = os.environ.copy()
					# env['PATH'] = '%s;%s' % (env['WINDIR'], env['WINDIR'] + '\\system32')					
					# proc = subprocess.Popen(cmd,stdout=subprocess.PIPE, env=env, shell=True)
					# output = proc.communicate()
					# print "output -> " + str(output)
					# exit_code = proc.wait()

			

print "\n \t Process done!!"