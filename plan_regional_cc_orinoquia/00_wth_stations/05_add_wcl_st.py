# ------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 2016/02
# Pourpose: Add WCL stations in zones where there are not enough stations based on a buffer of intial stations
# ------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "    - ie: python add_wcl_st.py D:\\CIAT\\Projects\\col-cormacarena\\01-datos-clima\\stations-averages D:\\CIAT\\Projects\\col-cormacarena\\01-datos-clima\\merge_all 0_5 ame D:\\CIAT\\climate_change\\station_data\\wcl_station_data D:\\CIAT\\_tools\\AdminBoundaries\\Continents\\5_continents\\sa.shp"
	sys.exit(1)

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print " 	   ADD WCL         "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

gp.toolbox = "analysis"

dirbase =sys.argv[1]
dirout =sys.argv[2]
buffdist =sys.argv[3]
suffix = sys.argv[4]
wcldir = sys.argv[5]
regionshp = sys.argv[6]

gp.workspace = dirbase
distname = buffdist.replace("_", ".")

# Buffer roads.shp based on each road feature's value in the Distance field,
# and dissolve buffers into groups according to values from Road_Type field.
varlist = "rain", "tmin", "tmax" 

for var in varlist:
	
	stations = gp.workspace + "\\" + var + "_" + suffix + ".shp"
	outbuff = dirout + "\\" + var + "_" + suffix + "_buff" + buffdist + ".shp"
	outdiss = dirout + "\\" + var + "_" + suffix + "_buff" + buffdist + "_diss.shp"
	wclstations = wcldir + "\\wc_" + var + "_stations_export.shp"
	outclipwcl = wcldir + "\\wc_" + var + "_stations_export_sa.shp"
	outerase = dirout + "\\" + var + "_" + suffix + "_buff" + buffdist + "_wcl_remain.shp"
	outmerge = dirout + "\\" + var + "_" + suffix + "_wcl_added.shp"
	
	print var, suffix, " buffering"
	### Buffer
	if not gp.Exists(outbuff):
		gp.buffer(stations, outbuff, distname, "FULL", "ROUND", "NONE")

	print var, suffix, " dissolving"
	### Dissolve
	if not gp.Exists(outdiss):
		gp.Dissolve_management(outbuff, outdiss, "BUFF_DIST")
	
	print var, suffix, " clipping"
	### Clip WCL Station reg
	if not gp.Exists(outclipwcl):
		gp.Clip_analysis(wclstations, regionshp, outclipwcl)
	
	print var, suffix, " erasing extra WCL"
	### Erase extra WCL stations
	if not gp.Exists(outerase):
		gp.erase(outclipwcl, outdiss, outerase)
		# gp.deletefeatures(outdiss)
		# gp.deletefeatures(outbuff)
	
	# print var, suffix, " merging WCL stations"
	# ### Join remain stations
	# if not gp.Exists(outmerge):
		# gp.Merge(stations + "\"" + outerase, outmerge)
		# gp.deletefeatures(outerase)
	
print "done!!!"    
