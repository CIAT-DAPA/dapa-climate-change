# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Copy rasters to another location
# ---------------------------------------------------------
# python 07-ZipGrids_process.py T:\gcm\cmip5\downscaled S:\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled rcp26 10min



import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python CopyRasters.py <dirbase> <dirout> <wildcard> <switch>"
	print "   - ex: python CopyRasters.py D:\Workspace D:\Workspace ALL NO"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
rcp = sys.argv[3]
res = sys.argv[4]

print "\n~~~~ COPY RASTERS ~~~~\n"

# Clear screen
os.system('cls')

# Set workspace
# gp.workspace = dirbase 

# Create output folder
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

# Get a list of grids in the workspace
# print "\t ..listing grids into " + dirbase
# if wildcard == "ALL":
	# rasters = sorted(gp.ListRasters("*", "GRID"))
# else:	
	# rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids

periodlist = ["2020_2049", "2040_2069", "2060_2089", "2070_2099"]
modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + res))
variablelist = ["bio","cons_mths","prec"] #["bio","cons_mths","prec","tmin","tmax","tmean" ]
# modellist = ["ipsl_cm5a_lr"]
# for rcp in rcpList:		
for period in periodlist:
	for model in modellist:
		
		if period == "2020_2049":
			tsmod= "2030s"
		elif period=="2040_2069":
			tsmod= "2050s"
		elif period== "2060_2089":
			tsmod= "2070s"
		else:
			tsmod= "2080s"
		
		if rcp== "rcp26":
			rcpmod ="rcp2_6"
		elif rcp== "rcp45":
			rcpmod ="rcp4_5"
		elif rcp== "rcp60":
			rcpmod ="rcp6_0"
		else:
			rcpmod= "rcp8_5"
	
		outfolder="S:\\portals\\ccafs_climate\\download_data\\files\\data\\ipcc_5ar_ciat_downscaled"+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +res
		descfile = outfolder+"_grd2zip_done2.txt"
		if not os.path.exists(descfile): 
			for var in variablelist:
				
				if var == "bio":
					num = 19
				elif var == "cons_mths":
					num = 1				
				else:
					num = 12
				# for month in [1,12]:
				
				
				# descfile = dirout+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +res+"_grd2zip_done2.txt"
				
				
				
				outmoddir = dirout+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +res
				if not os.path.exists(outmoddir+ "\\" +var):
					os.system("mkdir "+outmoddir+ "\\" +var)			
				print "......",rcp,period, model,"\n"
				for month in range(1,num+1):
					if var == "cons_mths":
						variable = var
					else:
						variable = var + "_" + str(month)
						
					workspace = dirbase + "\\" + rcp + "\\global_" + res + "\\" + model + "\\r1i1p1\\" + period	

					raster= workspace+"\\"+variable

					if not gp.Exists(outmoddir+ "\\" +var + "\\" + variable):
						gp.copy_management(raster, outmoddir+ "\\" +var+ "\\" +variable)
						print "\t", variable, "copied"
					else:
						print "\t", variable, "already exist!"
				
				if var == "cons_mths":
					vartype=var.split("_")[0]	
					
					if os.path.exists(outfolder+ "\\" +model+ "_" +rcpmod+ "_" +tsmod+ "_" +vartype+ "_" +res+ "_r1i1p1_no_tile_grd.zip"): 
						os.system("del "+outfolder+ "\\" +model+ "_" +rcpmod+ "_" +tsmod+ "_" +vartype+ "_" +res+ "_r1i1p1_no_tile_grd.zip")
					os.system(" 7za a -mmt=4 "+ outfolder+ "\\" +model+ "_" +rcpmod+ "_" +tsmod+ "_" +vartype+ "_" +res+ "_r1i1p1_no_tile_grd.zip "+ outmoddir+ "\\" +var)
					os.system("rmdir /s /q "+outmoddir+ "\\" +var)
				else:
					if os.path.exists(outfolder+ "\\" +model+ "_" +rcpmod+ "_" +tsmod+ "_" +var+ "_" +res+ "_r1i1p1_no_tile_grd.zip"): 
						os.system("del "+outfolder+ "\\" +model+ "_" +rcpmod+ "_" +tsmod+ "_" +var+ "_" +res+ "_r1i1p1_no_tile_grd.zip")
					os.system(" 7za a -mmt=4 "+ outfolder+ "\\" +model+ "_" +rcpmod+ "_" +tsmod+ "_" +var+ "_" +res+ "_r1i1p1_no_tile_grd.zip "+ outmoddir+ "\\" +var)
					os.system("rmdir /s /q "+outmoddir+ "\\" +var)				
			outFile = open(descfile, "w")

				
		
# for raster in rasters:

	# if not gp.Exists(dirout + "\\" + raster):
	
		# Copy function
		# gp.copy_management(raster, dirout + "\\" + raster)
		# print "\t", raster, "copied"
	
		# Remove asciis
		# if switch == "YES":
			# gp.delete_management(raster)
		
print "\n \t Process done!!"