# python ZipGrids_process_disaggregation.py S:\data\gcm\cmip3 disaggregated a2 ALL ALL ALL

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

dirbase = sys.argv[1]
dataset = sys.argv[2]
sres = sys.argv[3]
periods = sys.argv[4]
models = sys.argv[5]
wildcard = sys.argv[6]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of periods
if periods == "ALL":
	periodlist = '2010_2039', '2020_2049', '2030_2059', '2040_2069', '2050_2079', '2060_2089', '2070_2099'
else:
	periodlist = periods.split(",") # ej: "2010_2039", "2050_2079", "2060_2089", "2030_2059"
#Get lists of models
if models == "ALL":
	modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\SRES_" + sres+ "\\Global_30s"))
else: 
	modellist = models.split(",")

#Get lists of data
# if variable == "ALL":
	# variablelist = ["prec","tmin","tmax","cons_mths","bio","tmean"]
# else:
	# variablelist = variable.split(",")
	
gp.AddMessage("Models: " + str(modellist))
gp.AddMessage( "Periods: " + str(periodlist) )	
# gp.AddMessage( "Variables: " + str(variablelist))			
	
# for model in modellist:
for model in modellist:
	for period in periodlist:
		print "\tprocessing", model, period,"\n"
		gp.workspace = dirbase + "\\" + dataset + "\\SRES_" + sres  + "\\Global_30s\\" + model + "\\" + period

		if wildcard == "ALL":
			variablelist = sorted(gp.ListRasters("*", "GRID"))
		else:	
			variablelist = sorted(gp.ListRasters(wildcard + "*", "GRID"))	
		
		for variable in variablelist:
			if not os.path.exists(gp.workspace + "\\" + variable.split("_")[0] + "_grd.zip") or os.path.getsize(gp.workspace+ "\\" +variable.split("_")[0]+"_grd.zip")/float(1000000) < 50:
				print "\tconping",variable
				if not os.path.exists(gp.workspace + "\\" + variable.split("_")[0]):
					os.system('mkdir ' + gp.workspace + "\\" + variable.split("_")[0])
				if not gp.Exists(gp.workspace + "\\" + variable.split("_")[0]+ "\\" + variable):	
					gp.CopyRaster_management(gp.workspace + "\\" +variable, gp.workspace + "\\" + variable.split("_")[0]+ "\\" + variable)
				# gp.Delete_management(gp.workspace + "\\" +variable)
					os.system("rmdir /s /q "+gp.workspace + "\\" +variable)
					os.system("del "+gp.workspace + "\\" +variable+'.aux')
			elif os.path.exists(gp.workspace + "\\" + variable.split("_")[0] + "_grd.zip") and os.path.getsize(gp.workspace+ "\\" +variable.split("_")[0]+"_grd.zip")/float(1000000) > 500:
				print "\tDelete",variable
				# gp.Delete_management(gp.workspace + "\\" +variable)
				os.system("rmdir /s /q "+gp.workspace + "\\" +variable)
				os.system("del "+gp.workspace + "\\" +variable+'.aux')
			elif variable=="cons_mths" and os.path.getsize(gp.workspace+ "\\" +variable.split("_")[0]+"_grd.zip")/float(1000000) > 5:
				print "\tDelete",variable
				# gp.Delete_management(gp.workspace + "\\" +variable)
				os.system("rmdir /s /q "+gp.workspace + "\\" +variable)
				os.system("del "+gp.workspace + "\\" +variable+'.aux')
			
			
		list = ['bio','cons','prec','tmax','tmean','tmin']	
		for var in list:
			if not os.path.exists(gp.workspace + "\\" + var + "_grd.zip") or os.path.getsize(gp.workspace+ "\\" +var.split("_")[0]+"_grd.zip")/float(1000000) < 50:
				if os.path.exists(gp.workspace + "\\" + var):
					print "\ncompressing...",var
					inZip = gp.workspace + "\\" + var + "_grd.zip"
					os.system('7za a -mx=1 -mmt=on -mtc=on ' + inZip + " " + gp.workspace  + "\\" + var)
				if os.path.exists(gp.workspace + "\\" + var) and os.path.exists(inZip):
					os.system('rmdir /s /q ' + gp.workspace + "\\" + var)
					#os.system("del "+gp.workspace + "\\" +var+'.aux')
					
		# if not os.path.exists(gp.workspace + "\\info"):	
			# os.system('rmdir /s /q ' + gp.workspace + "\\info")	
		# if not os.path.exists(gp.workspace + "\\listgrids.list"):
			# os.system('rmdir /s /q ' + gp.workspace + "\\listgrids.list")	
		# if not os.path.exists(gp.workspace + "\\log"):
			# os.system('rmdir /s /q ' + gp.workspace + "\\log")	
			
			
		# if variable == "ALL":
			# var = "*"
		# else:	
			# var = variable + "*"
			
		# for variable in variablelist:
			# if gp.Exists(gp.workspace+ "\\" +variable+"_grd.zip") and os.path.getsize(gp.workspace+ "\\" +variable+"_grd.zip")/float(1000000) > 500:
				# print "\tprocessing", model, period


				# if variable == "bio":
					# num = 19
				# else:
					# num = 12
				# for month in range (1, num + 1, 1):
					# if variable == "cons_mths":
						# raster = gp.workspace + "\\" + variable
					# else:
						# raster = gp.workspace + "\\" + variable + "_" + str(month)
				
					# if gp.Exists(raster):
						# gp.delete_management(raster)
						# print "Deleted", model, period, os.path.basename(raster)
			# elif variable == "cons_mths" and gp.Exists(gp.workspace+ "\\cons_grd.zip"):
				# raster = gp.workspace + "\\" + variable
				# if gp.Exists(raster):
					# gp.delete_management(raster)
					# print "Deleted", model, period, os.path.basename(raster)			 

					
		
		