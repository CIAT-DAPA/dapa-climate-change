# -------------------------------------------------------------
# Author: Carlos Navarro
# Date: April 13th 2011
# Pourpose: Make a mxd with layers from GCM downscaled datasets
# -------------------------------------------------------------

import arcpy, os, glob, sys, string

if len(sys.argv) < 4:
	os.system('clear')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python GCMCreateMXDs.py D:\Workspace\AMKN_Maps\base-data\climate\current SRES_A1B Global_10min"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
scenario = sys.argv[2]
resolution = sys.argv[3]

#Clear screen
os.system('cls')

# print "     Make MXDs \n" 
# variableList = "bio", "prec", "tmax", "tmean", "tmin" #, "cons_mths",

# # Get a list of layers
# dirlayer = dirbase + "\\" + str(scenario) + "\\" + str(resolution)

# for variable in variableList:
	
	# if not os.path.exists(dirbase + "\\" + str(scenario) + "_" + str(resolution) + "_" + str(variable) + ".mxd"):
		
		# # Set map and data frame
		# mxd = arcpy.mapping.MapDocument(dirbase + "\\" + str(scenario) + ".mxd")
		# df = arcpy.mapping.ListDataFrames(mxd, str(scenario)[0]
		# layerList = sorted(glob.glob(dirlayer + "\\" + str(variable) + "*.lyr"))
		
		# for layer in layerList:
			# print str(variable) + "_" + os.path.basename(layer).split("-")[0].split("_")[1] + "-" + os.path.basename(layer).split("-")[1] + "-" + os.path.basename(layer).split("-")[2] + "-" + os.path.basename(layer).split("-")[3] + "-" + os.path.basename(layer).split("-")[4]+ " added"
			# addLayer = arcpy.mapping.Layer(str(dirlayer + "\\" + str(variable) + "_" + os.path.basename(layer).split("-")[0].split("_")[1] + "-" + os.path.basename(layer).split("-")[1] + "-" + os.path.basename(layer).split("-")[2] + "-" + os.path.basename(layer).split("-")[3] + "-" + os.path.basename(layer).split("-")[4]))
			# arcpy.mapping.AddLayer(df, addLayer, "BOTTOM")

		# mxd.saveACopy(dirbase + "\\" + str(scenario) + "_" + str(resolution) + "_" + str(variable) + ".mxd", "9.3")
		# # mxd.save()
		# del mxd
# print "Process done!!!"  





#From a workspace
variableList = "bio", "prec", "tmax", "tmean", "tmin" #, "cons_mths",

# Get a list of layers
dirlayer = dirbase + "\\WorldClim_data"

for variable in variableList:
	
	# if not os.path.exists(dirbase + "\\current.mxd"):
		
	# Set map and data frame
	mxd = arcpy.mapping.MapDocument(dirbase + "\\current.mxd")
	df = arcpy.mapping.ListDataFrames(mxd)[0]
	layerList = sorted(glob.glob(dirlayer + "\\" + variable + "*.lyr"))
	
	for layer in layerList:
		print layer
		addLayer = arcpy.mapping.Layer(str(dirlayer + "\\current_10min_" + str(variable) + "_" + os.path.basename(layer).split("-")[3]))
		arcpy.mapping.AddLayer(df, addLayer, "BOTTOM")

	mxd.saveACopy(dirbase + "\\current.mxd", "9.3")
	# mxd.save()
	del mxd
print "Process done!!!"  
