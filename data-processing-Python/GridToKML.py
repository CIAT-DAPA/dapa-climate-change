# Import system modules
import sys, string, os, arcgisscripting

os.system('cls')

# Create the Geoprocessor object
gp = arcgisscripting.create()  

# Check out any necessary licenses
gp.CheckOutExtension("spatial")

# Check out any necessary licenses
gp.CheckOutExtension("3D")

# allow output to overwrite
gp.OverwriteOutput = 1

basedir = "D:\\FAO_50_crops_cc_impacts\\results"

path_ascii = basedir + "\\FAO_50_crops_current_predictions" # el path de los ascii o en su defecto grids
out_kmlpath = basedir + "\\website_kmls" #donde vas a poner los kml
out_gridpath = basedir + "\\website_grids"

if not os.path.exists(out_kmlpath):
	os.system('mkdir ' + out_kmlpath)

if not os.path.exists(out_gridpath):
	os.system('mkdir ' + out_gridpath)

# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Convert GRIDs to KML 
# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++

croplist = os.listdir(path_ascii)

for cropname in croplist:
	print 'Processing ' + cropname
	
	dataset = 'suitability'
	workdir = path_ascii + "\\" + cropname + "\current"
	
	gp.Workspace = workdir
	
	outdataset = out_gridpath + "\\" + cropname
	outlayer = out_gridpath + "\\lyr_" + cropname
	
	if gp.exists(outdataset):
		print '  .Caution: Raster ' + cropname + ' will be overwritten'
	
	print '  .Conditional'
	gp.Con_sa (dataset, dataset, outdataset, '',"VALUE > 0")
	
	if os.path.exists(out_kmlpath + cropname + '.kmz'):
		os.system('del /q/f ' + out_kmlpath + "\\" + cropname + '.kmz')
	
	print '  .Layer'
	# Process: Make Raster Layer...
	gp.MakeRasterLayer_management(outdataset, outlayer, '', '', "1") #Create the .lyr for the Egrid
	
	print '  .Symbology'
	# Process: Apply Symbology From Layer...
	gp.ApplySymbologyFromLayer_management(outlayer, basedir + "\\suitability.lyr") #Symbology.lyr contains the colormap previously prepared with ArcMap
	
	print '  .KMLing'
	# Process: Layer To KML...
	gp.LayerToKML_3d(outlayer, out_kmlpath + "\\" + cropname + '.kmz', "1", "false", "DEFAULT", "12000", "300")
	print ''
