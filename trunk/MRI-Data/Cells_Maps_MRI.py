#------------------------------------------------------------------------------------------------
# Description:  Make cells maps from XY Feature Layer of MRI points(dbf), since 0.5 to 2.5 degrees
# Author: Carlos Navarro
# Date: 02/12/10
#------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Cells_Maps_MRI.py E:\MRI_Analysis\Maps 1989 1990 tmean world"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]
region = sys.argv[5]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

gp.workspace = dirbase

print "\n"
print "~~~~~~~~~~~~~~"
print "  CELLS MAPS  "
print "~~~~~~~~~~~~~~"
print "\n"

dirtable = dirbase + "\\dbf"

print "     ---> Creating Layer " + variable + " " + region

if not gp.Exists(dirtable + "\\" + variable + "_annual_" + region + ".lyr"):
  
    # Prepare the variables
    in_Table = dirtable + "\\" + variable + "_annual_" + region + ".dbf"
    in_x = "LON"
    in_y = "LAT"
    out_Layer = variable + "_annual_" + region

    # Make the XY event...
    gp.MakeXYEventLayer(in_Table, in_x, in_y, out_Layer, "")

    # Save to a layer file
    gp.SaveToLayerFile(out_Layer, dirtable + "\\" + out_Layer + ".lyr")
    print "     ---> Layer Created"
else:
	print "     ---> Layer Exists"

metriclist = "RSQ", "SLOPE", "RMSE", "MAE"
degree =  "0.5"

for year in range(inityear, finalyear + 1, 1):

	print "     ---> Calculating to 0.5 degrees \n"

	for metric in metriclist:
		
		inputFeatureDataset = dirtable + "\\" + variable + "_annual_" + region + ".lyr"
		diroutputRaster = dirbase + "\\CellsMaps_" + str(degree) + "\\" + variable + "_" + region
		
		if not os.path.exists(diroutputRaster):
			os.system('mkdir ' + diroutputRaster)    
		
		outputRaster = diroutputRaster + "\\" + metric + "_" + str(year)

		if not gp.Exists(outputRaster):
		
			print "     ---> Processing " + metric + " " + variable + " " + str(degree) + " " + str(year)
			# Set some of the IDW parameters
			cell_size = str(degree)
			IDW_power = 2
			IDW_neighborhood = "VARIABLE 20"
			Input_barrier_polyline_features = dirtable + "\\" + region + "_boundary.shp"

			# Set the attribute field
			attributeName = metric + "_" + str(year)

			# Check out Spatial Analyst extension license
			gp.CheckOutExtension("Spatial")

			# Process: IDW
			gp.Idw_sa(inputFeatureDataset, attributeName, outputRaster, cell_size, IDW_power, IDW_neighborhood, Input_barrier_polyline_features)

			print "     ---> Interpolated " + metric + " " + variable + " " + str(degree) + " " + str(year)

		else:
		
			print "     ---> Raster " + outputRaster + " exists"
	
print "Done!!!!"