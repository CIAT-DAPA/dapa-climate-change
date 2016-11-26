from osgeo import gdal,ogr
from osgeo.gdalconst import *
import struct
import sys,os, fnmatch
from bottle import route, request, response, template, run
# lon = 12.502742
# lat = 42.243713

def pt2fmt(pt):
	fmttypes = {
		GDT_Byte: 'B',
		GDT_Int16: 'h',
		GDT_UInt16: 'H',
		GDT_Int32: 'i',
		GDT_UInt32: 'I',
		GDT_Float32: 'f',
		GDT_Float64: 'f'
		}
	return fmttypes.get(pt, 'x')

def find(pattern, path):
    result = ""
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                result = os.path.join(root, name)
    return result

index = {'txavg':'Monthly mean maximum temperatures historical', 'tnavg':'Monthly mean minimum temperatures', 'txx':'Monthly maximum temperatures', 'tnn':'Monthly minimum temperatures', 'gd10':'growing degree days', 'hd18':'heating degree days', 'cd18':'cooling degree days', 'tx90':'90th percentile Tmax - one value per year', 'tx90p':'Generalized version: Pct of time T doesnt exceeds ref pd Nth percentile', 'tx10p':'Generalized version: Pct of time T doesnt exceeds ref pd Nth percentile', 'tn90p':'Generalized version: Pct of time T doesnt exceeds ref pd Nth percentile', 'tn10p':'Generalized version: Pct of time T doesnt exceeds ref pd Nth percentile', 'fd':'Frost days', 'gsl':'Thermal growing season length', 'hwdi':'Heat wave duration index wrt mean of reference_period', 'ptot':'Monthly total precip', 'cdd':'Consecutive dry days', 'r02':'Number of wet days > 0.2 mm/d', 'r5d':'Max consec 5 day precip', 'sdii':'simple daily precip intensity index', 'r90p':'calculate of precip due to this too'}
fmttypes = {'Byte':'B', 'UInt16':'H', 'Int16':'h', 'UInt32':'I', 
            'Int32':'i', 'Float32':'f', 'Float64':'d'}	
	

@route('/service')
def service():
	#acroIndex = sys.argv[1].split("_")
	wavg = ""
	if request.query.avg:
		wavg = request.query.avg.split("-")

	fileName = request.query.index+"_BCSD_"+request.query.scenario+"_"+request.query.gcm
	folderModels = "/media/camilo/Data/camilo/proyectos/WOCAT/"
	folder = folderModels+request.query.gcm+"/"
	allfiles = find(fileName+"*", folder)
	# return allfiles
	if allfiles:
		name = allfiles.split(folder)
		acroIndex = name[1].split("_")
		period = acroIndex[4].split("-")
		startDate = int(acroIndex[4].split("-")[0])
		json_output = {'name' : index[request.query.index.lower()], 'acronym':request.query.index,'model':'ACCESS1-0','scenario':request.query.scenario ,'values':[]}
		lat = float(request.query.lat)
		lon = float(request.query.lon)
		# px, py = lon, lat
		ds = gdal.Open(name[1], GA_ReadOnly)
		transf = ds.GetGeoTransform()
		px = (lon-transf[0])/transf[1]
		py = (lat-transf[3])/transf[5]

		if ds is None:
			print 'Failed open file'
			sys.exit(1)

		bands = ds.RasterCount
		if wavg:
			avg = 0
			for band in range( bands ):
				band += 1
				if int(wavg[0]) <= int(band+startDate-1) <= int(wavg[1]):
					srcband = ds.GetRasterBand(band)
					structval = srcband.ReadRaster(int(px), int(py), 1, 1, buf_type=srcband.DataType )
					bandtype = gdal.GetDataTypeName(srcband.DataType)
					intval = struct.unpack(fmttypes[bandtype] , structval)
					avg += float(intval[0])
			if avg != 0:
				avg = avg / (int(wavg[1]) - int(wavg[0]) + 1)
				output_item = {'date' : 'avg_'+request.query.avg , 'value' : str(avg)}
				json_output['values'].append(output_item)
			else :
				output_item = {'date' : 'avg_'+request.query.avg , 'value' : 'out of range'}
				json_output['values'].append(output_item)
			return json_output

		else:
			for band in range( bands ):
				band += 1
				srcband = ds.GetRasterBand(band)
				structval = srcband.ReadRaster(int(px), int(py), 1, 1, buf_type=srcband.DataType )
				bandtype = gdal.GetDataTypeName(srcband.DataType)
				intval = struct.unpack(fmttypes[bandtype] , structval)
				output_item = {'date' : int(band+startDate-1) , 'value' : float(intval[0])}
				json_output['values'].append(output_item)
			return json_output
	else :
		return "Data not found"

run(host='0.0.0.0', port=8080, debug=True)
#application = bottle.default_app()