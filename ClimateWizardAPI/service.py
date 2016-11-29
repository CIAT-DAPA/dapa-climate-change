from osgeo import gdal,ogr
from osgeo.gdalconst import *
import struct
import sys,os, fnmatch
from bottle import route, request, response, template, run

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
	if not request.query.range:
		if request.query.scenario == "historical":
			wrange = ["1976","2005"]
		else:
			wrange = ["2040","2069"]
	elif request.query.range == "false":
		wrange = "false"
	else:
		wrange = request.query.range.split("-")

	if not request.query.gcm:
		wgcm = "ensemble_lowest"
	else:
		wgcm = request.query.gcm

	if not request.query.avg:
		wavg = True
	else:
		wavg = False

	fileName = request.query.index+"_BCSD_"+request.query.scenario+"_"+wgcm
	folderModels = "/media/camilo/Data/camilo/proyectos/WOCAT/"
	folder = folderModels+wgcm+"/"
	allfiles = find(fileName+"*", folder)

	if allfiles:
		name = allfiles.split(folder)
		acroIndex = name[1].split("_")
		period = acroIndex[4].split("-")
		startDate = int(acroIndex[4].split("-")[0])
		json_output = {'name' : index[request.query.index.lower()], 'acronym':request.query.index,'model':'ACCESS1-0','scenario':request.query.scenario ,'values':[]}
		lat = float(request.query.lat)
		lon = float(request.query.lon)

		ds = gdal.Open(folder+name[1], GA_ReadOnly)
		transf = ds.GetGeoTransform()

		px = (lon-transf[0])/transf[1]
		py = (lat-transf[3])/transf[5]

		if ds is None:
			print 'Failed open file'
			sys.exit(1)

		bands = ds.RasterCount
		if wrange and wrange != "false":
			avg = 0
			for band in range( bands ):
				band += 1
				if int(wrange[0]) <= int(band+startDate-1) <= int(wrange[1]):
					srcband = ds.GetRasterBand(band)
					structval = srcband.ReadRaster(int(px), int(py), 1, 1, buf_type=srcband.DataType )
					bandtype = gdal.GetDataTypeName(srcband.DataType)
					intval = struct.unpack(fmttypes[bandtype] , structval)
					if wavg:
						avg += float(intval[0])
					else:
						output_item = {'date' : int(band+startDate-1) , 'value' : float(intval[0])}
						json_output['values'].append(output_item)
			if avg != 0 and wavg:
				avg = avg / (int(wrange[1]) - int(wrange[0]) + 1)
				output_item = {'date' : 'avg_'+wrange[0]+"-"+wrange[1] , 'value' : str(avg)}
				json_output['values'].append(output_item)
			elif avg == 0 and wavg:
				output_item = {'date' : 'avg_'+wrange[0]+"-"+wrange[1] , 'value' : 'out of range'}
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
		return {"error":"Data not found"}

