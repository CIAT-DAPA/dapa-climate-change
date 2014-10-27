# python D:\_scripts\dapa-climate-change\data-processing-Python\get_size_directory.py D:\Documentos\MANAUALES\workshop_netcdf gb

import os,sys
from os import walk
dirbase = sys.argv[1]
type = sys.argv[2]

def get_size(type,start_path = '.'):
    total_size = 0
    for dirpath, dirnames, filenames in os.walk(start_path):
        for f in filenames:
			fp = os.path.join(dirpath, f)
			total_size += os.path.getsize(fp)
			# total_size += os.stat(fp).st_size
	if type=="gb":
		total_size = total_size/(500000000.0*2)
	if type=="mb":
		total_size = total_size/(1024*1024.0)
	else:
		total_size = total_size
		
    return total_size

# print get_size(type,dirbase)	
	
folderList = next(os.walk(dirbase))[1]
for folder in folderList:
	print folder+":\t",get_size(type,dirbase+"\\"+folder),'\t',type

