# Jaime Tarapues
# Calcula el peso total de todos los subdirectorios 
# python get_size_directory.py G:\ gb

import os,sys
from os import walk
dirbase = sys.argv[1]

def get_size(start_path = '.'):
    total_size = 0
    for dirpath, dirnames, filenames in os.walk(start_path):
        for f in filenames:
			fp = os.path.join(dirpath, f)
			total_size += os.path.getsize(fp)
			# total_size += os.stat(fp).st_size
	if total_size>(536870912.0*2):
		total_size = (total_size/(536870912.0*2)),"gb"
	elif total_size>(1024*1024.0) and total_size<(536870912.0*2):
		total_size = (total_size/(1024*1024.0)),"mb"
	else:
		total_size = total_size,"bytes"
		
    return total_size

# print get_size(dirbase)	
	
folderList = next(os.walk(dirbase))[1]
for folder in folderList:
	print folder+":\t",get_size(dirbase+"\\"+folder),'\t'

