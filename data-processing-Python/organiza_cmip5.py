### Autor: Jaime Tarapues
##### python organiza_cmip5.py G:\jetarapues\FirefoxPortable\CMIP5 T:\gcm\cmip5\raw\daily
import os, sys, glob, string, shutil, errno
import os.path
import string
import os
import glob
import re
import itertools
import operator
import urllib2

dirbase = sys.argv[1]
outdir = sys.argv[2]
# outdir = 'X:\rcm\cordex'


### dirbase = 'F:\jtarapues\CORDEX\\'
os.chdir(dirbase)
	
os.chdir(dirbase)
lista = glob.glob("*.nc")

for w in lista:

	var=w.split('_')[0]
	model=w.split('_')[2].replace("-", "_")
	rcp=w.split('_')[3]
	ens=w.split('_')[4]
	outFile=outdir + '\\' + rcp + '\\' + model + '\\' + ens 

	if not os.path.exists(outFile+'\\'+w): ### si no existe el file destino
		print "....coping",w
	
		if not os.path.isdir(outFile):
			print "...Create Folder " + rcp + '\\' + model + '\\' + ens
			os.makedirs(outFile)

		# print '\n----copy file:', w
		os.system("robocopy "+ dirbase + " " + outFile +' ' +w + " /Z /S /MOV")
		# print '--move file done!'
	# else:
		# input = dirbase+ w
		# out = outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4] +'\\' +w
		# if os.path.getsize(out)==os.path.getsize(input):
			# os.remove(input)
			# print "\n-- removed",w


	
print "DONE !!!!"
		