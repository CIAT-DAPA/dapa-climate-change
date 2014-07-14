### Autor: Jaime Tarapues
##### python F:\jetarapues\_scripts\organiza_cordex.py F:\jetarapues\CORDEX\\ U:\rcm\cordex\\
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
	if os.path.exists(dirbase+ w) and not os.path.exists(outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4]+'\\'+w): ### si no existe el file destino

		if not os.path.isdir(outdir+'\\'+ w.split("_")[1] ):
			print "...Create Folder " + w.split("_")[1]
			os.makedirs(outdir+'\\'+ w.split("_")[1] )
		if not os.path.isdir(outdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] ):
			print "...Create Folder " + w.split("_")[7]
			os.makedirs(outdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7])
		if not os.path.isdir(outdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] ):
			print "...Create Folder " + w.split("_")[3]
			os.makedirs(outdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3])
		if not os.path.isdir(outdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7]+'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5]  ):
			print "...Create Folder " + w.split("_")[5]
			os.makedirs(outdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] )
		if not os.path.isdir(outdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4]  ):
			print "...Create Folder " + w.split("_")[4]
			os.makedirs(outdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4] )	

		###COPY FILE AND DELETE FILE
		print '\n----copy file:', w
		os.system("robocopy "+ dirbase + " " + outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4] +' ' +w + " /Z /s /MOV")
		print '--move file done!'
	else:
		input = dirbase+ w
		out = outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4] +'\\' +w
		if os.path.getsize(out)==os.path.getsize(input):
			# os.remove(input)
			print "\n-- removed",w
# if os.path.exists(dirbase+ w) and os.path.exists(outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4]+'\\'+file_name) and file_size == os.path.getsize(outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4]+'\\'+file_name):
		# print '\n----File exist!, delete file:', dirbase+w, os.path.getsize(dirbase+w), ' = size exits file: ', os.path.getsize(outdir+ w.split('_')[1] + '\\' + w.split('_')[7].split('\n')[0]+ '\\' + w.split('_')[3] + '\\' + w.split('_')[5] + '\\' + w.split('_')[4]+'\\'+file_name)/float(1000000) 
		# os.remove(dirbase+ file_name)

	
print "DONE !!!!"
		