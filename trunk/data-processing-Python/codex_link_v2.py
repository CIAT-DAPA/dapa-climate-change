### Autor: Jaime Tarapues
##### python F:\jtarapues\_scripts\codex_link_v2.py F:\jtarapues\CORDEX\\ all_links.txt
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
ourdir = 'Z:\\rcm\\cordex\\'
file = sys.argv[2]

### dirbase = 'F:\jtarapues\CORDEX\\'
os.chdir(dirbase)
files = glob.glob(file)
lista_nueva = []
for file_input in files:
	l=open(dirbase+ file_input)
	lines = [i for i in l.readlines()]
	for l in lines:
		txtfile = open(dirbase+'cordex.txt')
		for period in txtfile:
			while True:
				try:
					url = l.split('\n')[0] + period.split('\n')[0]
					
					# outFile = open(dirbase+'\\'+'links_cordex_proc.txt', 'a') # crea archivo por emsemble
					# outFile.write(url+'\n')
					# outFile.close()
					file_name = url.split('/')[-1]
					u = urllib2.urlopen(url)
					meta = u.info()
					file_size = int(meta.getheaders("Content-Length")[0])
					# print ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name, os.path.getsize( ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+ file_name) / float(1000000)
					
					if os.path.exists(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name) and os.path.getsize( ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+ file_name) < file_size:
						print '\n---remove file incomplete in outdir: mb', file_name, 'size', os.path.getsize( ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+ file_name) / float(1000000)
						os.remove(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name)
					
					if os.path.exists(dirbase+'\\'+ file_name) and os.path.getsize(dirbase+'\\'+file_name) < file_size:
						print "\n---file incomplete, remove file:", file_name, os.path.getsize(dirbase+'\\'+file_name)/float(1000000), ' < ' ,file_size/float(1000000)
						os.remove(dirbase+'\\'+ file_name)		
					print url, '\n'
					# else:
					# if not os.path.exists(dirbase+'\\'+ l.split(".")[24].split("\n")[0] +".nc") and not os.path.exists(ourdir+ l.split(".")[24].split("_")[1]+'\\'+ l.split(".")[24].split("_")[7] +'\\'+ l.split(".")[24].split("_")[3] +'\\'+ l.split(".")[24].split("_")[5] +'\\'+ l.split(".")[24].split("_")[4]+'\\'+l.split(".")[24].split("\n")[0] +".nc"):
					if not os.path.exists(dirbase+'\\'+ file_name) and not os.path.exists(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name):
						
						# txtfile = open(dirbase+'cordex.txt')
						# for period in txtfile:
							# url = l.split('\n')[0] + '_' + period.split('\n')[0] + ".nc"
							
						file_name = url.split('/')[-1]
						u = urllib2.urlopen(url)
						f = open(file_name, 'wb')
						meta = u.info()
						file_size = int(meta.getheaders("Content-Length")[0])
						print l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name
						print "\n---Downloading: %s Mb: %s" % (file_name, file_size/float(1000000))

						file_size_dl = 0
						block_sz = 8192
						while True:
							buffer = u.read(block_sz)
							if not buffer:
								break

							file_size_dl += len(buffer)
							f.write(buffer)
							status = r"%10d  [%3.2f%%]" % (file_size_dl, file_size_dl * 100. / file_size)
							status = status + chr(8)*(len(status)+1)
							print status,
							
						f.close()
						
				except:
					pass
				break	

			
					# Mueve archivo	
					
					# os.chdir(dirbase)
					# lista = glob.glob("*.nc")
			if os.path.exists(dirbase+'\\'+ file_name):		
				# w = l.split(".")[24].split("\n")[0] +".nc"
				w = file_name
				
				# for w in lista:
				# CREATE FOLDER
				if not os.path.isdir(ourdir+'\\'+ w.split("_")[1] ):
					print "...Create Folder " + w.split("_")[1]
					os.makedirs(ourdir+'\\'+ w.split("_")[1] )
				if not os.path.isdir(ourdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] ):
					print "...Create Folder " + w.split("_")[7]
					os.makedirs(ourdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7])
				if not os.path.isdir(ourdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] ):
					print "...Create Folder " + w.split("_")[3]
					os.makedirs(ourdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3])
				if not os.path.isdir(ourdir +'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7]+'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5]  ):
					print "...Create Folder " + w.split("_")[5]
					os.makedirs(ourdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] )
				if not os.path.isdir(ourdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4]  ):
					print "...Create Folder " + w.split("_")[4]
					os.makedirs(ourdir +'\\'+ w.split("_")[1]+'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4] )	

				####COPY FILE AND DELETE FILE
				if os.path.exists(dirbase+ w) and not os.path.exists(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name): ### si no existe el file destino
					print '\n----copy file:', w
					#shutil.copy2(dirbase+ w, ourdir+'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4] )
					os.system("robocopy "+ dirbase + " " + ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4] +' ' +file_name + " /Z")
					print '--move file done!'
					
				if os.path.exists(dirbase+ w) and os.path.exists(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name) and file_size == os.path.getsize(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name):
						print '\n----File exist!, delete file:', dirbase+w, os.path.getsize(dirbase+w), ' = size exits file: ', os.path.getsize(ourdir+ l.split('_')[1] + '\\' + l.split('_')[7].split('\n')[0]+ '\\' + l.split('_')[3] + '\\' + l.split('_')[5] + '\\' + l.split('_')[4]+'\\'+file_name)/float(1000000) 
						os.remove(dirbase+ file_name)
						
					# if dirbase+ w == dirbase+'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4] +'\\'+w and os.path.getsize(dirbase+ w) == os.path.getsize( dirbase+'\\'+ w.split("_")[1] +'\\'+ w.split("_")[7] +'\\'+ w.split("_")[3] +'\\'+ w.split("_")[5] +'\\'+ w.split("_")[4] +'\\'+w ):
						# print '----delete file:', dirbase+w
						# os.remove(dirbase+ w)				
					
					# else:
						# print 'eliminado archivo repetido: ', w
						# os.remove(dirbase+ w)	

		
print "DONE !!!!"
		