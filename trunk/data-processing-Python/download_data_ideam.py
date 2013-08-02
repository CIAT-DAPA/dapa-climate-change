
import cookielib
import urllib
import urllib2
import re
import os, sys, glob, string, shutil, errno

############python G:\jtarapues\ideam\script\dwnload_data_ideam.py G:\jtarapues\ideam\descargas 1960-01-01 2013-04-10

dirout = sys.argv[1]
start = sys.argv[2]
end = sys.argv[3]
#################################### Store the cookies and create an opener that will hold them
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    DOWNLOAD DATA IDEAM "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n Date start: ", start, " - Date end: ", end, "\n"

txtfile = open('G:\jtarapues\ideam\script\list_all.txt')
txtstation = open('G:\jtarapues\ideam\script\list_var_station.txt')

for line in txtfile: 
	if not os.path.exists(dirout + '\\' + line.split('=')[3].split('&')[0] + '\\'+ (line.split('=')[2].split('&')[0])[2:] + ".txt"):

		cj = cookielib.CookieJar()
		opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))

		################Add our headers
		opener.addheaders = [('User-agent', 'RedditTesting')]

		############################ Install our opener (note that this changes the global opener to the one
		############################ we just made, but you can also just call opener.open() if you want)
		urllib2.install_opener(opener)

		############################ The action/ target from the form
		authentication_url = 'http://hydras3.ideam.gov.co/CheckLog.htm'

		############################ Input parameters we are going to send
		payload = {
		  'submit': 'Ingresar',
		  'user': 'PAJONALES',
		  'pwd': 'pajonales'
		  }

		################ Use urllib to encode the payload
		data = urllib.urlencode(payload)

		################ Build our Request object (supplying 'data' makes it a POST)
		req = urllib2.Request(authentication_url, data)

		############################Make the request and read the response
		resp = urllib2.urlopen(req)
		contents = resp.read()
		#### print contents.split('"')[5].split("=")[1]

		#### print line.split('\n')[0] + contents.split('"')[5].split("=")[1] +contents.split('"')[5].split("=")[1]+ '&start=' + start.replace("-", "") + '&end=' + end.replace("-", "") + '&ymin=0&ymax=50&res=640x480&submit=Tabla'
		response = urllib2.urlopen( line.split('\n')[0] + contents.split('"')[5].split("=")[1] +contents.split('"')[5].split("=")[1]+ '&start=' + start.replace("-", "") + '&end=' + end.replace("-", "") + '&ymin=0&ymax=50&res=640x480&submit=Tabla')
		html = response.read()
		data = html.replace("</td><td>", "\t").replace("<tr><td>", "").replace("</td></tr>", "").replace("<table border><tr><th>", "").replace("</th><th>", "\t").replace("</th></tr>", "").replace("</th></tr>", "").replace("Fecha", "Date").replace("Hora", "Hour").replace("Valor", "Value").replace('<HTML><HEAD><TITLE>HYDRAS3 Data</TITLE><meta http-equiv="expires" content="0"></HEAD><BODY>', "").replace('</table></BODY></HTML>', "").replace('---[5]', "NA").replace('---[10]', "NA").replace('---[15]', "NA").replace('---', "NA").replace('---[2]', "NA").replace('---[1]', "NA")
		#### print dirout + '\\' + line.split('=')[3].split('&')[0] + '\\'+ (line.split('=')[2].split('&')[0])[1:] + ".txt"
		print line.split('\n')[0] + contents.split('"')[5].split("=")[1] +contents.split('"')[5].split("=")[1]+ '&start=' + start.replace("-", "") + '&end=' + end.replace("-", "") + '&ymin=0&ymax=50&res=640x480&submit=Tabla'
		while True:
			try:
				if data.split('\t')[2].split('\n')[1].isdigit(): ############ si existen datos crea el archivo
					## for station in txtstation:
						## if line.split('=')[3] == station.split('\t')[0]:
							## if not os.path.isdir(dirout+'\\'+station.split('\t')[1].replace(" ","_") ): ############crea carpeta variable
								## os.makedirs(dirout+'\\'+ station.split('\t')[1].replace(" ","_")) 
								## outFile = open(dirout + '\\' + station.split('\t')[1].replace(" ","_") + '\\'+ (line.split('=')[2].split('&')[0])[2:] + ".txt" , "a") ############ crea archivo por emsemble
					if not os.path.isdir(dirout+'\\'+line.split('=')[3].split('&')[0] ): ############crea carpeta variable
						os.makedirs(dirout+'\\'+ line.split('=')[3].split('&')[0]) 
					outFile = open(dirout + '\\' + line.split('=')[3].split('&')[0] + '\\'+ (line.split('=')[2].split('&')[0])[2:] + ".txt" , "a")
					outFile.write(str(data[2:-4])+'\n')
					outFile.close()
					print '---station: ', line.split('=')[2].split('&')[0], " var: ", line.split('=')[3].split('&')[0], '++done++'			

			except:
				pass
			break	
print 'done all!!!'	

