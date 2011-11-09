#!/usr/bin/env python
# Author: Johannes Signer
# Contact: jmsigner@gmail.com
# Date: 8.8.2011
# License: GPLv2

# import models
import hashlib, os, shutil, sys, time, zipfile


# zip recursevily
def zipper(dir, zip_file):
  zip = zipfile.ZipFile(zip_file, 'w', compression=zipfile.ZIP_DEFLATED)
  root_len = len(os.path.abspath(dir))
  for root, dirs, files in os.walk(dir):
    archive_root = os.path.abspath(root)[root_len:]
    for f in files:
      fullpath = os.path.join(root, f)
      archive_name = os.path.join(archive_root, f)
      print f
      zip.write(fullpath, archive_name, zipfile.ZIP_DEFLATED)
  zip.close()

# ccp: copy files between two destination and make sure none of the files are lost and files are complete.
def ccp(org, dest):
  success = False
  t_trial = 0
  while not success or t_trail != cp_trials:
    try:
      h1 = md5Checksum(org)
      shutil.copy2(org, dest)
      h2 = md5Checksum(dest)
      if h1 == h2:
        success = true
    except:
      time.sleep(cp_wait)
      t_trial += 1
      continue
  return success

# Calac md5Checksum from file (www.joelverhagen.com/blog/2011/02/md5-hash-of-file-in-python/)
def md5Checksum(filePath):
  fh = open(filePath, 'rb')
  m = hashlib.md5()
  while True:
     data = fh.read(8192)
     if not data:
       break
     m.update(data)
  return m.hexdigest()

def main():
  # check arguments
  if (len(sys.argv) >= 6 ):
	in_file = sys.argv[1]
	out_file = sys.argv[2]
	where_marksim = sys.argv[3]
	dirout = sys.argv[4]
	base = sys.argv[5]
  
  else:
    sys.exit("not three arguments provided")
  
  # # outidr
  # base = '_'.join(in_file.split('\\')[-2:]).split(".")[0]
  
  # 1. check if file exists
  if not os.path.exists(in_file):
    sys.exit("in_file does not exists")
  if not os.path.exists(where_marksim):
    sys.exit("marksim does not exists")
  
  # make temp structure on C:\
  if not os.path.exists(dirout + base ):
    os.mkdir(dirout + base + "")
  if not os.path.exists(dirout + base + "/data"):
    os.mkdir(dirout + base + "/data")
  if not os.path.exists(dirout + base + "/lib"):
    os.mkdir(dirout + base + "/lib")
  
  
  # cp gcm to local drive
  shutil.copy2(in_file, dirout + base + "/data")
  this_gcm = os.listdir(dirout + base + "/data")
  
  # cp marksim 
  shutil.copy2(where_marksim, dirout + base + "/lib")
  
  # unzip marksim
  zip = zipfile.ZipFile(dirout + base + "/lib/marksim.zip")
  
  for f in zip.namelist():
	open(dirout + base + "\\lib\\" + f, 'wb').write(zip.read(f))
  zip.close()
  
  # write MASTER_PATH
  f = open(dirout + base + "/lib/MASTER_PATH.CTR","w")
  f.write(dirout + base + "\\run.LST")
  f.close()
  
  
  print "copied everyting", "-----------------"
  
  # Run Marksim for each point
  zip = zipfile.ZipFile(dirout + base + "/data/" + this_gcm[0])
  for p in zip.namelist():
	print p
  count = 1
  for p in zip.namelist():
    if p[-4:] == '.dat':
		print p
		contents = zip.read(p)
		f_name = p[:-4] 
		f_parent = str(int(f_name) + 100000)[0:3]
		if not os.path.exists(dirout + base + "/data/" + f_parent): 
			os.mkdir(dirout + base + "/data/" + f_parent)
			f_path = dirout + base + "/data/" + f_parent + "/" + f_name
		if not os.path.exists(f_path): 
			os.mkdir(f_path)
		# write the *.dat file
		f = open(f_path + '/' + f_name + '.dat','w')
		f.write(contents)
		f.close()
		# write *.LST

		f = open(dirout + base + "/run.LST","w")
		f.write(dirout + base + "\\data\\" + f_parent + "\\" + f_name + "\\\n" +  dirout + base + "\\data\\" + f_parent + "\\" + f_name + "\\\n" + "tmp")
		f.close()
		# write *.CBF
		f = open(f_path + '/' + 'tmp.CBF','w')
		f.write(dirout + base + "\\data\\" + f_parent + "\\" + f_name + "\\" + f_name + ".dat")
		f.close()
		# write *.XBF
		f = open(f_path + '/' + 'tmp.XBF','w')
		f.write(dirout + base + "\\data\\" + f_parent + "\\" + f_name + "\\" + f_name + ".CLX," + f_name + ",1234,99,d")
		f.close()
		os.system(dirout + base + "/lib/MarkSim_v1.2.exe")
		count += 1
		# UNcomment the following line once testing is done!
		# if count == 3: break
  zip.close()
  
  # # Zip results
  # target_dir = dirout + base + "/data"
  # target_zip = dirout + base + ".zip "
  # print "zipping results"
  # os.system(dirout + base + "\\lib\\7za.exe a -tzip " + target_zip +  target_dir)
  # print dirout + base + "\\lib\\7za.exe a -tzip"  + target_dir
  # print "now cleaning"
  # os.system("rd /s /q C:\\tmp_marksim_" + base + "\\data")
  
  # while True:
    # print "trying to copy"
    # md5_1 = md5Checksum(target_zip)
    # target_cp = out_file + "\\" + base + ".zip"
    # os.system("COPY " + target_zip + " " + target_cp)
    # md5_2 = md5Checksum(target_cp)
    # if md5_1 == md5_2: break
  
  # os.system("rd /s /q C:\\tmp_marksim_" + base)
  


if __name__ == "__main__":
  main()
