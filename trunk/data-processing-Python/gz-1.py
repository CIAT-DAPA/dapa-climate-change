#!/usr/bin/env python
import os, sys, gzip, string, tempfile

class ZipManipulator:
    """This class does the work of manipulating gz files.
    Pass the repository as an argument during instantiation."""
   
    def __init__(self, input):
        self.repository = input
   
    def files(self, root):
       for path, folders, files in os.walk(root):
           for file in files:
               yield path, file
   
    def starter(self, filetype, work_type):
        '''Does the gzipping or gunzipping of given files.
        work_type -> 0 is for gzip
        work_type  -> 1 is for gunzip
        fileype -> is the type of file. Eg: html, txt, html.gz, txt.gz, HTML.gz et cetera'''
       
        for path, file in self.files(self.repository):
        #for path, file in files(repository):
            if file.endswith(filetype):
                if work_type == 0:
                    os.chdir(os.path.realpath(path))
                    ZipManipulator.gzip(self, file)
                elif work_type == 1:
                    os.chdir(os.path.realpath(path))
                    ZipManipulator.gunzip(self, file)
                else:
                    sys.stdout.write("Incorrect work type passed.\n")
               
           
    def gzip(self, file):
        '''Gzip the given file and then remove the file.'''
        r_file = open(file, 'r')
        w_file = gzip.GzipFile(file + '.gz', 'w', 9)
        w_file.write(r_file.read())
        w_file.flush()
        w_file.close()
        r_file.close()
        os.unlink(file) #We don't need the file now
        sys.stdout.write("%s gzipped.\n" % (file))
               
    def gunzip(self, file):
        '''Gunzip the given file and then remove the file.'''
        r_file = gzip.GzipFile(file, 'r')
        write_file = string.rstrip(file, '.gz')
        w_file = open(write_file, 'w')
        w_file.write(r_file.read())
        w_file.close()
        r_file.close()
        os.unlink(file) # Yes this one too.
        sys.stdout.write("%s gunzipped.\n" % (file))
       
def main():
    repository = raw_input("Enter the repository path: ")
    if repository == "":
        repository = os.curdir
       
    filetype = raw_input("Enter the filetype: ")
    zip_type = int(raw_input("Enter 0 for gzip and 1 for gunzip: "))
   
    instance = ZipManipulator(repository)
    instance.starter(filetype, zip_type)
   
if __name__ == '__main__':
    main() 