__author__ = 'edarague'

from sys import argv
from os import path, system

if len(argv) != 2:
    print "Usage: python tasmin_tasmax_avgyear.py list_monthly.txt"
    exit(1)

# Open a file
fi = open(argv[1], "r")
print "Name of the file: ", fi.name

for fn in fi.readlines():
    fn = fn.replace('./', '/mnt/out_stats/')[:-1]
    fo = fn.replace('.monthly', '')
    if not path.exists(fo):
        print "\n... year summary: %s" % (fn)
        txtcmd = "cdo -m 1e+20 yearmean " + fn + " " + fo
        print txtcmd
        system(txtcmd)
    else:
        raise Exception('infile not found: %s' % fn)

# Close opend file
fi.close()
