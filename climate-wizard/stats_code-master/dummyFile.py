__author__ = 'Edward Guevara'
from os import system, path
from sys import argv

if len(argv) != 3:
    print "Usage: python dummyFile.py MODEL Scenario"
    exit(1)

MODEL = str(argv[1])
Scen = str(argv[2])

for year in range(2006, 2100):
    fn = '/mnt/BCSD/%s/%s/day/r1i1p1/pr/pr_day_%s_%s_r1i1p1_%s.nc' % (MODEL, Scen, MODEL, Scen, year)
    if not path.exists(fn):
        print '... touch %s' % fn
        system('touch %s' % fn)
    else:
        print "\n... nothing to do, %s exist!\n" % fn
