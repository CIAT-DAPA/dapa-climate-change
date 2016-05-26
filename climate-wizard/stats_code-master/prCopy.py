__author__ = 'Edward Guevara'
from os import system, path
from sys import argv

if len(argv) != 3:
    print "Usage: python prCopy.py MODEL Scenario"
    exit(1)
MODEL = str(argv[1])
Scen = str(argv[2])

for year in range(2006, 2100):
    fs = '/box0_p2/BCSD/%s/%s/day/r1i1p1/pr/pr_day_%s_%s_r1i1p1_%s.nc' % (MODEL, Scen, MODEL, Scen, year)
    # print fs
    fc = '/mnt/BCSD/%s/junk/prmm_day_%s_%s_r1i1p1_%s.nc' % (MODEL, MODEL, Scen, year)
    # print fc
    ft = '/mnt/BCSD/%s/%s/day/r1i1p1/pr/pr_day_%s_%s_r1i1p1_%s.nc' % (MODEL, Scen, MODEL, Scen, year)
    # print ft
    if not path.exists(fc):
        print '... copying %s' % ft
        system('cp %s %s' % (fs, ft))
    else:
        print "\n... nothing to do, %s exist!\n" % fs
