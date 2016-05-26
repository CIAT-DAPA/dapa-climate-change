#!/usr/local/cdat5.2/bin/python

#Set up to use CDO utilities to calculate extremes
#daily downscaled files are in different directories for each variable, scenario and run
#input is one file per year
#some statistics are calculated relative to reference period, all of which must exist.
#For this code, the ref period conincides with the historical period

import cdms2
import os,string,sys
import daily_extreme_stats_temp as Tstats
import daily_extreme_stats_precip as Pstats

#variables to process: 'txavg','tnavg','txx','tnn','tx90','tx90p','tx10p','tn90p','tn10p','fd','gsl','hwdi','cd18','hd18','gd10','ptot','cdd','r02','r5d','r90p','sdii'

#vars = ['txavg','tnavg','txx','tnn','tx90','tx90p','tx10p','tn90p','tn10p','fd','gsl','hwdi','cd18','hd18','gd10','ptot','cdd','r02','r5d','r90p','sdii']
vars = ['ptot','cdd','r02','r5d','r90p','sdii']

#define reference historical period
StRefh = 1961
EnRefh = 1990

#define complete historical period
StRef = 1961
EnRef = 1999

#future periods
StYrs = [2046, 2081]
EnYrs = [2065, 2100]

#root dir needs to match list of runs below - they need to be located there
#RootDir = '/mnt/WorldBank1'
RootDir = '/media/data'

#input list created by get_run_lists.scr
inlist = './run_list_mri_b1_run1.txt'

#land-sea mask
lsm = "./global_0.5deg_LSM.nc"

#read in the list
models = []
scens = []
runs = []
f = open(inlist,'r')
for line in f.readlines():
    junk = line.split()
    models.append(junk[0])   #like bccr_bcm2_0
    scens.append(junk[1])    #sresa2, sresa1b, sresb1
    runs.append(junk[2])     #entire string like 'run1' must exist for 20c3m too
f.close()

for i in range(len(models)):
    #within each directory, subdirectories for each variable: pr, tasmax, tasmin
    ind20 = RootDir + "/" + models[i] + "/" + '20c3m' + "/" + runs[i]
    ind21 = RootDir + "/" + models[i] + "/" + scens[i] + "/" + runs[i]

    #raw daily input filename is of form miub_echo_g.sresb1.run1.pr_0.5deg_2046.nc
    #tas file names are calculated, so no raw directory needed
    #fn20tx = ind20+"/tasmax/"+models[i]+"."+'20c3m.'+runs[i]+".tasmax"+"_BCSD_0.5_2deg_"
    #fn20tn = ind20+"/tasmin/"+models[i]+"."+'20c3m.'+runs[i]+".tasmin"+"_BCSD_0.5_2deg_"
    #fn20tg = models[i]+"."+'20c3m.'+runs[i]+".tas"+"_BCSD_0.5_2deg_"
    fn20pr = ind20+"/pr/"+models[i]+"."+'20c3m.'+runs[i]+".pr"+"_BCSD_0.5_2deg_"
    #fn21tx = ind21+"/tasmax/"+models[i]+"."+scens[i]+"."+runs[i]+".tasmax"+"_BCSD_0.5_2deg_"
    #fn21tn = ind21+"/tasmin/"+models[i]+"."+scens[i]+"."+runs[i]+".tasmin"+"_BCSD_0.5_2deg_"
    #fn21tg = models[i]+"."+scens[i]+"."+runs[i]+".tas"+"_BCSD_0.5_2deg_"
    fn21pr = ind21+"/pr/"+models[i]+"."+scens[i]+"."+runs[i]+".pr"+"_BCSD_0.5_2deg_"

    ####################################################################################
    #CopyFiles really just creates soft links to files for existing variables
    #working directory is hardcoded in two py modules.
    #Also, tavg ('tas') is needed for many stats, calculate up front
    #Tstats.CopyFiles(fn20tx,StRef,EnRef)
    #for j in range(len(StYrs)):
    #    Tstats.CopyFiles(fn21tx,StYrs[j],EnYrs[j])
    #Tstats.CopyFiles(fn20tn,StRef,EnRef)
    #for j in range(len(StYrs)):
    #    Tstats.CopyFiles(fn21tn,StYrs[j],EnYrs[j])
    Tstats.CopyFiles(fn20pr,StRef,EnRef)
    for j in range(len(StYrs)):
        Tstats.CopyFiles(fn21pr,StYrs[j],EnYrs[j])
    #print fn20tn,fn20tx,StRef,EnRef
    #Tstats.CalcTavg(fn20tn,fn20tx,StRef,EnRef)
    #for j in range(len(StYrs)):
    #    Tstats.CalcTavg(fn21tn,fn21tx,StYrs[j],EnYrs[j])
    
    #Monthly mean maximum temperatures
    if 'txavg' in vars:
        of = Tstats.TAVG(fn20tx,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.TAVG(fn21tx,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
            #modify variable name, history, global attributes

    #Monthly mean minimum temperatures
    if 'tnavg' in vars:
        of = Tstats.TAVG(fn20tn,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.TAVG(fn21tn,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
            #modify variable name, history, global attributes

    #Monthly maximum temperatures
    if 'txx' in vars:
        of = Tstats.TXX(fn20tx,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.TXX(fn21tx,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
            #modify variable name, history, global attributes

    #Monthly minimum temperatures
    if 'tnn' in vars:
        of = Tstats.TNN(fn20tn,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.TNN(fn21tn,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #90th percentile Tmax - one value per year
    if 'tx90' in vars:
        of = Tstats.TX90(fn20tx,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.TX90(fn21tx,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tx90p' in vars:
        oftx90p_ref = Tstats.Tref(fn20tx,90,'tasmax',StRefh,EnRefh)
        print "created outfile %s\n" % (oftx90p_ref)
        of = Tstats.TP(fn20tx,90,'tasmax',oftx90p_ref,StRef,EnRef)
        for j in range(len(StYrs)):
            of = Tstats.TP(fn21tx,90,'tasmax',oftx90p_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
    #Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tx10p' in vars:
        oftx90p_ref = Tstats.Tref(fn20tx,10,'tasmax',StRefh,EnRefh)
        print "created outfile %s\n" % (oftx90p_ref)
        of = Tstats.TP(fn20tx,10,'tasmax',oftx90p_ref,StRef,EnRef)
        for j in range(len(StYrs)):
            of = Tstats.TP(fn21tx,10,'tasmax',oftx90p_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tn90p' in vars:
        oftx90p_ref = Tstats.Tref(fn20tn,90,'tasmin',StRefh,EnRefh)
        print "created outfile %s\n" % (oftx90p_ref)
        of = Tstats.TP(fn20tn,90,'tasmin',oftx90p_ref,StRef,EnRef)
        for j in range(len(StYrs)):
            of = Tstats.TP(fn21tn,90,'tasmin',oftx90p_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tn10p' in vars:
        oftx90p_ref = Tstats.Tref(fn20tn,10,'tasmin',StRefh,EnRefh)
        print "created outfile %s\n" % (oftx90p_ref)
        of = Tstats.TP(fn20tn,10,'tasmin',oftx90p_ref,StRef,EnRef)
        for j in range(len(StYrs)):
            of = Tstats.TP(fn21tn,10,'tasmin',oftx90p_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Frost days
    if 'fd' in vars:
        of = Tstats.FD(fn20tn,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.FD(fn21tn,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
            
    #Thermal growing season length
    if 'gsl' in vars:
        of = Tstats.GSL(fn20tn,fn20tx,lsm,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.GSL(fn21tn,fn21tx,lsm,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
    
    #Heat wave duration index wrt mean of reference_period
    if 'hwdi' in vars:
        oftxnorm_ref = Tstats.TXnorm(fn20tx,StRefh,EnRefh)
        print "created outfile %s\n" % (oftxnorm_ref)
        of = Tstats.HWDI(fn20tx,oftxnorm_ref,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.HWDI(fn21tx,oftxnorm_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #cooling degree days
    if 'cd18' in vars:
        of = Tstats.CD18(fn20tg,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.CD18(fn21tg,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #heating degree days
    if 'hd18' in vars:
        of = Tstats.HD18(fn20tg,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.HD18(fn21tg,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #growing degree days
    if 'gd10' in vars:
        of = Tstats.GD10(fn20tg,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Tstats.GD10(fn21tg,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
            
    ###########################################################################

    #Monthly total precip
    if 'ptot' in vars:
        of = Pstats.Ptot(fn20pr,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.Ptot(fn21pr,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Consecutive dry days
    if 'cdd' in vars:
        of = Pstats.CDD(fn20pr,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.CDD(fn21pr,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Number of wet days > 0.2 mm/d
    if 'r02' in vars:
        of = Pstats.R02(fn20pr,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.R02(fn21pr,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Max consec 5 day precip
    if 'r5d' in vars:
        of = Pstats.R5D(fn20pr,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.R5D(fn21pr,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #Pct of time precip exceeds ref pd 90th percentile (wet day values)
    #calculate % of precip due to this too
    if 'r90p' in vars:      
        ofpr90_ref = Pstats.R90ref(fn20pr,StRefh,EnRefh)
        print "created outfile %s\n" % (ofpr90_ref)
        of = Pstats.R90P(fn20pr,ofpr90_ref,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.R90P(fn21pr,ofpr90_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)
        of = Pstats.R90PTOT(fn20pr,ofpr90_ref,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.R90PTOT(fn21pr,ofpr90_ref,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

    #simple daily precip intensity index
    if 'sdii' in vars:
        of = Pstats.SDII(fn20pr,StRef,EnRef)
        print "created outfile %s\n" % (of)
        for j in range(len(StYrs)):
            of = Pstats.SDII(fn21pr,StYrs[j],EnYrs[j])
            print "created outfile %s\n" % (of)

#done
