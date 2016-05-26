#!/usr/local/cdat5.2/bin/python

# Set up to use CDO utilities to calculate extremes
# daily downscaled files are in different directories for each variable, scenario and run
# input is one file per year
# some statistics are calculated relative to reference period, all of which must exist.
# For this code, the ref period conincides with the historical period

from sys import exit, argv
from os import path, mkdir

if len(argv) != 2:
    print "Usage: python daily_extreme_stats_cdo_main.py MODEL_NAME"
    exit(1)

import daily_extreme_stats_temp as t_stats
import daily_extreme_stats_precip as p_stats

# var_stat = ['txavg', 'tnavg', 'txx', 'tnn', 'gd10', 'hd18', 'cd18', 'ptot', 'cdd', 'r02', 'r5d', 'sdii']
var_stat = ['txavg', 'tnavg', 'txx', 'tnn', 'gd10', 'hd18', 'cd18', 'ptot', 'cdd', 'r02', 'r5d', 'sdii']

# define reference historical period
StRefHis = 1950
EnRefHis = 1999

# define complete historical period
StComHis = 1950
EnComHis = 2005

# future periods
StYrsFut = 2006
EnYrsFut = 2099

# root dir needs to match list of runs below - they need to be located there
RootDir = '/mnt/BCSD'

# model name
model = str(argv[1])

# land-sea mask
lsm = "./global_0.25deg_LSM.nc"

if not path.isdir('/mnt/out_stats/' + model):
    mkdir('/mnt/out_stats/' + model)

# within each directory, subdirectories for each variable: pr, tasmax, tasmin
# copy_files really just creates soft links to files for existing variables
# working directory is hardcoded in two py modules.
# Also, tavg ('tas') is needed for many stats, calculate up front
for scens in ('historical', 'rcp45', 'rcp85'):
    if scens == 'historical':
        ind_hist = RootDir + "/" + model + "/historical/day/r1i1p1"

        # soflink tasmin historical
        fn_hist_tn = ind_hist + "/tasmin/tasmin_day_" + model + "_historical_r1i1p1_"
        t_stats.copy_files(fn_hist_tn, StComHis, EnComHis, model)

        # soflink tasmax historical
        fn_hist_tx = ind_hist + "/tasmax/tasmax_day_" + model + "_historical_r1i1p1_"
        t_stats.copy_files(fn_hist_tx, StComHis, EnComHis, model)

        # soflink pr historical
        fn_hist_pr = ind_hist + "/pr/pr_day_" + model + "_historical_r1i1p1_"
        t_stats.copy_files(fn_hist_pr, StComHis, EnComHis, model)

        # calculate tmean historical
        t_stats.calc_tavg(fn_hist_tn, fn_hist_tx, StComHis, EnComHis, model)

        # soflink tmean historical
        fn_hist_tas = RootDir + "/" + model + "/junk/tas_day_" + model + "_historical_r1i1p1_"
        t_stats.copy_files(fn_hist_tas, StComHis, EnComHis, model)

        # calculate pr in mm historical
        p_stats.calc_ppmm(fn_hist_pr, StComHis, EnComHis, model)
        # soflink prmm historical
        fn_hist_prmm = RootDir + "/" + model + "/junk/prmm_day_" + model + "_historical_r1i1p1_"
        t_stats.copy_files(fn_hist_prmm, StComHis, EnComHis, model)
    else:
        ind_rcp = RootDir + "/" + model + "/" + scens + "/day/r1i1p1"

        # soflink tasmin rcp
        fn_rcp_tn = ind_rcp + "/tasmin/tasmin_day_" + model + "_" + scens + "_r1i1p1_"
        t_stats.copy_files(fn_rcp_tn, StYrsFut, EnYrsFut, model)

        # soflink tasmax rcp
        fn_rcp_tx = ind_rcp + "/tasmax/tasmax_day_" + model + "_" + scens + "_r1i1p1_"
        t_stats.copy_files(fn_rcp_tx, StYrsFut, EnYrsFut, model)

        # soflink pr rcp
        fn_rcp_pr = ind_rcp + "/pr/pr_day_" + model + "_" + scens + "_r1i1p1_"
        t_stats.copy_files(fn_rcp_pr, StYrsFut, EnYrsFut, model)

        # calculate tmean rcp
        t_stats.calc_tavg(fn_rcp_tn, fn_rcp_tx, StYrsFut, EnYrsFut, model)

        # soflink tmean rcp
        fn_rcp_tas = RootDir + "/" + model + "/junk/tas_day_" + model + "_" + scens + "_r1i1p1_"
        t_stats.copy_files(fn_rcp_tas, StYrsFut, EnYrsFut, model)

        # calculate pr in mm rcp
        p_stats.calc_ppmm(fn_rcp_pr, StYrsFut, EnYrsFut, model)
        # soflink prmm rcp
        fn_rcp_prmm = RootDir + "/" + model + "/junk/prmm_day_" + model + "_" + scens + "_r1i1p1_"
        t_stats.copy_files(fn_rcp_prmm, StYrsFut, EnYrsFut, model)

    # Monthly mean maximum temperatures historical
    if 'txavg' in var_stat and scens == 'historical':
        of = t_stats.tavg(fn_hist_tx, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'txavg' in var_stat and scens != 'historical':
        of = t_stats.tavg(fn_rcp_tx, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of
        # modify variable name, history, global attributes

    # Monthly mean minimum temperatures
    if 'tnavg' in var_stat and scens == 'historical':
        of = t_stats.tavg(fn_hist_tn, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'tnavg' in var_stat and scens != 'historical':
        of = t_stats.tavg(fn_rcp_tn, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of
        # modify variable name, history, global attributes

    # Monthly maximum temperatures
    if 'txx' in var_stat and scens == 'historical':
        of = t_stats.txx(fn_hist_tx, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'txx' in var_stat and scens != 'historical':
        of = t_stats.txx(fn_rcp_tx, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of
        # modify variable name, history, global attributes

    # Monthly minimum temperatures
    if 'tnn' in var_stat and scens == 'historical':
        of = t_stats.tnn(fn_hist_tn, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'tnn' in var_stat and scens != 'historical':
        of = t_stats.tnn(fn_rcp_tn, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # growing degree days
    if 'gd10' in var_stat and scens == 'historical':
        of = t_stats.gd10(fn_hist_tas, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'gd10' in var_stat and scens != 'historical':
        of = t_stats.gd10(fn_rcp_tas, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # heating degree days
    if 'hd18' in var_stat and scens == 'historical':
        of = t_stats.hd18(fn_hist_tas, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'hd18' in var_stat and scens != 'historical':
        of = t_stats.hd18(fn_rcp_tas, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # cooling degree days
    if 'cd18' in var_stat and scens == 'historical':
        of = t_stats.cd18(fn_hist_tas, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'cd18' in var_stat and scens != 'historical':
        of = t_stats.cd18(fn_rcp_tas, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # 90th percentile Tmax - one value per year
    if 'tx90' in var_stat:
        of = t_stats.TX90(fn_hist_tx, StComHis, EnComHis)
        print "created outfile %s\n" % of
        of = t_stats.TX90(fn_rcp_tx, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tx90p' in var_stat:
        oftx90p_ref = t_stats.Tref(fn_hist_tx, 90, 'tasmax', StRefHis, EnRefHis)
        print "created outfile %s\n" % oftx90p_ref
        of = t_stats.TP(fn_hist_tx, 90, 'tasmax', oftx90p_ref, StComHis, EnComHis)
        of = t_stats.TP(fn_rcp_tx, 90, 'tasmax', oftx90p_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tx10p' in var_stat:
        oftx90p_ref = t_stats.Tref(fn_hist_tx, 10, 'tasmax', StRefHis, EnRefHis)
        print "created outfile %s\n" % oftx90p_ref
        of = t_stats.TP(fn_hist_tx, 10, 'tasmax', oftx90p_ref, StComHis, EnComHis)
        of = t_stats.TP(fn_rcp_tx, 10, 'tasmax', oftx90p_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tn90p' in var_stat:
        oftx90p_ref = t_stats.Tref(fn_hist_tn, 90, 'tasmin', StRefHis, EnRefHis)
        print "created outfile %s\n" % oftx90p_ref
        of = t_stats.TP(fn_hist_tn, 90, 'tasmin', oftx90p_ref, StComHis, EnComHis)
        of = t_stats.TP(fn_rcp_tn, 90, 'tasmin', oftx90p_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Generalized version: Pct of time T doesn't exceeds ref pd Nth percentile
    if 'tn10p' in var_stat:
        oftx90p_ref = t_stats.Tref(fn_hist_tn, 10, 'tasmin', StRefHis, EnRefHis)
        print "created outfile %s\n" % oftx90p_ref
        of = t_stats.TP(fn_hist_tn, 10, 'tasmin', oftx90p_ref, StComHis, EnComHis)
        of = t_stats.TP(fn_rcp_tn, 10, 'tasmin', oftx90p_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Frost days
    if 'fd' in var_stat:
        of = t_stats.FD(fn_hist_tn, StComHis, EnComHis)
        print "created outfile %s\n" % of
        of = t_stats.FD(fn_rcp_tn, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Thermal growing season length
    if 'gsl' in var_stat:
        of = t_stats.GSL(fn_hist_tn, fn_hist_tx, lsm, StComHis, EnComHis)
        print "created outfile %s\n" % of
        of = t_stats.GSL(fn_rcp_tn, fn_rcp_tx, lsm, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

    # Heat wave duration index wrt mean of reference_period
    if 'hwdi' in var_stat:
        oftxnorm_ref = t_stats.TXnorm(fn_hist_tx, StRefHis, EnRefHis)
        print "created outfile %s\n" % oftxnorm_ref
        of = t_stats.HWDI(fn_hist_tx, oftxnorm_ref, StComHis, EnComHis)
        print "created outfile %s\n" % of
        of = t_stats.HWDI(fn_rcp_tx, oftxnorm_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of


    ###########################################################################

    # Monthly total precip
    if 'ptot' in var_stat and scens == 'historical':
        of = p_stats.ptot(fn_hist_prmm, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'ptot' in var_stat and scens != 'historical':
        of = p_stats.ptot(fn_rcp_prmm, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # Consecutive dry days
    if 'cdd' in var_stat and scens == 'historical':
        of = p_stats.cdd(fn_hist_prmm, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'cdd' in var_stat and scens != 'historical':
        of = p_stats.cdd(fn_rcp_prmm, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # Number of wet days > 0.2 mm/d
    if 'r02' in var_stat and scens == 'historical':
        of = p_stats.r02(fn_hist_prmm, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'r02' in var_stat and scens != 'historical':
        of = p_stats.r02(fn_rcp_prmm, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # Max consec 5 day precip
    if 'r5d' in var_stat and scens == 'historical':
        of = p_stats.r5d(fn_hist_prmm, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'r5d' in var_stat and scens != 'historical':
        of = p_stats.r5d(fn_rcp_prmm, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # simple daily precip intensity index
    if 'sdii' in var_stat and scens == 'historical':
        of = p_stats.sdii(fn_hist_prmm, StComHis, EnComHis, model)
        print "created outfile %s\n" % of
    if 'sdii' in var_stat and scens != 'historical':
        of = p_stats.sdii(fn_rcp_prmm, StYrsFut, EnYrsFut, model)
        print "created outfile %s\n" % of

    # Pct of time precip exceeds ref pd 90th percentile (wet day values)
    # calculate % of precip due to this too
    if 'r90p' in var_stat:
        ofpr90_ref = p_stats.R90ref(fn_hist_pr, StRefHis, EnRefHis)
        print "created outfile %s\n" % ofpr90_ref
        of = p_stats.R90P(fn_hist_pr, ofpr90_ref, StComHis, EnComHis)
        print "created outfile %s\n" % of
        of = p_stats.R90P(fn_rcp_pr, ofpr90_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of
        of = p_stats.R90PTOT(fn_hist_pr, ofpr90_ref, StComHis, EnComHis)
        print "created outfile %s\n" % of
        of = p_stats.R90PTOT(fn_rcp_pr, ofpr90_ref, StYrsFut, EnYrsFut)
        print "created outfile %s\n" % of

print '\n\n... done!\n'
