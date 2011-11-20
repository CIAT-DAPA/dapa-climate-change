#!/usr/bin/env python

# Tests the effect of introducing increased error probability to observations.
# This study uses the Gujarat GLAM configuration, which as of the time of writing
# has not been integrated into the svn trunk.
#
# The design of this program is that runs are performed by __main__ when the
# required .pickle.bz2 files don't exist.
#
# The location and description of each of the simulated datasets are below, where
# P is the relevant percent standard deviation and S is the respective random number seed.
# Note that paths must be < 50 characters.
#   dqs_data/prec/day/p-P_s-S.dat        (daily uncorrelated precip)
#   dqs_data/prec/season/p-P_s-S.dat     (seasonally biased precip)
#   dqs_data/prec/climate/p-P_s-S.dat    (climatologically biased precip)
#   dqs_data/yield/season/p-P_s-S.dat    (seasonally biased yield)
#   dqs_data/yield/climate/p-P_s-S.dat   (climatologically biased yield)
#   dqs_data/mtemp/season/p-P_s-S.dat    (seasonally biased mean temp)
#   dqs_data/mtemp/climate/p-P_s-S.dat   (climatologically biased mean temp)
#
# The original scheme was to base the standard deviation on the percentage of the
# value being perturbed.  For example, for a daily input value v, an uncorrelated daily
# perturbation would be performed by choosing a value from the normal distribution
# with a mean of v and a standard deviation of P% of v.  For a seasonal/climatic
# perturbation, all values would be altered by a diff value based on the normal
# distribution with a mean of seasonal/climatic mean and a standard deviation of
# P% of that mean.
#
# To make comparisons between variables more meaningful, the new scheme is to use
# normal distributions with means as above, but standard deviations chosen from P% of
# the standard deviation of all inputs (for daily and climatological perturbations),
# or of the relevant seasonal values (for seasonal perturbations).


from os import system, getcwd, chdir, remove, symlink, mkdir
from os.path import islink, isfile, basename
from netCDF4 import Dataset
from numpy import loadtxt, zeros, array, append, corrcoef
from math import sqrt
from random import seed, randint, Random
from pylab import *
from bz2 import BZ2File
import cPickle
from sys import argv, stdout, stderr, exit
from glob import glob


results_loc = 'data_quality_study_results'
model_loc = 'src/glam-gujarat-config'

obs_yield_data = loadtxt(model_loc + '/inputs_gujarat/yield/obsyield.txt')
obs_yield = obs_yield_data[:,3]

obs_rain_file = open(model_loc + '/inputs_gujarat/weather/original/rainfall.dat')
obs_rain_data = obs_rain_file.readlines()
obs_rain_file.close()

obs_rain_layout = {}  # year : (start_line, stop_line)
l = 1
for year in range(1966, 1990):
    obs_rain_layout[year] = (l, l+7)
    l += 9

obs_min_temp_first_column = loadtxt(model_loc + '/inputs_gujarat/weather/original/mintemp.dat', dtype=str, usecols=(0,))
obs_min_temp_data = loadtxt(model_loc + '/inputs_gujarat/weather/original/mintemp.dat', dtype=int, usecols=range(1,13))

obs_max_temp_first_column = loadtxt(model_loc + '/inputs_gujarat/weather/original/maxtemp.dat', dtype=str, usecols=(0,))
obs_max_temp_data = loadtxt(model_loc + '/inputs_gujarat/weather/original/maxtemp.dat', dtype=int, usecols=range(1,13))

obs_temp_layout = {}  # year : line
l = 0
for year in range(1966, 1990):
    obs_temp_layout[year] = l
    l += 1

obs_rad_file = open(model_loc + '/inputs_gujarat/weather/original/radiation.dat')
obs_rad_data = obs_rad_file.readlines()
obs_rad_file.close()

filenames_file = open(model_loc + '/filenames-gnut-cal-guj.txt')
filenames_data = filenames_file.readlines()
filenames_file.close()

num_std_dev_datasets = 100
percent_std_dev = range(300)  # (0, 1, 2, 4, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100)

seed(12)  # used to generate lists of random seeds
obs_seeds_yield =    [randint(1,9999) for count in xrange(num_std_dev_datasets)]  # generate a seed for each std_dev dataset
obs_seeds_rainfall = [randint(1,9999) for count in xrange(num_std_dev_datasets)]
obs_seeds_min_temp = [randint(1,9999) for count in xrange(num_std_dev_datasets)]
obs_seeds_max_temp = [randint(1,9999) for count in xrange(num_std_dev_datasets)]
obs_seeds_rad      = [randint(1,9999) for count in xrange(num_std_dev_datasets)]
obs_seeds_all_temp = [randint(1,9999) for count in xrange(num_std_dev_datasets)]
dqs_seeds          = [randint(1,9999) for count in xrange(num_std_dev_datasets)]

dqs_data_config = {'prec' :  ('day', 'season', 'climate'),
                   'yield' : ('season', 'climate'),
                   'mtemp' : ('season', 'climate')}


def get_rain_values(r_filename):
    'Returns the values in the specified rainfall data file as a flattened array'

    r_file = open(r_filename)
    r_data = r_file.readlines()
    r_file.close()

    r_values = []
    for row in r_data:
        if len(row) in (51, 81):
            index = 0
            while index < (len(row)-1):
                r_values.append(float(row[index:(index+5)]))
                index += 5

    return array(r_values)


def get_rain_year_values(year):
    'Returns the rainfall values for the given year as a 1D array'
    
    if year not in obs_rain_layout.keys():
        return array([])
        
    val_rows = obs_rain_data[obs_rain_layout[year][0]:(obs_rain_layout[year][1]+1)]
    vals = []
    for row in val_rows:
        index = 0
        while index < (len(row)-1):  # there is a trailing '\n'
            vals.append(float(row[index:(index+5)]))
            index += 5

    return array(vals)


def get_rain_year(line_num):
    'Returns the year the given line number is part of.'
    
    for year, line_nums in obs_rain_layout.iteritems():
        if (line_num >= line_nums[0]) and (line_num <= line_nums[1]):
            return year
    
    return None


def create_dqs_data():
    '''Generates the set of input data files for this study.  This method
       supercedes the other create_* methods below, which are retained for
       reference.'''

    # calculate seasonal and climatological means
    obs_rainfall_vals = get_rain_values(model_loc + '/inputs_gujarat/weather/original/rainfall.dat')
    obs_rainfall_climate_mean = obs_rainfall_vals.mean()
    obs_rainfall_climate_std = obs_rainfall_vals.std()
    obs_yield_climate_mean = obs_yield.mean()
    obs_yield_climate_std = obs_yield.std()
    obs_temp_season_mean = []  # line_num : seasonal_mean
    obs_temp_season_std = []   # line_num : seasonal_std
    obs_temp_avg_svals = array([])  # array of all temp values from the season we are interested in
    for i in range(obs_min_temp_data.shape[0]):  # tmin and tmax files have same structure
        tmin_svals = obs_min_temp_data[i,5:9]  # June -> September (inclusive)
        tmax_svals = obs_max_temp_data[i,5:9]
        tavg_svals = (tmin_svals + tmax_svals) / 2.0
        obs_temp_season_mean.append(tavg_svals.mean())
        obs_temp_season_std.append(tavg_svals.std())
        obs_temp_avg_svals = append(obs_temp_avg_svals, tavg_svals)
    obs_temp_climate_mean = obs_temp_avg_svals.mean()
    obs_temp_climate_std = obs_temp_avg_svals.std()

    # create directories
    mkdir(model_loc + '/dqs_data')
    for var_name, bias_type in dqs_data_config.iteritems():
        mkdir(model_loc + '/dqs_data/' + var_name)
        for b in bias_type:
            mkdir(model_loc + '/dqs_data/' + var_name + '/' + b)

    for s in dqs_seeds:
        r = Random(s)
        for p in percent_std_dev:
            # -- daily uncorrelated precip --
            out_file = open(model_loc + '/dqs_data/prec/day/p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            for row in obs_rain_data:
                if len(row) in (51, 81):  # there are 10 or 16 {3.1}f values
                    new_row = ''
                    index = 0
                    while index < (len(row)-1):  # there is a trailing '\n'
                        orig_value = float(row[index:(index+5)])
                        out_std_dev = p / 100.0 * obs_rainfall_climate_std  # was: * orig_value
                        new_value = r.normalvariate(orig_value, out_std_dev)
                        if new_value < 0:
                            new_value = 0.0
                        elif new_value >= 1000:  # column format is {3.1}f
                            new_value = 999.9
                        new_row += '{0:5.1f}'.format(new_value)
                        index += 5
                    new_row += '\n'
                    out_file.write(new_row)
                else:
                    out_file.write(row)
            out_file.close()
            
            # -- seasonally biased precip --
            out_file = open(model_loc + '/dqs_data/prec/season/p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            for rain_year in range(1966,1990):
                obs_rainfall_year_mean = get_rain_year_values(rain_year).mean()
                obs_rainfall_year_std = get_rain_year_values(rain_year).std()
                out_std_dev = p / 100.0 * obs_rainfall_year_std  # was: * obs_rainfall_year_mean
                diff_val = r.normalvariate(obs_rainfall_year_mean, out_std_dev) - obs_rainfall_year_mean
                out_file.write(' BLOCK-NO 25 DAILY RF OF  ' + str(rain_year) + '\n')
                for row in obs_rain_data[obs_rain_layout[rain_year][0]:(obs_rain_layout[rain_year][1]+1)]:
                    new_row = ''
                    index = 0
                    while index < (len(row)-1):  # there is a trailing '\n'
                        orig_value = float(row[index:(index+5)])
                        new_value = orig_value + diff_val
                        if new_value < 0:
                            new_value = 0.0
                        elif new_value >= 1000.0:  # column format is {3.1}f
                            new_value = 999.9
                        new_row += '{0:5.1f}'.format(new_value)
                        index += 5
                    new_row += '\n'
                    out_file.write(new_row)
            out_file.write('\n\n')
            out_file.close()
            
            # -- climatologically biased precip --
            out_file = open(model_loc + '/dqs_data/prec/climate/p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            out_std_dev = p / 100.0 * obs_rainfall_climate_std  # was: obs_rainfall_climate_mean
            diff_val = r.normalvariate(obs_rainfall_climate_mean, out_std_dev) - obs_rainfall_climate_mean
            for row in obs_rain_data:
                if len(row) in (51, 81):  # there are 10 or 16 {3.1}f values
                    new_row = ''
                    index = 0
                    while index < (len(row)-1):  # there is a trailing '\n'
                        orig_value = float(row[index:(index+5)])
                        new_value = orig_value + diff_val
                        if new_value < 0:
                            new_value = 0.0
                        elif new_value >= 1000.0:  # column format is {3.1}f
                            new_value = 999.9
                        new_row += '{0:5.1f}'.format(new_value)
                        index += 5
                    new_row += '\n'
                    out_file.write(new_row)
                else:
                    out_file.write(row)
            out_file.close()
            
            # -- seasonally biased yield --
            out_file = open(model_loc + '/dqs_data/yield/season/p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            for row in obs_yield_data:
                out_mean = row[3]
                out_std_dev = p / 100.0 * obs_yield_climate_std  # was: out_mean
                out_yield = r.normalvariate(out_mean, out_std_dev)
                if out_yield < 0:
                    out_yield = 0.0
                out_file.write(str(int(row[0])).ljust(7) + str(int(row[1])).ljust(4) + str(int(row[2])) + '{0:.5f}'.format(out_yield).rjust(13) + '\n')
            out_file.close()
            
            # -- climatologically biased yield --
            out_file = open(model_loc + '/dqs_data/yield/climate/p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            out_std_dev = p / 100.0 * obs_yield_climate_std  # was: obs_yield_climate_mean
            diff_val = r.normalvariate(obs_yield_climate_mean, out_std_dev) - obs_yield_climate_mean
            for row in obs_yield_data:
                out_yield = row[3] + diff_val
                if out_yield < 0:
                    out_yield = 0.0
                out_file.write(str(int(row[0])).ljust(7) + str(int(row[1])).ljust(4) + str(int(row[2])) + '{0:.5f}'.format(out_yield).rjust(13) + '\n')
            out_file.close()
            
            # -- seasonally biased mean temp --
            tmin_out_file = open(model_loc + '/dqs_data/mtemp/season/tmin_p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            tmax_out_file = open(model_loc + '/dqs_data/mtemp/season/tmax_p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            for i in range(obs_min_temp_data.shape[0]):  # tmin and tmax files have same structure
                out_std_dev = p / 100.0 * obs_temp_season_std[i]  # was: obs_temp_season_mean[i]
                diff_val = int(r.normalvariate(obs_temp_season_mean[i], out_std_dev) - obs_temp_season_mean[i])
                new_tmin_row = obs_min_temp_first_column[i]
                new_tmax_row = obs_max_temp_first_column[i]
                for j in range(obs_min_temp_data.shape[1]):
                    new_tmin_val = obs_min_temp_data[i,j] + diff_val
                    if new_tmin_val < 0:
                        new_tmin_val = 0
                    elif new_tmin_val >= 1000:  # data format is (2XI3) in input.f90
                        new_tmin_val = 999

                    new_tmax_val = obs_max_temp_data[i,j] + diff_val
                    if new_tmax_val < 0:
                        new_tmax_val = 0
                    elif new_tmax_val >= 1000:
                        new_tmax_val = 999

                    new_tmin_row += '{0:5d}'.format(new_tmin_val)
                    new_tmax_row += '{0:5d}'.format(new_tmax_val)

                new_tmin_row += '\n'
                new_tmax_row += '\n'
                tmin_out_file.write(new_tmin_row)
                tmax_out_file.write(new_tmax_row)
            
            tmin_out_file.write('\n')
            tmax_out_file.write('\n')
            tmin_out_file.close()
            tmax_out_file.close()
            
            # -- climatologically biased mean temp --
            tmin_out_file = open(model_loc + '/dqs_data/mtemp/climate/tmin_p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            tmax_out_file = open(model_loc + '/dqs_data/mtemp/climate/tmax_p-' + str(p) + '_s-' + str(s) + '.dat', 'w')
            out_std_dev = p / 100.0 * obs_temp_climate_std  # was: obs_temp_climate_mean
            diff_val = int(r.normalvariate(obs_temp_climate_mean, out_std_dev) - obs_temp_climate_mean)
            for i in range(obs_min_temp_data.shape[0]):  # tmin and tmax files have same structure
                new_tmin_row = obs_min_temp_first_column[i]
                new_tmax_row = obs_max_temp_first_column[i]
                for j in range(obs_min_temp_data.shape[1]):
                    new_tmin_val = obs_min_temp_data[i,j] + diff_val
                    if new_tmin_val < 0:
                        new_tmin_val = 0
                    elif new_tmin_val >= 1000:  # data format is (2XI3) in input.f90
                        new_tmin_val = 999

                    new_tmax_val = obs_max_temp_data[i,j] + diff_val
                    if new_tmax_val < 0:
                        new_tmax_val = 0
                    elif new_tmax_val >= 1000:
                        new_tmax_val = 999

                    new_tmin_row += '{0:5d}'.format(new_tmin_val)
                    new_tmax_row += '{0:5d}'.format(new_tmax_val)

                new_tmin_row += '\n'
                new_tmax_row += '\n'
                tmin_out_file.write(new_tmin_row)
                tmax_out_file.write(new_tmax_row)

            tmin_out_file.write('\n')
            tmax_out_file.write('\n')
            tmin_out_file.close()
            tmax_out_file.close()



# ---- Julian: code below from shuffle module ----


#!/usr/bin/env python

# Jim Watson
#
# This study is an extension of data_quality_study.py, where the precip
# values are shuffled with 3 schemes:
#    - shuffle_years        (mixes years, preserves intra-seasonal variability)
#    - shuffle_within_year  (mixes within each year)
#    - shuffle_all          (mixes all values without considering year boundaries)
#
# Yield inputs are shuffled, but since these are yearly values, this is the
# equivalent of precip's shuffle_years.
#
# Temperature inputs are shuffled with the same schemes as for precip, but only
# the relevant seasonal values are considered, and note these are monthly values.

num_seeds = 1000  # number of times the simulations are repeated with a different random seed
p_config = ('A', 'B', 'C')  # GLAM configurations used
shuffled_data_config = {'sprec'  : ('years', 'wyear', 'all'),   # var_type : shuffle_schemes
                        'syield' : ('years',),
                        'stemp'  : ('years', 'wyear', 'all')}
results_loc = 'shuffled_data_quality_study_results'

from data_quality_study import get_rain_year_values, model_loc, get_ygp_yield, get_rmse, obs_yield, obs_yield_data, obs_min_temp_first_column, obs_min_temp_data, obs_max_temp_first_column, obs_max_temp_data
from random import seed, randint, Random
from os import mkdir
from numpy import array, hsplit, hstack
from os.path import isfile, basename
from sys import stdout
from bz2 import BZ2File
import cPickle
from glob import glob
from pylab import *

# create list of unique random number seeds
seed(21)
shuffle_seeds = {}
for v in shuffled_data_config.keys():
    shuffle_seeds[v] = []
    while len(shuffle_seeds[v]) < num_seeds:
        s = randint(1,9999)
        if s not in shuffle_seeds[v]:
            shuffle_seeds[v].append(s)


# load original observations
prec_obs = {}  # year : daily values
for year in range(1966,1990):
    prec_obs[year] = get_rain_year_values(year)


def save_prec_data(prec_data, prec_filename):
    '''Saves the data in the dictionary prec_data to the file with the
       given filename.

       prec_data should be of the form {year : array_of_daily_values}.'''

    d_file = open(prec_filename, 'w')
    for year in sorted(prec_data.keys()):
        assert (prec_data[year].shape == (122,))
        d_file.write(' BLOCK-NO 25 DAILY RF OF  ' + str(year) + '\n')
        i = 0
        for line in range(7):
            for val in range(16):
                d_file.write('{0:5.1f}'.format(prec_data[year][i])); i += 1
            d_file.write('\n')
        for val in range(10):
            d_file.write('{0:5.1f}'.format(prec_data[year][i])); i += 1
        d_file.write('\n')
    d_file.write('\n\n')
    d_file.close()
    

def create_shuffled_yield_data():
    'Generates the shuffled yield data.'
    
    # create directories
    mkdir(model_loc + '/dqs_data_shuffled/yield')
    mkdir(model_loc + '/dqs_data_shuffled/yield/years')

    for s in shuffle_seeds['syield']:
        r = Random(s)
        new_yield = list(obs_yield)
        r.shuffle(new_yield)
        
        # save new yield file
        out_file = open(model_loc + '/dqs_data_shuffled/yield/years/s-' + str(s) + '.dat', 'w')
        for i in range(obs_yield_data.shape[0]):
            out_file.write(str(int(obs_yield_data[i,0])).ljust(7) + str(int(obs_yield_data[i,1])).ljust(4) + str(int(obs_yield_data[i,2])) + '{0:.5f}'.format(new_yield[i]).rjust(13) + '\n')
        out_file.close()


def create_shuffled_temp_data():
    'Generates the shuffled minimum and maximum temperature data.'
    
    # create directories
    mkdir(model_loc + '/dqs_data_shuffled/temp')
    mkdir(model_loc + '/dqs_data_shuffled/temp/years')
    mkdir(model_loc + '/dqs_data_shuffled/temp/wyear')
    mkdir(model_loc + '/dqs_data_shuffled/temp/all')
    
    for s in shuffle_seeds['stemp']:
        r = Random(s)
        
        # shuffle years
        tmin_out_file = open(model_loc + '/dqs_data_shuffled/temp/years/tmin_s-' + str(s) + '.dat', 'w')
        tmax_out_file = open(model_loc + '/dqs_data_shuffled/temp/years/tmax_s-' + str(s) + '.dat', 'w')
        shuffled_index = range(obs_min_temp_data.shape[0])  # tmin and tmax input files have the same structure
        r.shuffle(shuffled_index)
        for i in range(obs_min_temp_data.shape[0]):
            new_tmin_row = obs_min_temp_first_column[i]
            new_tmax_row = obs_max_temp_first_column[i]
            for j in range(obs_min_temp_data.shape[1]):
                new_tmin_val = obs_min_temp_data[shuffled_index[i],j]
                new_tmax_val = obs_max_temp_data[shuffled_index[i],j]
                new_tmin_row += '{0:5d}'.format(new_tmin_val)
                new_tmax_row += '{0:5d}'.format(new_tmax_val)
            new_tmin_row += '\n'
            new_tmax_row += '\n'
            tmin_out_file.write(new_tmin_row)
            tmax_out_file.write(new_tmax_row)
        tmin_out_file.write('\n')
        tmax_out_file.write('\n')
        tmin_out_file.close()
        tmax_out_file.close()

        # shuffle within years
        tmin_out_file = open(model_loc + '/dqs_data_shuffled/temp/wyear/tmin_s-' + str(s) + '.dat', 'w')
        tmax_out_file = open(model_loc + '/dqs_data_shuffled/temp/wyear/tmax_s-' + str(s) + '.dat', 'w')
        shuffled_index = [5,6,7,8]  # only shuffling June -> September (inclusive)
        r.shuffle(shuffled_index)
        for i in range(4, -1, -1):
            shuffled_index.insert(0, i)
        shuffled_index.extend([9,10,11])
        for i in range(obs_min_temp_data.shape[0]):
            new_tmin_row = obs_min_temp_first_column[i]
            new_tmax_row = obs_max_temp_first_column[i]
            for j in range(obs_min_temp_data.shape[1]):
                new_tmin_val = obs_min_temp_data[i,shuffled_index[j]]
                new_tmax_val = obs_max_temp_data[i,shuffled_index[j]]
                new_tmin_row += '{0:5d}'.format(new_tmin_val)
                new_tmax_row += '{0:5d}'.format(new_tmax_val)
            new_tmin_row += '\n'
            new_tmax_row += '\n'
            tmin_out_file.write(new_tmin_row)
            tmax_out_file.write(new_tmax_row)
        tmin_out_file.write('\n')
        tmax_out_file.write('\n')
        tmin_out_file.close()
        tmax_out_file.close()
        
        # shuffle all
        tmin_out_file = open(model_loc + '/dqs_data_shuffled/temp/all/tmin_s-' + str(s) + '.dat', 'w')
        tmax_out_file = open(model_loc + '/dqs_data_shuffled/temp/all/tmax_s-' + str(s) + '.dat', 'w')

        # create an index where seasonal values are shuffled (only shuffling June -> September (inclusive))
        t_index = array(range(obs_min_temp_data.shape[0] * obs_min_temp_data.shape[1])).reshape(obs_min_temp_data.shape)
        t_index_start, t_index_to_shuffle, t_index_end = hsplit(t_index, array([5,9]))
        t_index_shuffle_vals = list(t_index_to_shuffle.flatten())
        r.shuffle(t_index_shuffle_vals)
        t_index_shuffled = array(t_index_shuffle_vals).reshape(t_index_to_shuffle.shape)
        t_index = hstack((t_index_start, t_index_shuffled, t_index_end)).flatten()

        obs_min_temp_flat = obs_min_temp_data.flatten()
        obs_max_temp_flat = obs_max_temp_data.flatten()
        new_min_temp = []
        new_max_temp = []
        for i in range(obs_min_temp_flat.shape[0]):
            new_min_temp.append(obs_min_temp_flat[t_index[i]])
            new_max_temp.append(obs_max_temp_flat[t_index[i]])
        new_min_temp = array(new_min_temp).reshape(obs_min_temp_data.shape)
        new_max_temp = array(new_max_temp).reshape(obs_max_temp_data.shape)

        for i in range(obs_min_temp_data.shape[0]):
            new_tmin_row = obs_min_temp_first_column[i]
            new_tmax_row = obs_max_temp_first_column[i]
            for j in range(obs_min_temp_data.shape[1]):
                new_tmin_val = new_min_temp[i,j]
                new_tmax_val = new_max_temp[i,j]
                new_tmin_row += '{0:5d}'.format(new_tmin_val)
                new_tmax_row += '{0:5d}'.format(new_tmax_val)
            new_tmin_row += '\n'
            new_tmax_row += '\n'
            tmin_out_file.write(new_tmin_row)
            tmax_out_file.write(new_tmax_row)
        tmin_out_file.write('\n')
        tmax_out_file.write('\n')
        tmin_out_file.close()
        tmax_out_file.close()


def create_shuffled_prec_data():
    'Generates the shuffled precip data.'

    # create directories
    mkdir(model_loc + '/dqs_data_shuffled/prec')
    mkdir(model_loc + '/dqs_data_shuffled/prec/years')
    mkdir(model_loc + '/dqs_data_shuffled/prec/wyear')
    mkdir(model_loc + '/dqs_data_shuffled/prec/all')

    years, year_data = zip(*prec_obs.items())
    for s in shuffle_seeds['sprec']:
        r = Random(s)

        # shuffle years
        new_years = list(years)
        r.shuffle(new_years)
        new_prec = {}
        for i in range(len(new_years)):
            new_prec[new_years[i]] = year_data[i]
        save_prec_data(new_prec, (model_loc + '/dqs_data_shuffled/prec/years/s-' + str(s) + '.dat'))

        # shuffle within years
        new_prec = {}
        for year, data in prec_obs.iteritems():
            new_data_list = list(data)
            r.shuffle(new_data_list)
            new_prec[year] = array(new_data_list)
        save_prec_data(new_prec, (model_loc + '/dqs_data_shuffled/prec/wyear/s-' + str(s) + '.dat'))

        # shuffle all
        new_data_list = []
        for data in year_data:
            new_data_list.extend(list(data))
        r.shuffle(new_data_list)
        new_prec = {}
        ycount = 0
        for year in years:
            new_prec[year] = array(new_data_list[(ycount*122):((ycount+1)*122)])
            ycount += 1
        save_prec_data(new_prec, (model_loc + '/dqs_data_shuffled/prec/all/s-' + str(s) + '.dat'))
