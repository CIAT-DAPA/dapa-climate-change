#!/bin/convsh1.90

#  Convsh script conv2nc.tcl
#
#  Convert each file, into a corresponding NETCDF file.
#  File names are taken from the command arguments.
#
#      conv2nc.tcl file.pp field outfile.nc

#  Write out utf file
set outformat netcdf

#  Automatically work out input file type
set filetype 0

#  Read in each of the input files and write output file

set infile [lindex $argv 0]

# Read the field argument

set field [lindex $argv 1]

# Output filename

set outfile [lindex $argv 2]

#  Replace input file extension with .utf to get output filename
#   set outfile [file tail [file rootname $infile].nc]

#  Read input file
readfile $filetype $infile

#  Write out all input fields to a utf file
writefile $outformat $outfile $field

#  Remove input file information from Convsh's memory
clearall

