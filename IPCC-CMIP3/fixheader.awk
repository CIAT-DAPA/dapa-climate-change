# This fixes the header of an asciigrid

BEGIN {	print "ncols        96" > "tmp.asc"
				print "nrows        72" > "tmp.asc"
				print "xllcorner    0" > "tmp.asc"
				print "yllcorner    -90" > "tmp.asc"
				print "dx           3.75" > "tmp.asc"
				print "dy           2.5" > "tmp.asc"
				print "NODATA_value 9.969209968386869e+036" > "tmp.asc"
			}
			
{if (NR >= 8) {
	print $0 > "tmp.asc"
}
}