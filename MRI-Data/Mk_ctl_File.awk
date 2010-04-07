# This program uses the actual control file of a GrADS file provided by the MRI
# and creates a subsequent file inside the specified folder. This corrects the first row
# and the 200 row

# awk -f Mk_ctl_File.awk INPUT_PATH/sfc_xxx_day.ctl DATE TYPE > OUTPUT_PATH/sfc_TYPE_day.ctl

# DATE must be formatted as DDMMMYYYY
# DD from 01 to 31; MMM from JAN to DEC; YYYY from 1979 to 2099 or whatever

# TYPE refers to the type of variable (max, min, avg)

# max contains three variables (windspeed, precipitation and temperature)
# min contains one variable (temperature)
# avg contains two variables (precipitation and temperature)

BEGIN {	OFS=","
				ARGC = 2
				date = ARGV[2]
				type = ARGV[3]
				ndays = ARGV[4]
			}
			
{ 
		if (NR==1) 
		{
			print "DSET ^sfc_" type "_day.dr"
		} else {
			if (NR==200) {
				print "TDEF   " ndays "  LINEAR   00:00Z" date " 1DY"
			} else { print $0 }
		}
}
