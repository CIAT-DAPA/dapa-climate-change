# Makefile to compile and run GLAM.
#
# Note: edit the system configuration section to suit your system.
#
# Usage:
#   compile all crop models:    make
#   run the GLAM test suite:    make test
#   clean intermediate files:   make clean
#
# Individual GLAM models can be compiled and run, e.g.:
#	make glam-gnut
#	./glam-gnut experiment-file.txt
#
# See the rules under 'main programs' for available models.


# -- system configuration --
# F90=ifort -traceback
# F90=ifort -O3 -traceback
# F90=ifort -g -warn -check -traceback #Max warnings
F90=ifort -g -warn errors -warn noalignments -check bounds -check format -check output_conversion -traceback
NETCDF_LOC = /apps/netcdf-3.6.1-ifc/lib
NAG_LOC = /apps/nag
# --------------------------

all: glam-gnut

test: test-gnut-cal test-gnut-run test-gnut-hyp test-wspr test-wwin test-maiz


# main programs
OBJS = common_blocks.o driver.o metcalcs.o watbal.o input.o output.o opt.o

glam-gnut: groundnut.o $(OBJS)
	$(F90) groundnut.o $(OBJS) -o glam-gnut -L$(NETCDF_LOC) -lnetcdf -L$(NAG_LOC) -lnag

glam-wspr: wheatspring.o $(OBJS)
	$(F90) wheatspring.o $(OBJS) -o glam-wspr -L$(NETCDF_LOC) -lnetcdf

glam-wwin: wheatwinter.o $(OBJS)
	$(F90) wheatwinter.o $(OBJS) -o glam-wwin -L$(NETCDF_LOC) -lnetcdf

glam-maiz: maize.o $(OBJS)
	$(F90) maize.o $(OBJS) -o glam-maiz -L$(NETCDF_LOC) -lnetcdf -L$(NAG_LOC) -lnag


# test sets
test-gnut-cal: glam-gnut
	@./glam-gnut filenames-gnut-cal.txt
	@diff glam.inf test_output/inf_files/glam_testgnutcal.inf
	@diff output/groundnut_YGP1993-1999-6_loc.mer test_output/groundnut_YGP1993-1999-6_loc.mer
	@echo glam-gnut filenames-gnut-cal.txt -\> PASSED

test-gnut-run: glam-gnut
	@./glam-gnut filenames-gnut-cal.txt
	@./glam-gnut filenames-gnut-run.txt
	@diff glam.inf test_output/inf_files/glam_testgnutrun.inf
	@diff output/groundnut.out test_output/groundnut.out
	@diff output/daily/groundnut_egeg0010011994.out test_output/daily/groundnut_egeg0010011994.out
	@diff output/daily/groundnut_egeg0010011995.out test_output/daily/groundnut_egeg0010011995.out
	@diff output/daily/groundnut_egeg0010021994.out test_output/daily/groundnut_egeg0010021994.out
	@diff output/daily/groundnut_egeg0010021995.out test_output/daily/groundnut_egeg0010021995.out
	@diff output/daily/groundnut_egeg0020011994.out test_output/daily/groundnut_egeg0020011994.out
	@diff output/daily/groundnut_egeg0020011995.out test_output/daily/groundnut_egeg0020011995.out
	@diff output/daily/groundnut_egeg0020021994.out test_output/daily/groundnut_egeg0020021994.out
	@diff output/daily/groundnut_egeg0020021995.out test_output/daily/groundnut_egeg0020021995.out
	@echo glam-gnut filenames-gnut-run.txt -\> PASSED

test-gnut-hyp: glam-gnut
	@./glam-gnut filenames-gnut-cal.txt
	@./glam-gnut filenames-gnut-hyp.txt
	@diff glam.inf test_output/inf_files/glam_testgnuthyp.inf
	@diff output/groundnut_DHDT1993-1999_glo.mer test_output/groundnut_DHDT1993-1999_glo.mer
	@echo glam-gnut filenames-gnut-hyp.txt -\> PASSED

test-wspr: glam-wspr
	@./glam-wspr filenames-wspr.txt > /dev/null
	@diff output/wheatspr.out test_output/wheatspr.out
	@diff output/daily/wheatspr_egeg0010011993.out test_output/daily/wheatspr_egeg0010011993.out 
	@diff output/daily/wheatspr_egeg0010021993.out test_output/daily/wheatspr_egeg0010021993.out
	@diff output/daily/wheatspr_egeg0020011993.out test_output/daily/wheatspr_egeg0020011993.out
	@diff output/daily/wheatspr_egeg0020021993.out test_output/daily/wheatspr_egeg0020021993.out
	@echo glam-wspr filenames-wspr.txt -\> PASSED

test-wwin: glam-wwin
	@./glam-wwin filenames-wwin.txt
	@diff glam.inf test_output/inf_files/glam_testwwin.inf
	@diff output/wheatwinter.out test_output/wheatwinter.out
	@diff output/daily/wheatwinter_HNZZ0010011997.out test_output/daily/wheatwinter_HNZZ0010011997.out
	@diff output/daily/wheatwinter_HNZZ0010021997.out test_output/daily/wheatwinter_HNZZ0010021997.out
	@diff output/daily/wheatwinter_HNZZ0020011997.out test_output/daily/wheatwinter_HNZZ0020011997.out
	@diff output/daily/wheatwinter_HNZZ0020021997.out test_output/daily/wheatwinter_HNZZ0020021997.out
	@echo glam-wwin filenames-wwin.txt -\> PASSED

test-maiz: glam-maiz
	@./glam-maiz filenames-maiz.txt
	@diff glam.inf test_output/inf_files/glam_testmaiz.inf
	@diff output/maize.out test_output/maize.out
	@diff output/daily/maize_egeg0010011994.out test_output/daily/maize_egeg0010011994.out
	@diff output/daily/maize_egeg0010021994.out test_output/daily/maize_egeg0010021994.out
	@diff output/daily/maize_egeg0020011994.out test_output/daily/maize_egeg0020011994.out
	@diff output/daily/maize_egeg0020021994.out test_output/daily/maize_egeg0020021994.out
	@echo glam-maiz filenames-maiz.txt -\> PASSED


# object files
common_blocks.o: common_blocks.f90
	$(F90) -c common_blocks.f90

driver.o: driver.f90 common_blocks.o
	$(F90) -c driver.f90

metcalcs.o: metcalcs.f90 common_blocks.o
	$(F90) -c metcalcs.f90

watbal.o: watbal.f90 common_blocks.o
	$(F90) -c watbal.f90

input.o: input.f90 common_blocks.o 
	$(F90) -c input.f90

output.o: output.f90 common_blocks.o
	$(F90) -c output.f90

opt.o: opt.f90 common_blocks.o 
	$(F90) -c opt.f90

groundnut.o: groundnut.f90  common_blocks.o 
	$(F90) -c groundnut.f90

wheatspring.o: wheatspring.f90 common_blocks.o
	$(F90) -c wheatspring.f90

wheatwinter.o: wheatwinter.f90 common_blocks.o
	$(F90) -c wheatwinter.f90

maize.o: maize.f90 common_blocks.o
	$(F90) -c maize.f90



# rules to create python modules (work in progress)
export CC := gcc -m32 -pthread  # required if Python and NAG are 32-bit on your system, -pthread from python's lib/python2.6/config/Makefile

python: driver.so groundnut.so

driver.so: driver.f90 watbal.o output.o groundnut.o opt.o common_blocks.o metcalcs.o input.o
	f2py -c -m driver -L$(NETCDF_LOC) -lnetcdf -L$(NAG_LOC) -lnag driver.f90 watbal.o output.o groundnut.o opt.o common_blocks.o metcalcs.o input.o

groundnut.so: groundnut.f90 driver.o watbal.o output.o opt.o common_blocks.o metcalcs.o input.o
	f2py -c -m groundnut -L$(NETCDF_LOC) -lnetcdf -L$(NAG_LOC) -lnag groundnut.f90 driver.o watbal.o output.o opt.o common_blocks.o metcalcs.o input.o

# input.f90 needs to be changed before f2py can parse it
input.so: input.f90 driver.o watbal.o output.o groundnut.o opt.o common_blocks.o metcalcs.o
	f2py -c -m input -L$(NETCDF_LOC) -lnetcdf -L$(NAG_LOC) -lnag input.f90 driver.o watbal.o output.o groundnut.o opt.o common_blocks.o metcalcs.o


clean:
	rm -f *.o *.mod *.so glam-gnut glam-wspr glam-wwin glam-maiz
