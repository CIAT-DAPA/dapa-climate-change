!     Fortran90 program file for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2008.
!     Copyright The University  of Reading and the University of Leeds 2008. All rights reserved.

PROGRAM DRIVER
  ! Driver for General Large-Area Model for annual crops
  ! This code is generic to all crop types     
  USE IO_VARS 
  USE MULTI_VARS
  USE GLOBAL_VARS
  IMPLICIT NONE
  CHARACTER*2 :: DUMMYCHAR !KJN
  INTEGER :: IHDATE,IDAP,IHYP
  REAL    :: ASWS,E_DEPTH 
  LOGICAL :: POT
  INTEGER NCOMLIN

  NPLANT=1 !Number of plantings wanted each year

! Modified by Steven Pickering, 11/6/2010

! Store the number of command line parameters
  
  NCOMLIN = IARGC()

! Check the number of command line parameters

  IF (NCOMLIN == 1) THEN

! Store the name of the experiment file

    CALL GETARG(1, FNAMESFILE)
 
  ELSE

! Display the usage message and exit

    PRINT *,"Usage: glam ExperimentFileName"
    PRINT *,""
    PRINT *,"The ExperimentFileName stores the locations of the input data files and parameters"

! Exit the program
    STOP

  ENDIF

! End Modifications

  CALL INIT_GLAM(POT)
  CALL READ_SPAT
  CALL INIT_MULTI
 
  ALLOCATE(SAVEMERIT(5,NHYP))
  DO IHYP=1,NHYP
     IF (MODE=='HYP') THEN
        LNEWSET=.TRUE.         
        WRITE(IINFFIL,*)
        WRITE(IINFFIL,*)'Starting HYP run no ', IHYP
     ENDIF
     DO ILAT=1,NLAT
        DO ILON=1,NLON
           IF (LOW_AVG_AREA(ILAT,ILON) .NE.0) JSKIP(ILAT,ILON) = 1 !Kathryn Nicklin added to skip low area gridcells
           IF (JSKIP(ILAT,ILON)==1) GOTO 10
           DO IYR=ISYR,IEYR
            IF(OBS_YLD(ILAT,ILON,IYR-ISYR+1).GE.0) THEN !Kathryn Nicklin - added so that GLAM isn't run for gridcells with no yield data.
              WRITE(IINFFIL,*) 'Running Lat, Lon, Year numbers: ',ILAT,ILON,IYR
              DO IPLANT=1,NPLANT !KJN added loop so that multiple plants per year can be done.
               LREPLANT=.FALSE. !Kathryn Nicklin. First run through - crop not being replanted.
               CALL INIT_RUN(ASWS,IHDATE,E_DEPTH,IHYP) 
               CALL GLAM(POT,ASWS,IHDATE,E_DEPTH,IDAP,IHYP,IPLANT) !Kathryn Nicklin added IPLANT to the variables pass to GLAM
               IF (IPLANT.LT.10) WRITE(DUMMYCHAR,'(I1)')IPLANT
               IF ((IPLANT.GE.10).AND.(IPLANT.LT.100)) WRITE(DUMMYCHAR,'(I2)')IPLANT
!              Kathryn Nicklin changed the following line to IASCII.GE.2 rather than IASCII>2 Dec 2009.
               IF (IASCII.GE.2) CALL SYSTEM('mv -f glam.out '//TRIM(OUTDIR)//'daily/'//TRIM(CROP)//'_'// &
                    TRIM(FLWTH(ILAT,ILON,IYR-ISYR+1)(18:35))//'_'//TRIM(DUMMYCHAR)//'.out')!Rename daily output file Kathryn Nicklin changed the indices of
                    !what is used from the FLWTH character string from 18:35 to 43:57 to do the ghana runs
               CALL STORE_VARS(IDAP,POT)
               BBYLD(IYR-ISYR+1,ILAT,ILON,IHYP,IPLANT)=YIELD !KJN added IPLANT dimension to store multiple planting dates
              ENDDO
            ENDIF
           ENDDO
10         CONTINUE
        ENDDO
     ENDDO
  ENDDO
  
  WRITE(IINFFIL,*)
  IF (MODE=='HYP') THEN
     CALL MERIT
     WRITE(IINFFIL,*)'Successful completion of HYP runs'
  ELSEIF (MODE=='SET') THEN
     IF (IASCII>0) CALL OUTPUT_AS
     IF (INETCDF>0) CALL OUTPUT_NC(POT)
     WRITE(IINFFIL,*)'Successful completion of SET runs'
  ENDIF
 CLOSE(IINFFIL)

  
END PROGRAM DRIVER



