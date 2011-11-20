!     Fortran90 subroutines for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2008.
!     Copyright The University  of Reading and the University of Leeds 2008. All rights reserved.

     SUBROUTINE READ_SPAT 
!C    Reads spatial inputs for happy model. Interpolates monthly=>daily if nec.

!C    ------------------------------------------------------------------
!C    Input     : where to look for spatial data
!C    In        : 
!C    Out       :
!C    Output    : RLL,DUL,SAT; soil types and sowing dates
!C                  SRAD, TMAX, TMIN, RAIN, TBAR, VAP, TAM
!C    Local     : 
!C    Called by : happy, sens
!C    Calls     : 
!C    ------------------------------------------------------------------
      USE GLOBAL_VARS
      USE IO_VARS
      USE MULTI_VARS
      USE HIGHT_VARS
      USE AFRICAMOD
      IMPLICIT NONE
      
      INCLUDE 'netcdf.inc'
      
      REAL, PARAMETER :: TTRIP=273.15
      INTEGER :: IYEAR,IT1,IT2,K,IOS,KSOIL,IPOFF,ICOUNT,I,J
      INTEGER :: IMON !Used to create weather input file name
      INTEGER :: IMC,IBL,IVAR,IVC,IED
      CHARACTER*30 :: STEMP
      CHARACTER*16 :: FMT
      CHARACTER*34 :: SVAR
      CHARACTER*7  :: MODL
      CHARACTER*50 :: FLNC
      INTEGER  :: IGEOIN(MAXLAT,MAXLON)
!      LOGICAL :: LUP
!      REAL :: WTHSAVE(MAXLAT,MAXLON,MAXYRS,NDAYSYR,NWTHVAR)

!     ALLOCATE(WTH(MAXLAT,MAXLON,MAXYRS,NDAYSYR,NWTHVAR))


!C    The following parameters were introduced 15Jan2009 by Kathryn Nicklin to read in different weather data

      REAL DELTAWTH, DAILYTEMPMAX(24,365),DAILYTEMPMIN(24,365)
      REAL DAILYRAD(365), MONTHRAD(8), RAINY(24,365)
      INTEGER P, YEARCHECK(24), JCHECK, MONTH,  IDEN
      INTEGER MONTHMAXTEMP(24,12),MONTHMINTEMP(24,12)
      INTEGER IDAYMID(12), NYEARS
      DATA IDAYMID /16,45,75,106,136,167,197,228,259,289,320,350/
      CHARACTER FILEPATH(4)*80

!     The following variables were introduced December 2009 by Kathryn Nicklin to read in the Africa era-interim data.
      INTEGER :: LONINDEX_REGION(NLON_AFRICA), LATINDEX_REGION(NLAT_AFRICA) !will contain the indexs of points in region of interest
      REAL :: MINLON_REGION, MAXLON_REGION, MINLAT_REGION, MAXLAT_REGION
      INTEGER :: NSMALL !NSMALL counts the number of negative rainfall values that are converted to zero
      INTEGER   :: NCID, STATUS
      INTEGER   :: IDAY
      CHARACTER(LEN=31) :: VARNAME
      CHARACTER(LEN=100) :: FL !used to store filename of netcdf file
      CHARACTER(LEN=4) :: READTEMP, YEARTEMP
      CHARACTER(LEN=3) :: DAYTEMP1
      CHARACTER(LEN=5) :: DAYTEMP2
      CHARACTER(LEN=10) :: LATTEMP, LONTEMP !temporary files used to output ascii weather data
      CHARACTER(LEN=1) :: LATTEMP1, LONTEMP1, TEMP
      CHARACTER(LEN=2) :: LATTEMP2, LONTEMP2
!      REAL :: AVG_JJATEMP(20) !introduced to calculate average June July August temperatures for all 20 years of erai data.

!C------------------------------------------------------------
!C  SET PLANTING DATE
!C------------------------------------------------------------     
 
      IF (II(KIPDATE)%VAL.EQ.NODATA) THEN
         IPDATEMAT(:,:)=NODATA
         CALL READ_LATLONINT(FLSOW,IPDATEMAT)            
      ELSE
         IPDATEMAT(:,:)=II(KIPDATE)%VAL
      ENDIF
      WHERE (IPDATEMAT(:,:)==NODATA) JSKIP(:,:)=1 
      !In place of SR TESTSKIP, sine NLON and NLAT  are not determined yet 
      IF (SUM(JSKIP(1:MAXLAT,1:MAXLON))==MAXLON*MAXLAT) THEN
         WRITE(IINFFIL,*)'All MAXLON*MAXLAT grid cells skipped - ie no IPDATE data found'
         CLOSE(IINFFIL)
         STOP
      ENDIF


!C------------------------------------------------------------
!C  READ IN WEATHER DATA AND DETERMINE NLAT, NLON
!C------------------------------------------------------------     
     
      IF (INDEX(WTHROOT,'ascii')>0) THEN
         PRINT*,'reading ascii weather'
         !ASCII input. FLWTH is (lat, long, year)
         WTH=FLOAT(NODATA)      
         DO ILAT=1,MAXLAT
            DO ILON=1,MAXLON
               DO IYR=ISYR,IEYR
                  STEMP='0000000000'
                  FMT='(A ,I ,A ,I ,I4)'
                  IT1=INT(LOG10(FLOAT(ILAT)))+1
                  IT2=INT(LOG10(FLOAT(ILON)))+1
                  WRITE(FMT(6:6),'(I1)')IT1
                  WRITE(FMT(12:12),'(I1)')IT2
                  WRITE(FMT(3:3),'(I1)')3-IT1
                  WRITE(FMT(9:9),'(I1)')3-IT2
                  WRITE(STEMP,FMT)'00',ILAT,'00',ILON,IYR
                  FLWTH(ILAT,ILON,IYR-ISYR+1)=TRIM(WTHROOT)//TRIM(STEMP)
                  OPEN(UNIT=IFLWTH,FILE=TRIM(FLWTH(ILAT,ILON,IYR-ISYR+1))//'.wth',STATUS='OLD',IOSTAT=IOS)
                  IF (IOS==0.AND.IPDATEMAT(ILAT,ILON).NE.NODATA) THEN
                     CALL READ_WTHTEXT(IYEAR,IPDATEMAT(ILAT,ILON))
                     RLATMAT(ILAT)=RLAT !C1: note that there is no check that the Lat/Lon are regual and correct
                     RLONMAT(ILON)=RLON ! These two statements simply keep the last lat/lon of index ilat/ilon
                     IF (IYEAR.NE.MOD(IYR,100)) THEN
                        WRITE(IINFFIL,*)'Warning: (ignored) mismatch in year label between param and weather files'
                     ENDIF
                     NLAT=ILAT
                     NLON=ILON
                  ELSE
                     JSKIP(ILAT,ILON)=1                    
                     EXIT
                  ENDIF                  
               ENDDO
            ENDDO
         ENDDO

         CALL TESTSKIP('ASCII weather ')

!---------------------------------------------------------------------------------------------------
! The following ELSEIF was introduced December 2009 by Kathryn Nicklin to allow
! the reading in of the ERA-INTERIM netcdf data for Africa. This is the updated routine
! that uses a module in common_blocks (AFRICAMOD) and another subroutine in input.f90 (GETVARA).

      ELSEIF(INDEX(WTHROOT,'ecmwf')>0)THEN
      WRITE(IINFFIL,*)'Reading erai netcdf files'
      PRINT *, 'Africa data - reading in started'

!     set the min and max lat and lon for the region you want to save
!     longitudes are in degrees east (use negative values for regions west of the equator)
!     latitudes in degrees north (use negative values for regions south of the equator)
!     note that in common_blocks.f90 MAXLAT and MAXLON must be big enough to accommodate this region
      
      MINLAT_REGION = 4.5
      MAXLAT_REGION = 24.5
      MINLON_REGION = -17.5
      MAXLON_REGION = 23.5

      WTH=FLOAT(NODATA)
 
!      IPDATEMAT(:,:) = 1 !need to delete this line eventually. will have already been read in

!------------------------------------------------------
!     open first year of weather file, retrieve the latitude and 
!     longitude values. find number of gridcells within the region of interest

      DO IYR = ISYR,ISYR  
          
       READTEMP='0000'

       WRITE(READTEMP,'(I4)')IYR
       FL='/nfs/see-archive-06_a17/eekjn/erai/&
       &erai_glam_'//READTEMP//'0101_'//READTEMP//'1231_africa_00.nc'

!      Open netCDF file.
       STATUS=NF_OPEN(FL,NF_NOWRITE,NCID)
          IF ( STATUS.NE.NF_NOERR ) THEN
           WRITE(IINFFIL,*) 'error reading era-interim nefcdf file'
           STOP
          ENDIF

!      Retrieve data for Variable 'lon' and put into LONGITUDE
       
       VARNAME = 'lon'
       CALL GETVARA(NCID, VARNAME,'lon')

       NLON = 0
       I=1
       DO ILON = 1,NLON_AFRICA
       IF((LONGITUDE(ILON).GE.MINLON_REGION).AND.(LONGITUDE(ILON).LE.MAXLON_REGION)) THEN
       NLON = NLON+1
       LONINDEX_REGION(I) = ILON
       RLONMAT(I) = LONGITUDE(ILON)
       I=I+1
       ENDIF
       ENDDO

       PRINT *, 'number of longitude points in my region is:',NLON
!       PRINT *, 'contents of RLONMAT:',RLONMAT
!       PRINT *, 'longitude indexs', LONINDEX_REGION(1),LONINDEX_REGION(NLON)

!      Retrieve data for Variable 'lat' and put into LATITUDE
       VARNAME='lat'
       CALL GETVARA(NCID,VARNAME,'lat')
       
       NLAT = 0
       I=1
       DO ILAT = 1,NLAT_AFRICA
       IF((LATITUDE(ILAT).GE.MINLAT_REGION).AND.(LATITUDE(ILAT).LE.MAXLAT_REGION)) THEN
       NLAT = NLAT+1
       LATINDEX_REGION(I) = ILAT
       RLATMAT(I) = LATITUDE(ILAT)
       I=I+1
       ENDIF
       ENDDO

       PRINT *, 'number of latitude points in my region is:', NLAT
!       PRINT *, 'contents of RLATMAT:', RLATMAT
!       PRINT *, 'latitude indexs',LATINDEX_REGION(1),LATINDEX_REGION(NLAT)


       STATUS=NF_CLOSE(NCID)
       IF(STATUS.NE.NF_NOERR) WRITE(IINFFIL,*)'error closing file'
      ENDDO

!-----------------------------------------------------------------------
!     Now open all of the weather files from ISYR to IEYR and read in the data
!     Only weather data from the region of interest is saved in WTH

      DO IYR = ISYR,IEYR  
        
       VAR_STORE(:,:,:) = NODATA

       READTEMP='0000'

!      determine name of input file
       WRITE(READTEMP,'(I4)')IYR
       FL='/nfs/see-archive-06_a17/eekjn/erai/&
       &erai_glam_'//READTEMP//'0101_'//READTEMP//'1231_africa_00.nc'

!      make sure output files are named in a sensible way
       DO ILAT=1,NLAT
        DO ILON=1,NLON
          IF((ILAT.LT.10).AND.(ILON.LT.10))THEN
           LATTEMP1='0'
           LONTEMP1='0'
           WRITE(LATTEMP1,'(I1)')ILAT
           WRITE(LONTEMP1,'(I1)')ILON
           FLWTH(ILAT,ILON,IYR-ISYR+1)='__________________________________________africa00'//LATTEMP1//'00'//LONTEMP1//READTEMP
          ELSE IF((ILAT.LT.10).AND.(ILON.GE.10))THEN
           LATTEMP1='0'
           LONTEMP2='00'
           WRITE(LATTEMP1,'(I1)')ILAT
           WRITE(LONTEMP2,'(I2)')ILON
           FLWTH(ILAT,ILON,IYR-ISYR+1)='__________________________________________africa00'//LATTEMP1//'0'//LONTEMP2//READTEMP
          ELSE IF((ILAT.GE.10).AND.(ILON.LT.10))THEN
           LATTEMP2='00'
           LONTEMP1='0'
           WRITE(LATTEMP2,'(I2)')ILAT
           WRITE(LONTEMP1,'(I1)')ILON
           FLWTH(ILAT,ILON,IYR-ISYR+1)='__________________________________________africa0'//LATTEMP2//'00'//LONTEMP1//READTEMP 
          ELSE IF((ILAT.GE.10).AND.(ILON.GE.10))THEN
           LATTEMP2='00'
           LONTEMP2='00'
           WRITE(LATTEMP2,'(I2)')ILAT
           WRITE(LONTEMP2,'(I2)')ILON
           FLWTH(ILAT,ILON,IYR-ISYR+1)='__________________________________________africa0'//LATTEMP2//'0'//LONTEMP2//READTEMP
          ENDIF
!          PRINT *,FLWTH(ILAT,ILON,IYR-ISYR+1)
        ENDDO
       ENDDO

!      Open netCDF input file.
       STATUS=NF_OPEN(FL,NF_NOWRITE,NCID)
          IF ( STATUS.NE.NF_NOERR ) THEN
            WRITE(IINFFIL,*)'error reading nefcdf file'
           STOP
          ENDIF
!-------------------------------------------------------------------------
!      Retrieve data for Variable 'tp'

       VARNAME='tp'
       CALL GETVARA(NCID,VARNAME,'wth')

!      save the values of precipitation that are inside the region of interest in WTH
       I=0
       DO ILAT = LATINDEX_REGION(1),LATINDEX_REGION(NLAT)
        DO ILON = LONINDEX_REGION(1),LONINDEX_REGION(NLON)
         DO IDAY = 1,NDAYSYR
          WTH(ILAT-LATINDEX_REGION(1)+1,ILON-LONINDEX_REGION(1)+1,IYR-ISYR+1,IDAY,4)&
          =VAR_STORE(ILON,ILAT,IDAY)*100.0 !conversion from m/day to cm/day
         ENDDO
          I=I+1
        ENDDO
       ENDDO

       IF(I.NE.NLAT*NLON) WRITE(IINFFIL,*) 'problem with number of gridcells in era-interim read routine'

!      testing for -ve rainfall values and setting them to zero.

       NSMALL = 0
       DO ILAT = 1,NLAT
        DO ILON = 1,NLON
         DO IDAY=1,NDAYSYR
          IF((WTH(ILAT,ILON,IYR-ISYR+1,IDAY,4)).LT.0.0) THEN
           WTH(ILAT,ILON,IYR-ISYR+1,IDAY,4) = 0.0
           NSMALL = NSMALL+1
          ENDIF
         ENDDO
        ENDDO
       ENDDO

       WRITE(IINFFIL,*) 'ERA-INTERIM DATA: number of -ve rainfall values converted to zero in year',IYR,'is',NSMALL

!----------------------------------------------------
!      Retrieve data for Variable 'ssrd'

       VARNAME='ssrd'
       CALL GETVARA(NCID,VARNAME,'wth')

!     save solar rad for the region of interest and convert from W m**-2 seconds to MJ m**-2 per day

       DO ILAT = LATINDEX_REGION(1),LATINDEX_REGION(NLAT)
        DO ILON = LONINDEX_REGION(1),LONINDEX_REGION(NLON)
         DO IDAY = 1,NDAYSYR
          WTH(ILAT-LATINDEX_REGION(1)+1,ILON-LONINDEX_REGION(1)+1,IYR-ISYR+1,IDAY,1)&
          =VAR_STORE(ILON,ILAT,IDAY)/1000000.0
         ENDDO  
        ENDDO
       ENDDO

!----------------------------------------------------
!      Retrieve data for Variable 'sund'

       VARNAME='sund'
       CALL GETVARA(NCID,VARNAME,'wth')

       DO ILAT = LATINDEX_REGION(1),LATINDEX_REGION(NLAT)
        DO ILON = LONINDEX_REGION(1),LONINDEX_REGION(NLON)
         DO IDAY =1,NDAYSYR
          WTH(ILAT-LATINDEX_REGION(1)+1,ILON-LONINDEX_REGION(1)+1,IYR-ISYR+1,IDAY,5)&
          =VAR_STORE(ILON,ILAT,IDAY)
         ENDDO
        ENDDO
       ENDDO

!----------------------------------------------------
!      Retrieve data for Variable 'mxtemp'

       VARNAME='mxtemp'
       CALL GETVARA(NCID,VARNAME,'wth')


       DO ILAT = LATINDEX_REGION(1),LATINDEX_REGION(NLAT) 
        DO ILON = LONINDEX_REGION(1),LONINDEX_REGION(NLON)
         DO IDAY =1,NDAYSYR
          WTH(ILAT-LATINDEX_REGION(1)+1,ILON-LONINDEX_REGION(1)+1,IYR-ISYR+1,IDAY,2)&
          =VAR_STORE(ILON,ILAT,IDAY)
         ENDDO
        ENDDO
       ENDDO

!----------------------------------------------------
!      Retrieve data for Variable 'mntemp'

       VARNAME='mntemp'
       CALL GETVARA(NCID,VARNAME,'wth')

       DO ILAT = LATINDEX_REGION(1),LATINDEX_REGION(NLAT) 
        DO ILON = LONINDEX_REGION(1),LONINDEX_REGION(NLON)
         DO IDAY =1,NDAYSYR
          WTH(ILAT-LATINDEX_REGION(1)+1,ILON-LONINDEX_REGION(1)+1,IYR-ISYR+1,IDAY,3)&
          =VAR_STORE(ILON,ILAT,IDAY)
         ENDDO
        ENDDO
       ENDDO
 
!      convert from Kelvin to degrees C
       DO IDAY=1,NDAYSYR
       WTH(:,:,IYR-ISYR+1,IDAY,3) = WTH(:,:,IYR-ISYR+1,IDAY,3) - 273.15    
       WTH(:,:,IYR-ISYR+1,IDAY,2) = WTH(:,:,IYR-ISYR+1,IDAY,2) - 273.15
       ENDDO
      
      STATUS=NF_CLOSE(NCID)
      IF(STATUS.NE.NF_NOERR) WRITE(IINFFIL,*) 'error closing era-interim weather file'

      ENDDO

!     Kathryn Nicklin introduced this code to output ecmwf weather data to ascii files
!     so that netcdf and binary weather input files do not have to be used.
!      DO IYR=ISYR,IEYR
!       DO ILAT=1,NLAT
!        DO ILON=1,NLON
!         WRITE(YEARTEMP,'(I4)')IYR
!         WRITE(UNIT=LATTEMP,FMT='(F5.1)') RLATMAT(ILAT)
!         WRITE(UNIT=LONTEMP,FMT='(F5.1)') RLONMAT(ILON)
!         FL='inputs_africa/ascii_wth/ecmwf/'//TRIM(ADJUSTL(LATTEMP))//'N_'//TRIM(ADJUSTL(LONTEMP))//'E/ghana001001'//TRIM(YEARTEMP)//'.wth'
!         OPEN(UNIT=83,FILE=FL,STATUS='UNKNOWN',IOSTAT=IOS)
!          IF(IOS.NE.0) THEN
!           PRINT*,'can not open ascii weather output file'
!          ENDIF
!         WRITE(83,*) 'Weather file for Ghana'
!         WRITE(83,*) 'Gridcell latitude and longitude in degrees North and degrees East'
!         WRITE(83,'(A5,F6.2,F6.2)') ' are:', RLATMAT(ILAT), RLONMAT(ILON)
!         WRITE(83,*) 'DATE  SRAD  TMAX   TMIN    RAIN'
!         DO IDAY=1,NDAYSYR
!          WRITE(UNIT=DAYTEMP1,FMT='(I3)')IDAY
!          IF(IDAY.LT.10)THEN
!           DAYTEMP2=YEARTEMP(3:4)//'00'//TRIM(ADJUSTL(DAYTEMP1))
!          ELSEIF(IDAY.LT.100)THEN
!           DAYTEMP2=YEARTEMP(3:4)//'0'//TRIM(ADJUSTL(DAYTEMP1))
!          ELSE
!           DAYTEMP2=YEARTEMP(3:4)//TRIM(ADJUSTL(DAYTEMP1))
!          ENDIF
!          WRITE(83,'(A5,2X,F4.1,3(1X,F6.2))') DAYTEMP2, WTH(ILAT,ILON,IYR-ISYR+1,IDAY,1), WTH(ILAT,ILON,IYR-ISYR+1,IDAY,2), WTH(ILAT,ILON,IYR-ISYR+1,IDAY,3), WTH(ILAT,ILON,IYR-ISYR+1,IDAY,4)*10
!         ENDDO
!         CLOSE(83)
!        ENDDO
!       ENDDO
!      ENDDO

!     If WTHROOT contains gpcp then read in the gpcp rainfall data and overwrite the erai rainfall data
      IF(INDEX(WTHROOT,'gpcp')>0)THEN

      WTH(:,:,:,:,4)=FLOAT(NODATA)
      PRINT *,'calling read_gpcp_1dd'
      CALL READ_GPCP_1DD

!      PRINT *,'grid cell (0.5N, -19.5E) for 1997. rainfall values are:',GPCPRAIN(1,1,4,:)

      DO IYR=ISYR,IEYR
       DO ILAT = LATINDEX_REGION(1),LATINDEX_REGION(NLAT) 
        DO ILON = LONINDEX_REGION(1),LONINDEX_REGION(NLON)
         DO IDAY =1,NDAYSYR
          WTH(ILAT-LATINDEX_REGION(1)+1,ILON-LONINDEX_REGION(1)+1,IYR-ISYR+1,IDAY,4)&
          =GPCPRAIN(ILAT,ILON,IYR-ISYR+1,IDAY)/10. !mm->cm
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      ENDIF

!     Kathryn Nicklin introduced this code to output gpcp weather data to ascii files
!     so that netcdf and binary weather input files do not have to be used.
!      DO IYR=ISYR,IEYR
!       DO ILAT=1,NLAT
!        DO ILON=1,NLON
!         WRITE(YEARTEMP,'(I4)')IYR
!         WRITE(UNIT=LATTEMP,FMT='(F5.1)') RLATMAT(ILAT)
!         WRITE(UNIT=LONTEMP,FMT='(F5.1)') RLONMAT(ILON)
!         FL='inputs_africa/ascii_wth/gpcp/'//TRIM(ADJUSTL(LATTEMP))//'N_'//TRIM(ADJUSTL(LONTEMP))//'E/ghana001001'//TRIM(YEARTEMP)//'.wth'
!         OPEN(UNIT=84,FILE=FL,STATUS='UNKNOWN',IOSTAT=IOS)
!          IF(IOS.NE.0) THEN
!           PRINT*,'can not open ascii weather output file'
!          ENDIF
!         WRITE(84,*) 'Weather file for Ghana'
!         WRITE(84,*) 'Gridcell latitude and longitude in degrees North and degrees East'
!         WRITE(84,'(A5,F6.2,F6.2)') ' are:', RLATMAT(ILAT), RLONMAT(ILON)
!         WRITE(84,*) 'DATE  SRAD  TMAX   TMIN    RAIN'
!         DO IDAY=1,NDAYSYR
!          WRITE(UNIT=DAYTEMP1,FMT='(I3)')IDAY
!          IF(IDAY.LT.10)THEN
!           DAYTEMP2=YEARTEMP(3:4)//'00'//TRIM(ADJUSTL(DAYTEMP1))
!          ELSEIF(IDAY.LT.100)THEN
!           DAYTEMP2=YEARTEMP(3:4)//'0'//TRIM(ADJUSTL(DAYTEMP1))
!          ELSE
!           DAYTEMP2=YEARTEMP(3:4)//TRIM(ADJUSTL(DAYTEMP1))
!          ENDIF
!          WRITE(84,'(A5,2X,F4.1,3(1X,F6.2))') DAYTEMP2, WTH(ILAT,ILON,IYR-ISYR+1,IDAY,1), WTH(ILAT,ILON,IYR-ISYR+1,IDAY,2), WTH(ILAT,ILON,IYR-ISYR+1,IDAY,3), WTH(ILAT,ILON,IYR-ISYR+1,IDAY,4)*10
!         ENDDO
!         CLOSE(84)
!        ENDDO
!       ENDDO
!      ENDDO

!--------------------------------------------------------------------------------
!     now all data is in WTH(MAXLAT,MAXLON,MAXYRS,NDAYSYR,NWTHVAR) but need to
!     reorder so that it starts on IPDATE (i.e. the START of the intellegent sowing window)

      PRINT *, 'reordering the weather data'
      DO ILAT=1,NLAT
       DO ILON=1,NLON
        IF(IPDATEMAT(ILAT,ILON).NE.NODATA) THEN
         DO IYR = ISYR,IEYR
          ICOUNT=0
          DO IDAY=1,NDAYSYR !Note that NDAYSYR=365 so the last day (31 dec) of leap years is ignored
           IF(IDAY.GE.IPDATEMAT(ILAT,ILON)) THEN
            ICOUNT=ICOUNT+1
            WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,1) = WTH(ILAT,ILON,IYR-ISYR+1,IDAY,1)
            WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,2) = WTH(ILAT,ILON,IYR-ISYR+1,IDAY,2)
            WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,3) = WTH(ILAT,ILON,IYR-ISYR+1,IDAY,3)
            WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,4) = WTH(ILAT,ILON,IYR-ISYR+1,IDAY,4)
            WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,5) = WTH(ILAT,ILON,IYR-ISYR+1,IDAY,5)
           ENDIF
          ENDDO
          NWTHMAT(ILAT,ILON,IYR-ISYR+1) = ICOUNT
          DO IDAY = NWTHMAT(ILAT,ILON,IYR-ISYR+1)+1,NDAYSYR
           WTH(ILAT,ILON,IYR-ISYR+1,IDAY,:) = NODATA
          ENDDO
         ENDDO
        ELSE
         JSKIP(ILAT,ILON)=1
        ENDIF
       ENDDO
      ENDDO

!      Kathryn Nicklin added the following few lines so that could calculate average July temps and average June-July-August temps
!      for each of the 20 years of era-interim data. This is set up to just to one gridcell at a time. These lines are not
!      needed for anything else.
!       AVG_JJATEMP(:) = 0.0
!       DO IYR = ISYR,IEYR
!        DO IDAY = 152,243
!         AVG_JJATEMP(IYR-ISYR+1) = AVG_JJATEMP(IYR-ISYR+1) + ((WTH(1,1,IYR-ISYR+1,IDAY,2) + WTH(1,1,IYR-ISYR+1,IDAY,3))/2)
!        ENDDO
!       ENDDO

!       AVG_JJATEMP(:) = AVG_JJATEMP/92.0
       
!       DO IYR = ISYR,IEYR
!       PRINT*,AVG_JJATEMP(IYR-ISYR+1)
!       ENDDO


!      Kathryn Nicklin added the following few lines February 2010 so that could output erai temp information to files. These lines
!      are not needed for anything else. 
!      OPEN(UNIT=37,FILE='output_africa/erai_temp_data/year_2008_lat_8.5_lon_16.5.out',STATUS='UNKNOWN',IOSTAT=IOS)
!      IF(IOS.NE.0)THEN
!       WRITE(IINFFIL,*)'Error creating temp output file'
!       STOP
!      ENDIF
!      DO IDAY=1,NDAYSYR
!      WRITE(37,*) IDAY, WTH(1,1,1,IDAY,2), WTH(1,1,1,IDAY,3), ((WTH(1,1,1,IDAY,3) + WTH(1,1,1,IDAY,2))/2)
!      ENDDO
!      CLOSE(37)

!      PRINT *,'last gridcell in first year min temp values in WTH are:',WTH(21,42,1,:,2)

!     check weather data in WTH is realistic, i.e don't have -ve rainfall, radiation or sun duration
!     and make sure Tmax > Tmin
      DO ILAT=1,NLAT
       DO ILON=1,NLON
        DO IYR=ISYR,IEYR
         DO IDAY = 1,NWTHMAT(ILAT,ILON,IYR-ISYR+1)
          IF((WTH(ILAT,ILON,IYR-ISYR+1,IDAY,1).LT.0.0).OR.(WTH(ILAT,ILON,IYR-ISYR+1,IDAY,4).LT.0.0)&
            .OR.(WTH(ILAT,ILON,IYR-ISYR+1,IDAY,5).LT.0.0).OR.(WTH(ILAT,ILON,IYR-ISYR+1,IDAY,3).GT.&
            WTH(ILAT,ILON,IYR-ISYR+1,IDAY,2))) THEN
            PRINT*, 'unrealistic weather data in year and day', IYR, IDAY
            PRINT*, WTH(ILAT,ILON,IYR-ISYR+1,IDAY,1:5)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL TESTSKIP('ERAweather') 

!---------------------------------------------------------------------------------------------------
! The following ELSEIF was introduced 15Jan2009 by Kathryn Nicklin to allow different weather data to be read in

      ELSEIF(INDEX(WTHROOT,'gujarat')>0)THEN

       WRITE(IINFFIL,*)'Reading in started'
       FILEPATH(1)='inputs_gujarat/weather/rainfall.dat' 
       FILEPATH(2)='inputs_gujarat/weather/maxtemp.dat'
       FILEPATH(3)='inputs_gujarat/weather/mintemp.dat'
       FILEPATH(4)='inputs_gujarat/weather/radiation.dat'

       RAINY=FLOAT(NODATA)
       DAILYTEMPMAX=FLOAT(NODATA)
       DAILYTEMPMIN=FLOAT(NODATA)
       DAILYRAD=FLOAT(NODATA)
       WTH=FLOAT(NODATA)

       NLAT = 1
       NLON = 1
       RLAT = 23.00  !latitude and longitude for gujarat
       RLON = -72.00
       RLATMAT(1) = RLAT
       RLONMAT(1) = RLON

!      CHANGED NYEARS FROM 24 TO 23
       NYEARS =24

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAINFALL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c      Reads rainfall data from rainfall.dat into RAINY(24,365) and converts it to cm/day
!c      The data only runs from 1st June(152nd day of the year) to  30th Sept inclusive, all days after this have 0.0 rainfall.

       OPEN(UNIT=IFLWTH,FILE=FILEPATH(1),STATUS='OLD',IOSTAT=IOS)
       IF(IOS.NE.0)THEN
        WRITE(IINFFIL,*)'Error opening rainfall input file'
        STOP
       ENDIF

       DO IYR=1,NYEARS
        READ(IFLWTH,'(10X,I2,14X,I4)')JCHECK,YEARCHECK(IYR)
        IF(JCHECK.NE.25)THEN
         WRITE(IINFFIL,*)'Error, JCHECK does not equal 25'
        ENDIF

        IDEN=1
         DO K=1,7
          READ(IFLWTH,'(16(F5.1))')RAINY(IYR,IDEN:IDEN+15)
          IDEN=IDEN+16
         ENDDO
        READ(IFLWTH,'(10(F5.1))')RAINY(IYR,IDEN:IDEN+9)
       ENDDO

!c      convert from mm/day to cm/day and adjust so that the data starts on the 1st of Jan (although the values will be NODATA until 1st June)

       DO P=1,NYEARS
        DO K=1,122
         RAINY(P,K+151)=RAINY(P,K)/10.0
         RAINY(P,K)=FLOAT(NODATA)
        ENDDO
       ENDDO

!c     this is not real data but i have made the reasonable assumption that there is no rainfall after 30th September
       DO P=1,NYEARS
       DO K=274,365
         RAINY(P,K)=0.0
       ENDDO
       ENDDO

       CLOSE(IFLWTH)

!!!!!!!!!!!!!!!!!!!TEMPERATURE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!c      read in the max temps

       OPEN(UNIT=IFLWTH,FILE=FILEPATH(2),STATUS='OLD',IOSTAT=IOS)
       IF(IOS.NE.0)THEN
        WRITE(IINFFIL,*)'Error opening max temp input file'
        STOP
       ENDIF

       DO IYR=1,NYEARS
        READ(IFLWTH,'(5X,I4,1X,12(2X,I3))')YEARCHECK(IYR),MONTHMAXTEMP(IYR,1:12)
!c        IF(YEARCHECK(IYR).NE.1965+IYR)THEN
!c         WRITE(IINFFIL,*),'Error, Year is wrong'
!c        ENDIF
       ENDDO

       CLOSE(IFLWTH)

!c      read in the min temps

       OPEN(UNIT=IFLWTH,FILE=FILEPATH(3),STATUS='OLD',IOSTAT=IOS)
       IF(IOS.NE.0)THEN
        WRITE(IINFFIL,*)'Error opening min temp input file'
        STOP
       ENDIF
    
       DO IYR=1,NYEARS
        READ(IFLWTH,'(5X,I4,1X,12(2X,I3))')YEARCHECK(IYR),MONTHMINTEMP(IYR,1:12)
!c        IF(YEARCHECK(IYR).NE.1965+IYR)THEN
!c         WRITE(IINFFIL,*)'Error, Year is wrong'
!c        ENDIF
       ENDDO

       CLOSE(IFLWTH)

!c      Linear interpolation, works correctly from 16Jan-->15thDec
!c      Temperature data outside this range is not needed for current study.

       DO IYR=1,NYEARS
        DO MONTH=1,11
         DELTAWTH=(MONTHMAXTEMP(IYR,MONTH+1)-MONTHMAXTEMP(IYR,MONTH))*FLOAT(IDT)/(IDAYMID(MONTH+1)-IDAYMID(MONTH))
         DO IDEN=IDAYMID(MONTH),IDAYMID(MONTH+1)
          DAILYTEMPMAX(IYR,IDEN)=MONTHMAXTEMP(IYR,MONTH)+DELTAWTH*FLOAT(IDEN-IDAYMID(MONTH))
         ENDDO
        ENDDO
       ENDDO

       DO IYR=1,NYEARS
        DO MONTH=1,11
         DELTAWTH=(MONTHMINTEMP(IYR,MONTH+1)-MONTHMINTEMP(IYR,MONTH))*FLOAT(IDT)/(IDAYMID(MONTH+1)-IDAYMID(MONTH))
         DO IDEN=IDAYMID(MONTH),IDAYMID(MONTH+1)
          DAILYTEMPMIN(IYR,IDEN)=MONTHMINTEMP(IYR,MONTH)+DELTAWTH*FLOAT(IDEN-IDAYMID(MONTH))
         ENDDO
        ENDDO
       ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!RADIATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!c      The data does not change from year to year
!c      The interpolated radiation data in dailyrad runs from day 136 (middle of May) to day 350 (middle of December)

       OPEN(UNIT=IFLWTH,FILE=FILEPATH(4),STATUS='OLD',IOSTAT=IOS)
       IF(IOS.NE.0)THEN
        WRITE(IINFFIL,*)'Error opening radiation input file'
        STOP
       ENDIF

       DO K=1,2
        READ(IFLWTH,*)
       ENDDO

       READ(IFLWTH,'(8(2X,F5.1))')MONTHRAD(1:8)

       CLOSE(IFLWTH)
      

       DO MONTH=1,7
        DELTAWTH=(MONTHRAD(MONTH+1)-MONTHRAD(MONTH))*FLOAT(IDT)/(IDAYMID(MONTH+5)-IDAYMID(MONTH+4))
        DO IDEN=IDAYMID(MONTH+4),IDAYMID(MONTH+5)
        DAILYRAD(IDEN)=MONTHRAD(MONTH)+DELTAWTH*FLOAT(IDEN-IDAYMID(MONTH+4))
        ENDDO
       ENDDO

      
!c      DO K=1,135
!c      PRINT *,'the first batch is', DAILYRAD(K)
!c      ENDDO
      
!c      DO K=136,365
!c       PRINT *,'the second batch is', DAILYRAD(K)
!c      ENDDO

!c      convert radiation from W/m^2 to MJ/m^2/day
       DO K=136,350    
        DAILYRAD(K)=DAILYRAD(K)*60*60*24/1000000
       ENDDO

!!!!!!!!!!!!!!!!!!put into WTH()!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       IF(IPDATEMAT(1,1).NE.NODATA) THEN
        DO IYR=ISYR,IEYR
         ICOUNT=0
         DO K=1,365
          IF(K.GE.IPDATEMAT(1,1)) THEN
           WTH(1,1,IYR-ISYR+1, K-IPDATEMAT(1,1)+1,1)=DAILYRAD(K)
           WTH(1,1,IYR-ISYR+1, K-IPDATEMAT(1,1)+1,2)=DAILYTEMPMAX(IYR-1965,K)/10.0
           WTH(1,1,IYR-ISYR+1, K-IPDATEMAT(1,1)+1,3)=DAILYTEMPMIN(IYR-1965,K)/10.0
           WTH(1,1,IYR-ISYR+1, K-IPDATEMAT(1,1)+1,4)=RAINY(IYR-1965,K)
           IF((DAILYRAD(K).GT.-99.).AND.(DAILYTEMPMAX(IYR-1965,K).GT.-99.).AND.(DAILYTEMPMIN(IYR-1965,K).GT.-99.).AND.(RAINY(IYR-1965,K).GT.-99.)) THEN
           ICOUNT=ICOUNT+1
           ENDIF
          ENDIF
         ENDDO
         NWTHMAT(1,1,IYR-ISYR+1)=ICOUNT
       ENDDO
       ELSE
        JSKIP(1,1)=1
        WRITE(IINFFIL,*)'No data in planting date matrix'
       ENDIF
       
       WRITE(IINFFIL,*)'NWTHMAT for the start year is',NWTHMAT(1,1,1)
       CALL TESTSKIP('myweather')

!c     Need to put info into FLWTH to make sure the daily output files are named correctly

       FLWTH(1,1,1)='inputs/ascii/wth/egeg0010011966'
       FLWTH(1,1,2)='inputs/ascii/wth/egeg0010011967'
       FLWTH(1,1,3)='inputs/ascii/wth/egeg0010011968'
       FLWTH(1,1,4)='inputs/ascii/wth/egeg0010011969'
       FLWTH(1,1,5)='inputs/ascii/wth/egeg0010011970'
       FLWTH(1,1,6)='inputs/ascii/wth/egeg0010011971'
       FLWTH(1,1,7)='inputs/ascii/wth/egeg0010011972'
       FLWTH(1,1,8)='inputs/ascii/wth/egeg0010011973'
       FLWTH(1,1,9)='inputs/ascii/wth/egeg0010011974'
       FLWTH(1,1,10)='inputs/ascii/wth/egeg0010011975'
       FLWTH(1,1,11)='inputs/ascii/wth/egeg0010011976'
       FLWTH(1,1,12)='inputs/ascii/wth/egeg0010011977'
       FLWTH(1,1,13)='inputs/ascii/wth/egeg0010011978'
       FLWTH(1,1,14)='inputs/ascii/wth/egeg0010011979'
       FLWTH(1,1,15)='inputs/ascii/wth/egeg0010011980'
       FLWTH(1,1,16)='inputs/ascii/wth/egeg0010011981'
       FLWTH(1,1,17)='inputs/ascii/wth/egeg0010011982'
       FLWTH(1,1,18)='inputs/ascii/wth/egeg0010011983'
       FLWTH(1,1,19)='inputs/ascii/wth/egeg0010011984'
       FLWTH(1,1,20)='inputs/ascii/wth/egeg0010011985'
       FLWTH(1,1,21)='inputs/ascii/wth/egeg0010011986'
       FLWTH(1,1,22)='inputs/ascii/wth/egeg0010011987'
       FLWTH(1,1,23)='inputs/ascii/wth/egeg0010011988'
       FLWTH(1,1,24)='inputs/ascii/wth/egeg0010011989'

      ELSEIF (INDEX(WTHROOT,'netcd')>0) THEN         
         !Determine spatiotemporal domain
         !The four vars below could be modified by GUI, rather than being hardwired or command line
         !    DO I=1,4
         !       CALL GETARG(I,CINP(I)) 
         !    ENDDO
         !    READ(CINP(1),*)RCOIN(1) !Min longitude
         !    READ(CINP(2),*)RCOIN(2) !Max lon
         !    READ(CINP(3),*)RCOIN(3) !Min lat
         !    READ(CINP(4),*)RCOIN(4) !Max lat
         RCOIN(1)=74.
         RCOIN(2)=79.
         RCOIN(3)=22.
         RCOIN(4)=26.
         WRITE(IINFFIL,*)'Input area = ',RCOIN
         
         !Read in and apply land-sea mask
         DO IYR=1,1
            FLNC='inputs/netcdf/QUMP_LandSurfMask.nc'
            CALL READ_WTHNC(FLNC,9,1)
         ENDDO
         WHERE (RLSMASK(:,:)>TESTRNCNODATA) JSKIP(:,:)=1
         CALL TESTSKIP('NetCDF LS mask ')

         !Read in weather data
         WTH=RNCNODATA
         IMC=INDEX(WTHROOT,'#') !Create weather file name from information between the hashes
         IF (IMC>0) THEN          !MMMMM (M=model run code, e.g. aexsj_P or Histor2)             
            MODL=WTHROOT(IMC+1:IMC+8)
         ELSE
            WRITE(IINFFIL,*)'SR READ_SPAT: Input weather data error - no information on model/month/ensemble'
            STOP
         ENDIF
         SVAR='srad tmax tmin prec' !Variable names (in filenames)
!         IF (IMON==58) THEN  !Allow for reading in of both start dates
!            LUP=.TRUE.
!            IMON=5
!         ELSE
!            LUP=.FALSE.            
!         ENDIF
10       DO IVAR=1,4 !Four weather variables
            IVC=(IVAR-1)*5+1
           DO IYR=ISYR,IEYR              
              !Set filename, e.g. Histor2_1989_00000_tmin.nc
              FLNC=WTHROOT(1:IMC-1)//MODL//'_'//'    '//'_00000_'//SVAR(IVC:IVC+3)//'.nc'
              IBL=INDEX(FLNC,' ')
              WRITE(FLNC(IBL:IBL+3),'(I4)')IYR
!              FLNC(IBL+4:IBL+4)='0'
!              WRITE(FLNC(IBL+5:IBL+5),'(I1)')IMON
!              FLNC(IBL+6:IBL+8)='.nc'
              FLNC=TRIM(FLNC)   
              !Read in data
              CALL READ_WTHNC(FLNC,IVAR)
           ENDDO

           !Convert weather to correct units. 
           IF (IVAR==2.OR.IVAR==3) THEN
              WHERE (WTH(:,:,:,:,IVAR)>TESTNODATA) WTH(:,:,:,:,IVAR)=WTH(:,:,:,:,IVAR)-TTRIP 
           ELSEIF (IVAR==4) THEN !Rainfall kg/m-2/s-1 -> cm; also removes negative values
              WHERE (WTH(:,:,:,:,IVAR)>TESTNODATA) WTH(:,:,:,:,IVAR) = &
                   MAX(0.,WTH(:,:,:,:,IVAR)*100./RHO_W*60.*60.*24.)
           ELSEIF (IVAR==1) THEN
              WHERE (WTH(:,:,:,:,IVAR)>TESTNODATA) WTH(:,:,:,:,IVAR) = &
                   WTH(:,:,:,:,IVAR) *24.*60.*60. /1E6 !w/m-2 -> MJ/day 
           ENDIF
        ENDDO
        
        !Reorder WTH st the first day is the planting date
        DO IVAR=1,4
           DO ILAT=1,NLAT
              DO ILON=1,NLON                
                 IF (IPDATEMAT(ILAT,ILON).NE.NODATA) THEN
                    YEAR: DO IYR=ISYR,IEYR
                       IED=NDAYSYR-IPDATEMAT(ILAT,ILON)+1
                       IPOFF=0 !ITIMOFF has already been used to get first day to be Jan 1st
                       !IPOFF=(IMON-1)*30 !no of pre-wth-data months * no of days per month
                       IF (IPDATEMAT(ILAT,ILON)-IPOFF>0) THEN
                          WTH(ILAT,ILON,IYR-ISYR+1,1:IED,IVAR)= WTH(ILAT,ILON,IYR-ISYR+1,IPDATEMAT(ILAT,ILON)-IPOFF:,IVAR)
                       ELSE
                          WRITE(IINFFIL,*)'Planting date ',IPDATEMAT(ILAT,ILON),' not found in weather data'
                          STOP
                       ENDIF
                       WTH(ILAT,ILON,IYR-ISYR+1,IED+1:,IVAR)= FLOAT(NODATA)
                       IF (WTH(ILAT,ILON,IYR-ISYR+1,1,IVAR)<TESTNODATA) THEN
                          JSKIP(ILAT,ILON)=1
                          EXIT YEAR
                       ENDIF
                    ENDDO YEAR
                 ELSE
                    JSKIP(ILAT,ILON)=1                       
                 ENDIF
              ENDDO
           ENDDO
        ENDDO
        
        CALL TESTSKIP('NetCDF weather')
        !Calculate NWTHMAT
        DO ILAT=1,NLAT
          DO ILON=1,NLON
           DO IYR=ISYR,IEYR
            ICOUNT=0
            WLOOP: DO I=1,NDAYSYR
             IF (WTH(ILAT,ILON,IYR-ISYR+1,I,1)>TESTNODATA.AND.WTH(ILAT,ILON,IYR-ISYR+1,I,2)>TESTNODATA &
                  .AND.WTH(ILAT,ILON,IYR-ISYR+1,I,3)>TESTNODATA.AND.WTH(ILAT,ILON,IYR-ISYR+1,I,4)>TESTNODATA) THEN
                ICOUNT=ICOUNT+1
             ELSE 
                EXIT WLOOP
             ENDIF
            ENDDO WLOOP
            NWTHMAT(ILAT,ILON,IYR-ISYR+1)=ICOUNT
           ENDDO
          ENDDO
         ENDDO

      ELSE
         WRITE(IINFFIL,*)'Incomprehensible weather input file directory'
         CLOSE(IINFFIL)
         STOP
      ENDIF
      WRITE(IINFFIL,*)'Input grid size [lat,lon] is ',NLAT, NLON,' = ',NLAT*NLON

!C------------------------------------------------------------
!C  SOIL INPUTS
!C------------------------------------------------------------     

      IF (RI(KRLL)%VAL.LT.TESTNODATA.OR.RI(KDUL)%VAL.LT.TESTNODATA.OR.RI(KSAT)%VAL.LT.TESTNODATA) THEN
         !Read in soil types (soil lookup table)
         OPEN(UNIT=IFLSTYP,FILE=FLSTYP,STATUS='old',IOSTAT=IOS)
         IF (IOS.NE.0) THEN
            WRITE(IINFFIL,*)'Incomprehensible soil input file: ',FLSTYP
            CLOSE(IINFFIL)
            STOP
         ENDIF
         READ(IFLSTYP,*)
         SOIL(1:MAXNSOILS)%RLL=NODATA
         SOIL(1:MAXNSOILS)%DUL=NODATA
         SOIL(1:MAXNSOILS)%SAT=NODATA
         DO K=1,MAXNSOILS
!        June 2010 Kathryn Nicklin changed the format of the read statement from
!        '(I1,10X,11(F3.2,4X))' to '(I3,8X,11(F3.2,4X))'
            READ(IFLSTYP,'(I3,8X,11(F3.2,4X))',END=33)KSOIL, &
                 SOIL(KSOIL)%RLL, SOIL(KSOIL)%RLL_L, SOIL(KSOIL)%RLL_U, &
                 SOIL(KSOIL)%DUL, SOIL(KSOIL)%DUL_L, SOIL(KSOIL)%DUL_U, &
                 SOIL(KSOIL)%SAT, SOIL(KSOIL)%SAT_L, SOIL(KSOIL)%SAT_U, &
                 SOIL(KSOIL)%ASW_L, SOIL(KSOIL)%ASW_U
         ENDDO
33       CLOSE(IFLSTYP)

         !Read in soil geocode file
         IGEOIN(:,:)=NODATA
         CALL READ_LATLONINT(FLSOL,IGEOIN)  
      ENDIF
 
      !Set soil parameters         
      IF (RI(KRLL)%VAL.LT.TESTNODATA) THEN
         DO ILAT=1,NLAT
            DO ILON=1,NLON              
               IF (IGEOIN(ILAT,ILON).NE.NODATA) THEN
                  RLLMAT(ILAT,ILON)=SOIL(IGEOIN(ILAT,ILON))%RLL
               ELSE
                  JSKIP(ILAT,ILON)=1
               ENDIF
            ENDDO
         ENDDO
      ELSE
         RLLMAT(:,:)=RI(KRLL)%VAL
      ENDIF
      IF (RI(KDUL)%VAL.LT.TESTNODATA) THEN
         DO ILAT=1,NLAT
            DO ILON=1,NLON       
               IF (IGEOIN(ILAT,ILON).NE.NODATA) THEN
                  DULMAT(ILAT,ILON)=SOIL(IGEOIN(ILAT,ILON))%DUL
               ELSE
                  JSKIP(ILAT,ILON)=1
               ENDIF
            ENDDO
         ENDDO
      ELSE
         DULMAT(:,:)=RI(KDUL)%VAL
      ENDIF
      IF (RI(KSAT)%VAL.LT.TESTNODATA) THEN
         DO ILAT=1,NLAT
            DO ILON=1,NLON    
               IF (IGEOIN(ILAT,ILON).NE.NODATA) THEN
                  SATMAT(ILAT,ILON)=SOIL(IGEOIN(ILAT,ILON))%SAT
               ELSE
                  JSKIP(ILAT,ILON)=1
               ENDIF
            ENDDO
         ENDDO
      ELSE
         SATMAT(:,:)=RI(KSAT)%VAL
      ENDIF

      CALL TESTSKIP('soils         ')

      !C------------------------------------------------------------
      !C  YGP AND OBSERVED YIELDS
      !C------------------------------------------------------------     

      !Read in YGP file
      IF (RI(0)%VAL<TESTNODATA) THEN
         YGPMAT=FLOAT(NODATA)     
         CALL READ_LATLONREAL(FLYGP,YGPMAT) 
         DO ILAT=1,NLAT
            DO ILON=1,NLON
               IF (YGPMAT(ILAT,ILON)<TESTNODATA) JSKIP(ILAT,ILON)=1
            ENDDO
         ENDDO
      ENDIF
      CALL TESTSKIP('YGP           ')

      !Read in observed yield
      OPEN(UNIT=IFLYLD,FILE=FLYLD,STATUS='old',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE(IINFFIL,*)'Input yield data file not found',FLYLD
         IF (MODE=='HYP') THEN
            CLOSE(IINFFIL)
            STOP
         ENDIF
      ELSE
         OBS_YLD(:,:,:)=FLOAT(NODATA)
         CALL READ_LATLONTIMREAL(OBS_YLD) 
      ENDIF

      !Kathryn Nicklin added this code instead of the following 3 lines
      !so that only grid cells with no yield data at all are skipped.      
      DO ILAT=1,NLAT
       DO ILON=1,NLON
        J=0
        DO IYR=1,IEYR-ISYR+1
         IF(OBS_YLD(ILAT,ILON,IYR).LT.TESTNODATA) J=J+1
        ENDDO
        IF(J.EQ.(IEYR-ISYR+1)) JSKIP(ILAT,ILON)=1
       ENDDO
      ENDDO

!      DO IYR=1,IEYR-ISYR+1
!         WHERE (OBS_YLD(:,:,IYR)<TESTNODATA) JSKIP(:,:)=1 
!      ENDDO

     CALL TESTSKIP('yield')

     WRITE(IINFFIL,*)'Number of grid cells skipped per year = ',SUM(JSKIP(1:NLAT,1:NLON))



!C----------------------------------------------------------------------------------------------
      RETURN

    END SUBROUTINE READ_SPAT
!C----------------------------------------------------------------------------------------------

    SUBROUTINE TESTSKIP(CERR)
      USE MULTI_VARS
      IMPLICIT NONE
      INTEGER :: I,J
      CHARACTER :: CERR*14
      IF (SUM(JSKIP(1:NLAT,1:NLON))==NLON*NLAT) THEN
         WRITE(IINFFIL,*)'All NLON*NLAT grid cells skipped - check input data availability: '//TRIM(CERR)
         CLOSE(IINFFIL)
         STOP
      ELSE
         WRITE(IINFFIL,*)'Total grid cells skipped by ' &
              //TRIM(CERR)//' stage = ',SUM(JSKIP(1:NLAT,1:NLON))
         IF (SUM(JSKIP(1:NLAT,1:NLON))>0) WRITE(IINFFIL,*)'Comprising'
         DO I=1,NLAT
            DO J=1,NLON
!              The indices of JSKIP on the following line were switched by Kathryn Nicklin Dec 2009 because they were wrong!
               IF (JSKIP(I,J)>0) WRITE(IINFFIL,*)I,RLATMAT(I),J,RLONMAT(J)
            ENDDO
         ENDDO
      ENDIF
      RETURN
    END SUBROUTINE TESTSKIP


    SUBROUTINE READ_WTHTEXT(IYEAR,IPDAT)
      !C Read in *.wth file (i.e. DSSAT-compatable format)
      !C    Weather data indices are in DAP
      USE GLOBAL_VARS
      USE IO_VARS
      USE MULTI_VARS
      USE HIGHT_VARS
      IMPLICIT NONE
      CHARACTER*23, PARAMETER :: WTHFMT='(I5,2X,F4.1,3(1X,F6.2))'
      CHARACTER*7 :: CDUM
      INTEGER :: I,I_ACTION,IDUM,ICHECK,ICOUNT,IW,IDAY,IYEAR,IPDAT
      REAL, DIMENSION (4) :: WIN
      REAL :: RDUM
      LOGICAL :: LWTH

      DO I=1,2
         READ(IFLWTH,*)
      ENDDO
      READ(IFLWTH,*)CDUM,RLAT,RLON
      READ(IFLWTH,*)
      I_ACTION=1
      
      DUMMYREAD:      DO I=1,NDAYSYR        
         READ(IFLWTH,WTHFMT,END=1)IDUM,RDUM,RDUM,RDUM,RDUM
         IYEAR=INT(IDUM/1000)
         IDAY=MOD(IDUM,1000)
         IF (IDAY.EQ.IPDAT-1) THEN 
            I_ACTION=2
            IF (I==1) ICHECK=IDAY-1
            EXIT
         ELSE IF(IDAY.EQ.IPDAT) THEN 
            I_ACTION=3
            EXIT
         ENDIF
         ICHECK=IDAY
      ENDDO DUMMYREAD
1     IF (I_ACTION.EQ.1) THEN
         WRITE(IINFFIL,*)'IPDAT= ',IPDAT,' not found in ',FLWTH(ILAT,ILON,IYR-ISYR+1)
         CLOSE(IINFFIL)
         STOP
      ELSE IF (I_ACTION.EQ.2.AND.ICHECK+1.NE.IDAY) THEN
         WRITE(IINFFIL,*)'Error in reading weather: non-sequencial dates',icheck,idum
         CLOSE(IINFFIL)
         STOP
      ELSE IF (I_ACTION.EQ.3) THEN
         REWIND(IFLWTH)
         DO I=1,4
            READ(IFLWTH,*)
         ENDDO
      ENDIF
      ICOUNT=0
      LWTH=.FALSE.
      WLOOP: DO I=1,NDAYSYR
         READ(IFLWTH,WTHFMT,END=10)IW,WIN(1),WIN(2),WIN(3),WIN(4)
         IW=MOD(IW,1000)  !Exctracts DOY from IDATE
         IF (IW==IPDAT) LWTH=.TRUE. !First day of saved weather data is IPDAT
         IF (LWTH) THEN 
            IF (WIN(1)>TESTNODATA.AND.WIN(2)>TESTNODATA.AND.WIN(3)>TESTNODATA.AND.WIN(3)>TESTNODATA) THEN
               ICOUNT=ICOUNT+1
               WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,1)=WIN(1)
               WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,2)=WIN(2) 
               WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,3)=WIN(3) 
               WTH(ILAT,ILON,IYR-ISYR+1,ICOUNT,4)=WIN(4)/10. !mm-->cm
            ELSE 
               EXIT WLOOP
            ENDIF
         ENDIF
      ENDDO WLOOP
10    NWTHMAT(ILAT,ILON,IYR-ISYR+1)=ICOUNT
      CLOSE(IFLWTH)
      RETURN
    END SUBROUTINE READ_WTHTEXT
    
    
    SUBROUTINE READ_LATLONREAL(FNAME,DMAT)
      USE MULTI_VARS
      USE IO_VARS
      IMPLICIT NONE
      REAL, DIMENSION (MAXLAT,MAXLON) :: DMAT
      CHARACTER*50 :: FNAME !Kathryn Nicklin changed this from 40 to 50
      INTEGER :: K,IOS
      REAL :: RIN
      OPEN(UNIT=IFLGRID,FILE=FNAME,STATUS='OLD',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE(IINFFIL,*)'Incomprehensible input file: ',FNAME
         CLOSE(IINFFIL)
         STOP
      ENDIF
      DO K=1,MAXLON*MAXLAT
         READ(IFLGRID,*,END=44)ILAT,ILON,RIN
         IF (ILAT.LE.MAXLAT.AND.ILON.LE.MAXLON) THEN 
            DMAT(ILAT,ILON)= RIN
         ENDIF
      ENDDO
44    CLOSE(IFLGRID)
    END SUBROUTINE READ_LATLONREAL

!   Subroutine added by Kathryn Nicklin September 2010 to read in 
!   info about the yield data such are the yield and areaeqprod flag
!   and the low_cult_area grid cells
    SUBROUTINE READ_LATLONTIMINT(FNAME,IMAT)
      USE MULTI_VARS
      USE IO_VARS
      IMPLICIT NONE
      INTEGER,DIMENSION (MAXLAT,MAXLON, MAXYRS) :: IMAT
      INTEGER :: K, IIN, IOS
      CHARACTER*50 :: FNAME
      OPEN(UNIT=IFLGRID,FILE=FNAME,STATUS='OLD',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
       WRITE(IINFFIL,*)'Incomprehensible input file: ',FNAME
       CLOSE(IINFFIL)
       STOP
      ENDIF
      DO K=1,MAXLON*MAXLAT*MAXYRS
       READ(IFLGRID,*,END=43)IYR,ILAT,ILON,IIN
       IF (IYR.LE.IEYR.AND.IYR.GE.ISYR.AND.ILAT.LE.MAXLAT.AND.ILON.LE.MAXLON) THEN 
        IMAT(ILAT,ILON,IYR-ISYR+1) = IIN
       ENDIF
      ENDDO
43    CLOSE(IFLGRID)
     END SUBROUTINE READ_LATLONTIMINT      

   SUBROUTINE READ_LATLONTIMREAL(DMAT)
      USE MULTI_VARS
      USE IO_VARS
      IMPLICIT NONE
      REAL, DIMENSION (MAXLAT,MAXLON,1:IEYR-ISYR+1) :: DMAT
      REAL :: RIN
      INTEGER :: K
      DO K=1,MAXLON*MAXLAT*MAXYRS
         READ(IFLYLD,*,END=45)IYR,ILAT,ILON,RIN
         IF (IYR.LE.IEYR.AND.IYR.GE.ISYR.AND.ILAT.LE.MAXLAT.AND.ILON.LE.MAXLON) THEN 
            DMAT(ILAT,ILON,IYR-ISYR+1) = RIN
         ENDIF
      ENDDO
45    CLOSE(IFLYLD)
    END SUBROUTINE READ_LATLONTIMREAL


    SUBROUTINE READ_LATLONINT(FNAME,IMAT)
      USE MULTI_VARS
      USE IO_VARS
      IMPLICIT NONE
      INTEGER, DIMENSION (MAXLAT,MAXLON) :: IMAT
      CHARACTER*50 :: FNAME !Kathryn Nicklin changed this from 40 to 50
      INTEGER :: K,IOS,IIN
      OPEN(UNIT=IFLGRID,FILE=FNAME,STATUS='OLD',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE(IINFFIL,*)'Incomprehensible input file: ',FNAME
         CLOSE(IINFFIL)
         STOP
      ENDIF
      DO K=1,MAXLON*MAXLAT
         READ(IFLGRID,*,END=46)ILAT,ILON,IIN
         IF (ILAT.LE.MAXLAT.AND.ILON.LE.MAXLON) THEN 
            IMAT(ILAT,ILON)= IIN
         ENDIF         
      ENDDO
46    CLOSE(IFLGRID)
    END SUBROUTINE READ_LATLONINT


!C----------------------------------------------------------------------------------------------
    SUBROUTINE READ_WTHNC(FLNC,IVAR)
!
    USE HIGHT_VARS
    USE MULTI_VARS
    USE IO_VARS
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'
    REAL, DIMENSION (:,:,:,:), ALLOCATABLE :: WTHIN
    REAL :: RNCSCALE,RNCOFFSE,RNCMISSV,TEST(MAXLATLONIN)
    INTEGER :: ISTAT,IVAR,MLOC(1),ITIMOFF,I,J,ITS(1)
    INTEGER :: NCID,LONID,LATID,TIMID,ID,NTIMEWTH!,SURID
    CHARACTER*50 :: FLNC
    INTEGER, DIMENSION(4) :: ISTART,ICOUNT
    !print*,flnc,iyr
    !Initialise
    RLONMAT(:)=-1.0*RNCNODATA
    RLATMAT(:)=-1.0*RNCNODATA
    RTIMMAT(:)=-1.0*RNCNODATA
!    RSURMAT(:)=-1.0*RNCNODATA

    !Open the file and relevant arrays
    ISTAT=NF_OPEN(FLNC,0,NCID)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(1,ISTAT,IINFFIL,FLNC)
    ISTAT=NF_INQ_VARID(NCID,'longitude',LONID)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(2,ISTAT,IINFFIL,FLNC)
    ISTAT=NF_INQ_VARID(NCID,'latitude',LATID)
!    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(3,ISTAT,IINFFIL,FLNC)
!    ISTAT=NF_INQ_VARID(NCID,'surface',SURID)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(4,ISTAT,IINFFIL,FLNC)
    ISTAT=NF_INQ_VARID(NCID,'t',TIMID)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(5,ISTAT,IINFFIL,FLNC)
    IF (IVAR==2) THEN
       ISTAT=NF_INQ_VARID(NCID,'temp',ID)
       IF(ISTAT.NE.NF_NOERR) ISTAT=NF_INQ_VARID(NCID,'temp',ID)
       IF(ISTAT.NE.NF_NOERR) CALL NCERROR(6,ISTAT,IINFFIL,FLNC)
    ELSEIF (IVAR==3) THEN
       ISTAT=NF_INQ_VARID(NCID,'temp',ID)
       IF(ISTAT.NE.NF_NOERR) ISTAT=NF_INQ_VARID(NCID,'temp',ID)
       IF(ISTAT.NE.NF_NOERR) CALL NCERROR(7,ISTAT,IINFFIL,FLNC)
    ELSEIF(IVAR==4) THEN
       ISTAT=NF_INQ_VARID(NCID,'precip',ID)
       IF(ISTAT.NE.NF_NOERR) ISTAT=NF_INQ_VARID(NCID,'precip',ID)
       IF(ISTAT.NE.NF_NOERR) CALL NCERROR(8,ISTAT,IINFFIL,FLNC)
    ELSEIF(IVAR==1) THEN
       ISTAT=NF_INQ_VARID(NCID,'field203',ID)
       IF(ISTAT.NE.NF_NOERR) ISTAT=NF_INQ_VARID(NCID,'field203',ID)
       IF(ISTAT.NE.NF_NOERR) CALL NCERROR(9,ISTAT,IINFFIL,FLNC)
    ELSEIF(IVAR==9) THEN !Land-sea mask
       ISTAT=NF_INQ_VARID(NCID,'sm',ID)
       IF(ISTAT.NE.NF_NOERR) ISTAT=NF_INQ_VARID(NCID,'sm',ID)
       IF(ISTAT.NE.NF_NOERR) CALL NCERROR(9,ISTAT,IINFFIL,FLNC)
    ELSE
       WRITE(IINFFIL,*)'SR READ_WTH: IVAR error'
    ENDIF
    ISTAT=NF_GET_VAR_REAL(NCID,LONID,RLONMAT)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(10,ISTAT,IINFFIL,FLNC)
    ISTAT=NF_GET_VAR_REAL(NCID,LATID,RLATMAT)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(11,ISTAT,IINFFIL,FLNC)
!    ISTAT=NF_GET_VAR_REAL(NCID,SURID,RSURMAT)
!    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(12,ISTAT,IINFFIL,FLNC)
    ISTAT=NF_GET_VAR_REAL(NCID,TIMID,RTIMMAT)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(13,ISTAT,IINFFIL,FLNC)


    !Determine input grid. Note that the following could have been used instead
        !ISTAT=NF_INQ_DIMID(ncid,'longitude',LONID)
        !ISTAT=NF_INQ_DIMLEN(ncid,LONID,NLON)
    DO I=1,MAXLATLONIN    
       IF (RLONMAT(I)<(-1.0*TESTRNCNODATA)) THEN
          NLON=I-1 !Temporary value: total number of input lon points
          EXIT
       ENDIF
    ENDDO
    DO I=1,MAXLATLONIN    
       IF (RLATMAT(I)<(-1.0*TESTRNCNODATA)) THEN
          NLAT=I-1 !Temporary value: total number of input lat points
          EXIT
       ENDIF
    ENDDO

    !Determine which lat/lon points to read in
    DO I=1,NLON
       TEST(I)=ABS(RLONMAT(I)-RCOIN(1))
    ENDDO
    ITS=MINLOC(TEST(1:NLON))
    ISTART(1)=ITS(1)
    DO I=1,NLON
       TEST(I)=ABS(RLONMAT(I)-RCOIN(2))
    ENDDO
    ITS=MINLOC(TEST(1:NLON))
    ICOUNT(1)=ITS(1)-ISTART(1)+1 
    IF (RLONMAT(ISTART(1))<(-1.0*TESTRNCNODATA).OR.RLONMAT(ITS(1))<(-1.0*TESTRNCNODATA)) THEN       
       WRITE(IINFFIL,*)'Input longitude not found ',ISTART(1),RLONMAT(ISTART(1)),ITS(1),RLONMAT(ITS(1))
       DO I=1,NLON
          WRITE(IINFFIL,*),RLONMAT(I),TEST(I)
       ENDDO
    ENDIF
    DO I=1,NLAT
       TEST(I)=ABS(RLATMAT(I)-RCOIN(4))
    ENDDO
    ITS=MINLOC(TEST(1:NLAT))
    ISTART(2)=ITS(1)
    DO I=1,NLAT
       TEST(I)=ABS(RLATMAT(I)-RCOIN(3))
    ENDDO
    ITS=MINLOC(TEST(1:NLAT))
    ICOUNT(2)=ITS(1)-ISTART(2)+1 
    IF (RLATMAT(ISTART(1))<(-1.0*TESTRNCNODATA).OR.RLATMAT(ITS(1))<(-1.0*TESTRNCNODATA)) THEN       
       WRITE(IINFFIL,*)'Input Latitude not found ',ISTART(2),RLATMAT(ISTART(2)),ITS(1),RLATMAT(ITS(1))
       DO I=1,NLAT
          WRITE(IINFFIL,*),RLATMAT(I),TEST(I)
       ENDDO
    ENDIF

    !Write info to file
    IF (IVAR==9) WRITE(IINFFIL,*)'Land-sea mask read in from ',FLNC
    IF (IASCII==3.OR.IVAR==9) THEN
       WRITE(IINFFIL,*)NLON,NLAT,' lon and lat points read from NetCDF file ',FLNC
       WRITE(IINFFIL,*)'lon points ',ISTART(1), 'to ', ICOUNT(1)+ISTART(1)-1,' to be read in'
       WRITE(IINFFIL,*)'lat points ',ISTART(2), 'to ', ICOUNT(2)+ISTART(2)-1,' to be read in'
       WRITE(IINFFIL,*)'Corresponding to ',RLONMAT(ISTART(1)),RLONMAT(ISTART(1)+ICOUNT(1)-1),RLATMAT(ISTART(2)+ICOUNT(2)-1),RLATMAT(ISTART(2))
    ENDIF

    MLOC=MINLOC(RLONMAT) !AJC-L4
    IF (RLONMAT(MLOC(1))<-1.*TESTRNCNODATA) THEN
       NLON=ICOUNT(1)
       RLONMAT(1:ICOUNT(1))=RLONMAT(ISTART(1):ISTART(1)+ICOUNT(1))
       RLONMAT(ICOUNT(1)+1:)=RNCNODATA
    ELSE
       WRITE(IINFFIL,*)'MAXLON is too small for this netcdf file'
       STOP
    ENDIF
    MLOC=MINLOC(RLATMAT)
    IF (RLATMAT(MLOC(1))<-1.*TESTRNCNODATA) THEN
       NLAT=ICOUNT(2)
       RLATMAT(1:ICOUNT(2))=RLATMAT(ISTART(2):ISTART(2)+ICOUNT(2))
       RLATMAT(ICOUNT(2)+1:)=RNCNODATA
    ELSE
       WRITE(IINFFIL,*)'MAXLAT is too small for this netcdf file'
       STOP
    ENDIF
    MLOC=MINLOC(RTIMMAT)
    IF (RTIMMAT(MLOC(1))<-1.*TESTRNCNODATA) THEN
       NTIMEWTH=MLOC(1)-1
    ELSE
       WRITE(IINFFIL,*)'MAXTIM is too small for this netcdf file'
       STOP
    ENDIF

    !Spatial
!   ISTART(1)=1
!   ICOUNT(1)=NLON
!   ISTART(2)=1
!   ICOUNT(2)=NLAT
    ISTART(3)=1
    ICOUNT(3)=1 !For QUMP17 runs, this is surface
    !Temporal
    ISTART(4)=1
    ICOUNT(4)=NTIMEWTH
    !Allocate array
    ALLOCATE(WTHIN(NLON,NLAT,1,NTIMEWTH))
    WTHIN=RNCNODATA
    !Read in data for simulation region
    ISTAT=NF_GET_VARA_REAL(NCID,ID,ISTART,ICOUNT,WTHIN)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(14,ISTAT,IINFFIL,FLNC)
    !Put it in the right place
    ITIMOFF=31 !UM year begins 1st Dec; QUMP runs use 30-day months, so day 31 is 1st Jan
    IF (IVAR<5) THEN
       DO I=1,NLON
          DO J=1,NLAT
             WTH(J,I,IYR-ISYR+1,1:NTIMEWTH-ITIMOFF+1,IVAR) =  WTHIN(I,J,1,ITIMOFF:NTIMEWTH) 
          ENDDO
       ENDDO
    ELSE !Land-sea mask
       DO I=1,NLON
          DO J=1,NLAT
             RLSMASK(J,I)=WTHIN(I,J,1,1)
          ENDDO
       ENDDO
    ENDIF

    DEALLOCATE(WTHIN)
    !Read in missing_value, scale factor and offset    
!    ISTAT=NF_GET_ATT_REAL(NCID,ID,'scale_factor',RNCSCALE)
!    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(16,ISTAT,IINFFIL,FLNC)
!    ISTAT=NF_GET_ATT_REAL(NCID,ID,'add_offset',RNCOFFSE)
!    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(17,ISTAT,IINFFIL,FLNC)
!    ISTAT=NF_GET_ATT_REAL(NCID,ID,'missing_value',RNCMISSV)
!    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(18,ISTAT,IINFFIL,FLNC)

    !Close file
    ISTAT=NF_CLOSE(NCID)
    IF(ISTAT.NE.NF_NOERR) CALL NCERROR(20,ISTAT,IINFFIL,FLNC)

   
  END SUBROUTINE READ_WTHNC
!C----------------------------------------------------------------------------------------------
  

    SUBROUTINE NCERROR(IDONTHINKSOBOB,I,IF,FLNC)
      IMPLICIT NONE
      INCLUDE 'netcdf.inc'    
      INTEGER :: I,IF,IDONTHINKSOBOB
      CHARACTER:: FLNC*50
      WRITE(IF,*)'NetCDF read error at ',IDONTHINKSOBOB
      WRITE(IF,*)NF_STRERROR(I)
      WRITE(IF,*)'File: ',FLNC
      STOP
    END SUBROUTINE NCERROR


!C----------------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------------
! INITIALISATION SRS: init_glam, init_vars, and assign_vals and read_error  
!--------------------------------------------------------------------------------------------

      SUBROUTINE INIT_GLAM(POT)
!C    Initialiser for glam.f90; initialisation at the highest level
!C    Coding began 10/4/01 by AJC
!C
!C    ------------------------------------------------------------------
!C    Input     : 
!C    In        : Param file
!C    Out       :
!C    Output    : Crop parameters in three arrays (II CI RI), global initial conditions
!C    Local     : See declarations below
!C    Called by : glam
!C    Calls     : assign_vals, read_error (included)
!C    ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      USE MULTI_VARS
      USE IO_VARS      
      IMPLICIT NONE
      INTEGER :: I,JJ,J,JR,JI,JC,KK,IOS
      CHARACTER*80 :: INFFIL
	  CHARACTER :: PARFILE*60,INTYPE*150,TEXT*30
      !CHARACTER :: FNAMESFILE*30,PARFILE*50,INTYPE*150,TEXT*30 !Kathryn Nicklin changed PARFILE from length 40 to length 50
      INTEGER, PARAMETER :: NHEAD=15  !Number of paramter headers in param file
      INTEGER :: NVAR(NHEAD) !Number of vars under each param file heading
      CHARACTER :: HEADER(NHEAD)*30 !Param file header names
      CHARACTER :: STRING*14
      CHARACTER :: CINP(4)*4
      LOGICAL :: POT
      DATA INTYPE/'CCIIIIRRRRIIRRRRRRRRRRRRRRRRRRRRRRRIRRRRRRRRRRRRRRRRRRRRRRRRRRRRRIIIRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR&
&IIIIIIIIIRRRRRRRRRIIIIIIIIIIIIIIIIIIRRRRRRRRR'/
      DATA HEADER/'*MODEL MANAGEMENT',&
           '*SPATIAL MANAGEMENT AND LAI', &
           '*SOIL SPATIAL PARAMS',&
           '*DRAINAGE AND UPTAKE',&
           '*EVAPORATION AND TRANSPIRATION',&
           '*BIOMASS',&
           '*PHENOLOGY',&
           '*INTELLIGENT SOWING', &
           '*ADDITIONAL VARIABLES CURRENTL', &
           '*ADDITIONAL WHEAT (SPRING AND ', &
           '*ADDITIONAL WINTER WHEAT VARIA', &
           '*ADDITIONAL MAIZE VARIABLES', &
           '*ADDITIONAL RICE VARIABLES', &
           '*SPARE INTEGER VARIABLES',&
           '*SPARE REAL VARIABLES'/
      DATA NVAR/7,6,8,5,6,3,17,1,17,2,24,18,18,9,9/
      CHARACTER*11 :: CRLIST(NR),CILIST(NI),CCLIST(NC)
      DATA CRLIST(1:21)/'ASWS','DLDTMX','SHF_CTE','EXTC','SWF_THRESH','RLL','DUL','SAT','EFV',&
           &'E_DEPTH','SMCT_FACT','ALBEDO','RKCTE','D3','UPCTE','DLDLAI','UPDIFC','RLVEF',&
           &'EC_CTE','R_THRESH','CRIT_LAI_T'/
      DATA CRLIST(22:60)/'P_TRANS_MAX','VPD_CTE','VPD_REF','TE','DHDT','TEN_MAX','GCPLFL',&
           &'TBFLWR','TOFLWR','TMFLWR','GCFLPF',&
           &'TBPODF','TOPODF','TMPODF','GCPFLM','TBLMAX','TOLMAX','TMLMAX','GCLMHA','TBHARV',&
           &'TOHARV','TMHARV','FSWSOW','SWFF_THR','TENFAC','B_TE','B_TEN_MAX'&
           &,'TCRITMIN','PPCRIT','TLINT','TCSLOPE','TLSLOPE','FDWIDTH','FDOFFSET',&
           &'TLIMMIN','TETR1','TETR2','GCPFEN','GCENHA'/
      DATA CRLIST(61:76)/'GCPLTS','TBPLTS','TOPLTS','TMPLTS','GCTSFL','TBTSFL','TOTSFL',&
           &'TMTSFL','WWSPA1','RLAIFLT','EN','VR','FTDRFAC','FTTSFAC','VS','PS'/
      DATA CRLIST(77:84)/'WWSPA2','WWSPA3','WWSPA4','WWSPA5','WWSPA6','WWSPA7','WWSPA8','WWSPA9'/
      DATA CRLIST(85:93)/'RMAIZE1','RMAIZE2','RMAIZE3','RMAIZE4','RMAIZE5','RMAIZE6','RMAIZE7','RMAIZE8','RMAIZE9'/
      DATA CRLIST(94:102)/'RRICE1','RRICE2','RRICE3','RRICE4','RRICE5','RRICE6','RRICE7','RRICE8','RRICE9'/
      DATA CRLIST(103:111)/'RSPARE1','RSPARE2','RSPARE3','RSPARE4','RSPARE5','RSPARE6','RSPARE7','RSPARE8','RSPARE9'/
      DATA CILIST(1:10)/'NSL','I_R','I_E','ISDAY','IPDATE','IHDATE','IEMDAY','IDURMAX','IBAMAX','IAAMAX'/
      DATA CILIST(11:19)/'IMAIZE1','IMAIZE2','IMAIZE3','IMAIZE4','IMAIZE5','IMAIZE6','IMAIZE7','IMAIZE8','IMAIZE9'/
      DATA CILIST(20:28)/'IRICE1','IRICE2','IRICE3','IRICE4','IRICE5','IRICE6','IRICE7','IRICE8','IRICE9'/
      DATA CILIST(29:37)/'ISPARE1','ISPARE2','ISPARE3','ISPARE4','ISPARE5','ISPARE6','ISPARE7','ISPARE8','ISPARE9'/
      DATA CCLIST/'SEASON','MODE'/

!C--------------------------------------------
!C    FIXED MODEL PARAMETERS
!C--------------------------------------------
                            
!  I/O files
!      IINFFIL=6  !Information output file no; 6=screen, 600 for file; see line 190
      INFFIL='glam.inf'   !Information output file name; only used if IINFFIL=600
      FLDAILY='glam.out' !daily output file name
      !FNAMESFILE='filenames.txt'  !List of input files

!  Other fixed stuff
      ZSMIN=0.    !Uppermost soil level used in uptake calculations
!      ZSMAX=252.  !Depth of soil in cm. Expanded from 210 for winter wheat, but dz kept the same (8.4cm)
      D1=2.96     !Drainage param 
      D2=-2.62    !Drainage param 
!      SMLON=82.5  !Standard meridian long for India, used for high temperature stress 
!      TIME_DIFF=5.5 !Also used for high temperature stress

!C  Model options 
      I_C=0 !Method of partitioning. Usually 0
      I_P=3 !Meth of det. of trans below the evap layer (TRANS2). Used for I_C=1,2 or 3; CMH4
!      SLA_INI=300. !SLA is held to this value for the first
!      NDSLA=5     ! NDSLA days of non-zero biomass. See also subroutine LAI.

!C--------------------------------------------
!C    READ INPUT FILENAMES
!C--------------------------------------------
      OPEN(UNIT=IFLPAR,FILE=FNAMESFILE,STATUS='OLD')
      READ(IFLPAR,'(A50)')PARFILE !Kathryn Nicklin changed all these formats from A40 to A50
      READ(IFLPAR,'(A50)')WTHROOT !to allow for longer filenames
      READ(IFLPAR,'(A50)')FLSTYP
      READ(IFLPAR,'(A50)')FLSOL
      READ(IFLPAR,'(A50)')FLSOW      
      READ(IFLPAR,'(A50)')FLYLD 
      READ(IFLPAR,'(A50)')FLYGP
      CLOSE(IFLPAR)

!C--------------------------------------------
!C    READ IN AND CHECK VARIABLE PARAMS
!C--------------------------------------------

      OPEN(UNIT=IFLPAR,FILE=PARFILE,STATUS='OLD',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE(IINFFIL,*)'Incomprehensible paramter file: ',PARFILE
         CLOSE(IINFFIL)
         STOP
      ENDIF
      READ(IFLPAR,*)     
      READ(IFLPAR,100)RI(0)%NAME, RI(0)%VAL, RI(0)%METH, RI(0)%MIN, RI(0)%MAX, RI(0)%NVAL  ! Yield gap paramter first
      IF (RI(0)%VAL.LT.0.AND.RI(0)%VAL.GT.-1.) THEN
         WRITE(IINFFIL,*)'YGP value error; Possibly a decimal point missing in param file value of YGP'
         CLOSE(IINFFIL)
         STOP
      ENDIF
      READ(IFLPAR,'(12X,A3,8X,I1,6X,I1,8X,I1,8X,I1)') YGP_METH,MMNO,IMERF,ISHF,IUPT ! The yield gap reduction method; 2 * merit inputs; ISHF and IUPT for GLAM-R2
      IF (ISHF>1) THEN
         WRITE(IINFFIL,*)'ISHF out of bounds (0 or 1)'
         STOP
      ENDIF
      IF (IUPT>1) THEN
         WRITE(IINFFIL,*)'IUPT out of bounds (0 or 1)'
         STOP
      ENDIF
      ! SLA is held to SLA_INI for first NDSLA days (NDSLA.LE.0 turns of SLA-control)
      READ(IFLPAR,'(12X,I3,8X,F5.1,2X,F5.1,4X,F5.1)') NDSLA,SLA_INI,ZSMAX,SMLON   

      ! High-temperature-TE-reduction-switch, crop type, IVMETH and CO2 (current or elevated)
      READ(IFLPAR,'(12X,F6.2,5X,A14,2X,I3,5X,I3)') TETRS,STRING,IVMETH,ICO2
      READ(IFLPAR,100)
      CROP=TRIM(STRING)

      ! The rest of the global parameters
      JR=0
      JI=0
      JC=0
      JJ=0
      DO I=1,NHEAD
         READ(IFLPAR,'(A30)')TEXT
         IF (TEXT.NE.HEADER(I)) CALL READ_ERROR(JJ,1,TEXT,HEADER(I),0)
         READ(IFLPAR,*)
         DO J=1,NVAR(I)
            JJ=JJ+1
            IF (INTYPE(JJ:JJ).EQ.'R') THEN
               JR=JR+1
               READ(IFLPAR,100,ERR=5) & 
                    RI(JR)%NAME, RI(JR)%VAL, RI(JR)%METH, RI(JR)%MIN, RI(JR)%MAX, RI(JR)%NVAL
               IF (TRIM(RI(JR)%NAME).NE.TRIM(CRLIST(JR))) CALL READ_ERROR(JJ,5,RI(JR)%NAME,CRLIST(JR),0)
            ELSE IF (INTYPE(JJ:JJ).EQ.'I') THEN
               JI=JI+1
               READ(IFLPAR,200,ERR=5) & 
                    II(JI)%NAME, II(JI)%VAL, II(JI)%METH, II(JI)%MIN, II(JI)%MAX, II(JI)%NVAL
               IF (TRIM(II(JI)%NAME).NE.TRIM(CILIST(JI))) CALL READ_ERROR(JJ,5,II(JI)%NAME,CRLIST(JI),0)
            ELSE IF (INTYPE(JJ:JJ).EQ.'C') THEN
               JC=JC+1
               READ(IFLPAR,300,ERR=5) CI(JC)%NAME, CI(JC)%VAL, CI(JC)%METH, CI(JC)%MIN, CI(JC)%MAX, CI(JC)%NVAL
               IF (TRIM(CI(JC)%NAME).NE.TRIM(CCLIST(JC))) CALL READ_ERROR(JJ,5,CI(JC)%NAME,CCLIST(JC),0)
            ELSE
               CALL READ_ERROR(JJ,3,INTYPE(JJ:JJ), 'R, I or C',0)
            ENDIF
         ENDDO
         READ(IFLPAR,*)
      ENDDO

100   FORMAT(A11,1X,F10.3,1X,A1,6X,2(F8.3,1X),I3)
200   FORMAT(A11,1X,I6,5X,A1,6X,2(I6,3X),I3)
300   FORMAT(A11,1X,A8,3X,A1,6X,2(I6,3X),I3)
      IF (NI.NE.JI.OR.NR.NE.JR.OR.NC.NE.JC) CALL READ_ERROR(-99,4,'           ','           ',0)

      CLOSE(IFLPAR)

!C--------------------------------------------
!C    READ IN MODES AND ASSIGN VARIABLE PARAMS
!C-------------------------------------------- 

      IF (CI(1)%NVAL==1) THEN
         IINFFIL=6
      ELSE
         IINFFIL=600
         OPEN(UNIT=IINFFIL,FILE=INFFIL)      
      ENDIF
      IF (CI(2)%NVAL==1) THEN
         LHIGHT=.TRUE.
      ELSE
         LHIGHT=.FALSE.
      ENDIF

      !Run modes and spare input channels
      ISYR=CI(1)%MIN    
      IEYR=CI(1)%MAX     
      INETCDF=CI(2)%MIN      
      IASCII=CI(2)%MAX      
      OUTSCALE=CI(2)%METH !Spatial scale of output; also used in calc of merit fn
      IF (TRIM(CI(1)%VAL)=='RFD') THEN
         POT=.FALSE.  !Rainfed run
      ELSEIF (TRIM(CI(1)%VAL)=='IRR') THEN
         POT=.TRUE. ! (fully) irrigated run
      ELSE
         WRITE(IINFFIL,*)'Incomprehensible SEASON input'
         CLOSE(IINFFIL)
         STOP
      ENDIF
      MODE=CI(2)%VAL
      WRITE(IINFFIL,*)'CROP (RunID) = ',CROP
      WRITE(IINFFIL,*)'SEASON= ',TRIM(CI(1)%VAL)
      WRITE(IINFFIL,*)'MODE= ',MODE
      RUNSCALE=CI(1)%METH !Spatial scale on which model is run
      IF (RUNSCALE.NE.'b') THEN
         WRITE(IINFFIL,*) 'Incomprehensible spatial run scale. Termination.'
         CLOSE(IINFFIL)
         STOP
      ENDIF
      IF (OUTSCALE.NE.'b') THEN
         WRITE(IINFFIL,*) 'Incomprehensible spatial output scale. Termination.'
         CLOSE(IINFFIL)
         STOP
      ENDIF
      IF (YGP_METH.NE.'ASW'.AND.YGP_METH.NE.'EOS'.AND.YGP_METH.NE.'LAI'.AND.YGP_METH.NE.'DUL'.AND.YGP_METH.NE.'SOL') THEN
         WRITE(IINFFIL,*)'YGP_METH value is incomprehensible'
         CLOSE(IINFFIL)
         STOP
      ENDIF
      IF (TRIM(RI(0)%NAME).NE.'YGP') CALL READ_ERROR(0,5,TRIM(RI(0)%NAME),'YGP',0)
      IF (II(KISDAY)%VAL<0) THEN  !Intelligent planting
         LINT=.TRUE.
      ELSE
         LINT=.FALSE.
      ENDIF

      !Set SLA control on if chosen
      IF (NDSLA>0)THEN
         IF (SLA_INI<0.) THEN
            WRITE(IINFFIL,*)'Invalid SLA_INI value'
            CLOSE(IINFFIL)
            STOP
         ENDIF
         LSLA=.TRUE.
         WRITE(IINFFIL,*)'SLA-control turned on'
      ELSE
         LSLA=.FALSE.
         WRITE(IINFFIL,*)'SLA-control turned off'  
      ENDIF

      IF (IASCII==3) WRITE(IINFFIL,*)'YGP= ',RI(0)%VAL

      !Spatial initialisation
      JSKIP(1:MAXLAT,1:MAXLON)=0
      STORE=RNCNODATA
      IPSAVE(:,:,:,:)=NODATA !Added by Kathryn Nicklin so no data values are NODATA rather than zeros
      ISTGSAVE(:,:,:,:)=NODATA !Added by Kathryn Nicklin so no data values are NODATA rather than zeros

!C--------------------------------------------

      RETURN
5     CALL READ_ERROR(JJ,2,'            ','            ',I)
8     CALL READ_ERROR(KK,8,'            ','            ',0)
      IF (IINFFIL.NE.6) CLOSE(IINFFIL)
      STOP


      
      END SUBROUTINE INIT_GLAM
!C--------------------------------------------





      SUBROUTINE INIT_RUN(ASWS,IHDATE,E_DEPTH,IHYP)
!C     Calculates parameters for single model run.
!C
!C     ------------------------------------------------------------------
!C     Input     : II,RI,CI
!C     In        : 
!C     Out       : 
!C     Output    : Full set of model params as determined by param file
!C     Local     : 
!C     Called by : glam
!C     Calls     : 
!C     ------------------------------------------------------------------
      USE GLOBAL_VARS
      USE MULTI_VARS
      USE IO_VARS
      IMPLICIT NONE
      INTEGER :: IHDATE,ID,IHYP
      REAL :: E_DEPTH,ASWS,RLATRAD
      REAL :: YR,EQNT,DEC,SHASS,SUNRISE,SUNSET,TAMP,DELT,DEF_INT

      !--------------------------------------------
      !   SET WEATHER
      !--------------------------------------------
      SRAD=WTH(ILAT,ILON,IYR-ISYR+1,:,1)
      TMAX=WTH(ILAT,ILON,IYR-ISYR+1,:,2)
      TMIN=WTH(ILAT,ILON,IYR-ISYR+1,:,3)
      ! this is to get some HTS during ISTG=2 but only for a short period of time (<=10days)
      TMAX(26:30)=TMAX(26:30)+40.
      ! TMAX(27:31)=TMAX(27:31)+40.
      ! TMAX(30:34)=TMAX(30:34)+40.
      TMAX(41:45)=TMAX(41:45)+40.
      ! TMAX(60:65)=TMAX(60:65)+40.
      ! TMAX(60:63)=TMAX(60:63)+40.
      TMIN(26:30)=TMIN(26:30)+40.
      ! TMIN(27:31)=TMIN(27:31)+40.
      ! TMIN(30:34)=TMIN(30:34)+40.
      TMIN(41:45)=TMIN(41:45)+40.
      ! TMIN(60:65)=TMIN(60:65)+40.
      ! TMIN(60:63)=TMIN(60:63)+40.

      RAIN=WTH(ILAT,ILON,IYR-ISYR+1,:,4)
      TBAR=(TMAX+TMIN)/2.
      RLAT=RLATMAT(ILAT)
      RLON=RLONMAT(ILON)

      !Calculate TAM temperature for high-T stress calc
      DO ID=1,NDAYSYR
         IF (TMIN(ID)>TESTNODATA.AND.TMAX(ID)>TESTNODATA) THEN
            YR=2.*PI/365.*FLOAT(ID-1) !"Fractional year" in radians
            RLATRAD=RLAT*PI/180. !Lat in rad
            EQNT=229.18/60.* & !"Equation of time" in hours
                 (0.000075+0.001868*COS(YR)-0.032077*SIN(YR)-0.014615*COS(2*YR)-0.040849*SIN(2*YR))
            DEC=0.006918-0.399912*COS(YR)+00.070257*SIN(YR)-0.006758*COS(2.*YR) + &
                 0.000907*SIN(2.*YR)-0.002697*COS(3.*YR)+0.00148*SIN(3.*YR)
            SHASS=ACOS(-1.*TAN(RLATRAD)*TAN(DEC)) !Sunrise/sunset hour angle (zero at local solar noon)
            SUNRISE=12. - 12./PI*SHASS !Noon minus [time from daybreak to noon]; solar time
            SUNRISE=SUNRISE + (SMLON-RLON)*24./360. - EQNT !Convert to local time and correct for EQNT
            SUNSET= 12. + 12./PI*SHASS !Noon plus [time from noon to sunset]; solar time
            SUNSET= SUNSET  + (SMLON-RLON)*24./360. - EQNT !Convert to local time and correct for EQNT
            TAMP=TMAX(ID) - TMIN(ID)
            DELT=SUNSET-SUNRISE               
            DEF_INT=SIN(PI*(12.-DELT/2.-SUNRISE)/DELT) - SIN(PI*(6.-DELT/2.-SUNRISE)/DELT)
            TAM(ID)=TBAR(ID) + TAMP*DELT/(12.*PI) * DEF_INT
         ELSE
            TAM(ID)=FLOAT(NODATA)
         ENDIF
      ENDDO
      !Determine NWTH         
      NWTH=NWTHMAT(ILAT,ILON,IYR-ISYR+1)
      IF (NWTH==0) THEN
         WRITE(IINFFIL,*)'SR INIT_RUN: Missing wth data at start of record'
         WRITE(IINFFIL,*)'SR INIT_RUN: Planting date is ',IPDATEMAT(ILAT,ILON)
         CLOSE(IINFFIL)
         STOP
      ELSE
         IF (IASCII==3) WRITE(IINFFIL,*) NWTH, 'days of wth data'
      ENDIF
     
      !--------------------------------------------
      !   INITIALISE ACCORDING TO MODE (SET OR HYP)
      !--------------------------------------------
      IF (MODE.EQ.'SET') THEN
         IF(LREPLANT)THEN !Kathryn Nicklin
         !if we are replanting don't open the daily output files again
         ELSE
          IF (IASCII.GE.2) OPEN(UNIT=IFLDAILY,FILE=FLDAILY,STATUS='UNKNOWN')
         ENDIF
      ELSE IF (MODE.EQ.'HYP') THEN
         IF (LNEWSET) THEN
            IF (NHYP==MAXNVR) THEN
               VALUE=RI(KNRUNS)%MIN+FLOAT(IHYP-1)*R_INTER
               RI(KNRUNS)%VAL=VALUE
               IF (RI(KNRUNS)%NAME(1:3)=='RLL') THEN
                  RLLMAT(:,:)=RLLMAT(:,:)+R_INTER
               ELSE IF (RI(KNRUNS)%NAME(1:3)=='DUL') THEN
                  DULMAT(:,:)=DULMAT(:,:)+R_INTER
               ELSE IF (RI(KNRUNS)%NAME(1:3)=='SAT') THEN 
                  SATMAT(:,:)=SATMAT(:,:)+R_INTER
               ELSE
                  IF (TBFLAG) THEN
                     RI(KTB(:))%VAL=VALUE
                  ELSE IF (TOFLAG) THEN
                     RI(KTO(:))%VAL=VALUE
                  ELSE IF (TMFLAG) THEN
                     RI(KTM(:))%VAL=VALUE
                  ENDIF
               ENDIF
            ELSE IF (NHYP==MAXNVI) THEN
!               IF(II(KNRUNS)%NAME(1:5)=='IPDAT') THEN !This option not available
!                  IVALUE=I_INTER
!                  IPDATEMAT(:,:)=IVALUE
!               ELSE
                  IVALUE=II(KNRUNS)%MIN+(IHYP-1)*I_INTER 
                  II(KNRUNS)%VAL=IVALUE  
!               ENDIF
            ELSE IF (NHYP==MAXNVC) THEN
               IF (KNRUNS.NE.2) THEN
                  WRITE(IINFFIL,*)'Error in param file: character variable to be varied is not variable'
                  CLOSE(IINFFIL)
                  STOP
               ENDIF
            ELSE
               WRITE(IINFFIL,*)'Error in HYP INTER'
            ENDIF
            IF (NHYP==MAXNVR) THEN             
               WRITE(IINFFIL,*)'HYP: ',RI(KMAXNVR)%NAME//' ',RI(KMAXNVR)%VAL
               IF (TBFLAG.OR.TOFLAG.OR.TMFLAG.OR.GCFLAG) THEN
                  WRITE(IINFFIL,*)'HYP: Other variables co-varying with variable above'
               ENDIF
            ELSE IF (NHYP==MAXNVI) THEN
               WRITE(IINFFIL,*)'HYP: ',II(KMAXNVI)%NAME//' ',II(KMAXNVI)%VAL
            ENDIF
            LNEWSET=.FALSE.
         ENDIF
      ENDIF

      !--------------------------------------------
      !   SET SOIL  AND PLANTING DATE
      !--------------------------------------------
      !Set Soils and planting date     
      IPDATE=IPDATEMAT(ILAT,ILON)
      RLL=RLLMAT(ILAT,ILON)
      DUL=DULMAT(ILAT,ILON)
      SAT=SATMAT(ILAT,ILON)
     
5     CONTINUE
      

      CALL ASSIGN_VALS(ASWS,IHDATE,E_DEPTH)

!c    following lines adding by Kathryn Nicklin 12 February 2009 to introduce another calibration option

      IF(YGP_METH =='DUL') THEN
      DUL=DUL-(1.0-YGP)*(DUL-RLL)
      ENDIF

!     Kathryn Nicklin. If the soil method of calibration is being used then in the param file 
!     the value of SAT doesn't matter,
!     the value of DUL is varied between the value in the param file and the value of RLL which should be set 
!     to zero in the param file. 
      IF(YGP_METH == 'SOL') THEN
      DUL=DUL-(1.0-YGP)*(DUL-RLL)
      SAT = 0.25352 +0.78717*DUL
      ENDIF

!      PRINT*,'YGP,RLL,DUL,SAT:',YGP,RLL,DUL,SAT

!c    Kathryn Nicklin. following section moved to here from beneath 'set soils and planting date' SAT = SATMAT (a few lines above)
      IF (DUL.GE.SAT.OR.RLL.GE.DUL) THEN
         WRITE(IINFFIL,*)'Soils problem! Aborting.'
         WRITE(IINFFIL,*)'RLL, DUL, SAT= ',RLL,DUL,SAT
         CLOSE(IINFFIL)
         STOP
       ENDIF

!c    to check correct soils data is being used:

!      IF(MODE=='HYP')THEN
!        OPEN(UNIT=98,FILE='output/soilsdata.out',STATUS='UNKNOWN',ACCESS='APPEND')
!        WRITE(98,*) YGP, RLL, DUL, SAT
!        CLOSE(98)
!       ENDIF

      CALL INIT_VARS(ASWS,IHDATE,E_DEPTH)       
    
      SAVEMERIT(1,IHYP)=YGP

      RETURN
      END SUBROUTINE INIT_RUN




      SUBROUTINE INIT_VARS(ASWS,IHDATE,E_DEPTH)
!C    Initialises variables for a single model run
!C
!C    ------------------------------------------------------------------
!C    Input     : Crop variables and params
!C    In        : 
!C    Out       :
!C    Output    : Crop variables and params, initial conditions
!C    Local     : See declarations below
!C    Called by : glam, sens
!C    Calls     : TTCALC
!C    ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      USE WWINTER_VARS
      USE IO_VARS
      IMPLICIT NONE
      INTEGER :: IZ,IHDATE,I
      REAL :: ASWS, E_DEPTH

!C--------------------------------------------
!C    INITIALISATION AND PARAMETER CHECKS
!C--------------------------------------------

!C Soil structure initialisation etc
      IF (NSL.GT.MAXNSL) THEN
         PRINT*, 'Increase MAXNSL'
         STOP
      ENDIF
      S_THICK=ZSMAX-ZSMIN !(total) Thickness of soil
      DZ=S_THICK/FLOAT(NSL) !Depth of each soil layer
      DO IZ=1,NSL
         Z(IZ)=ZSMIN+(0.5+FLOAT(IZ-1))*DZ !Z defined at layer middle
      ENDDO

!C Water Balance
      VOLSWS=RLL+ASWS*(DUL-RLL)!Vol. soil water at IPDATE
      TP_UP=0. !total potential uptake
      DPUPTK(1:NSL)=0. !initial value of potential uptake summed over latest curve. shouldn't matter whether reset or not
      TRADABS=0. !Total radiation absorbed by the canopy
      TP_EVAP=0. !Total potential evaporation
      SWCRIT=RLL+(DUL-RLL)*SWC_FAC
      IF(LREPLANT)THEN !Kathryn Nicklin
       !don't want to reset the soil water
      ELSE
       VOLSW(1:NSL)=VOLSWS !Vol. soil water at start of similation across whole soil layer
       T_DRAIN=0.!Total drainage
       T_RUN=0.  !total runoff (from start of season)
       T_RAIN=0. !Total rainfall for the season, from actual day of planting
       T_TRANS=0.!Total transpiration
       T_EVAP=0. !Total evaporation
       TRADNET=0.!Total net radiation at the surface
       C_EVAP=0. !Cumulative evap (for current evap curve)
       TP_TRANS=0.!Total potential transpiration
       E_TIME =1.  ! Time since start of current evaporation curve
      ENDIF

!C Roots
      ITEXP(:)=NODATA
      ZEF(:)=0.!To enable calc of ZEF on day ISDAY

!C Crop status
      RLAI(:)=0.!Initial LAI
      !Groundnut
      TTFLWR=0. !Thermal time from planting to first flower (count)
      TTPODF=0. !Thermal time from first flower to pod filling (count)
      TTLMAX=0. !Thermal time from pod-fill to max LAI
      TTHARV=0. !Thermal time from pod filling to harvest (count)
      !Additional wheat (spring and winter)
      VD=0
      AFT=0
      DTE=0 
      !Additional winter wheat
      TTPLTS=0. !Thermal time from planting to termal spiklet (count)
      TTTSFL=0. !thermal time from termal spiklet to flower(count)
      !General
      ISTG=0    !growth stage (see glam.f)
      HI=0.
      BMASS=0.
      YIELD=0.
      IF (IASCII==3) WRITE(IINFFIL,500)'ISDAY= ',ISDAY,'; IEMDAY= ',IEMDAY
500   FORMAT(2(A,I3))

!C Paramter checks etc
      IF (IHDATE.EQ.NODATA) THEN
         IHDAP=NODATA
      ELSE
         IHDAP=IHDATE!-IPDATE
         IF (IHDAP<0) IHDAP=IHDAP+365
      ENDIF
      NED=INT(E_DEPTH/DZ) ! No of soil layers in evap layer
      IF(NED==0) THEN
         NED=1
         WRITE(IINFFIL,*)'E_DEPTH increased to 1*DZ= ',DZ
      ENDIF
      IF(NED>NSL) THEN
         WRITE(IINFFIL,*)'NED > NSL',NED,NSL
         CLOSE(IINFFIL)
         STOP
      ENDIF

!C
!C--------------------------------------------
!C    PRE-SIMULATION CALCULATIONS
!C--------------------------------------------
!C

!  Water balance paramterisations
      D_RATE=D1*DUL**2+D2*DUL+D3 !;CMH4, c81
      IF (D_RATE<0.) THEN
         WRITE(IINFFIL,*)'Init: -VE D_RATE!! ',D_RATE
         CLOSE(IINFFIL)
         STOP
      ENDIF
      RKSAT=RKCTE*((SAT-DUL)/DUL)**2 !cm/day;CMH4, c82
      IF (IASCII==3) WRITE(IINFFIL,150)'D_RATE calculated to be ',D_RATE,'. KSAT calculated to be ',RKSAT
      UPTOL=UPCTE*(DUL-RLL)+RLL  !Vol. soil water at and above which uptake=max. value; FAO;CMH1 
      IF (I_E==1) THEN 
         EVAP_COEF=EC_CTE*DUL !CMH4, c80; I_E=1
         SMC_THRESH=SMCT_FACT*(DUL-RLL)+RLL
         IF (VOLSWS<SMC_THRESH.AND.I_E.EQ.1) THEN
            WRITE(IINFFIL,*)'SWATER < SMC_THRESH sr init'
            !         STOP
         ENDIF
         IF (SMC_THRESH.GE.SAT.AND.I_E.EQ.1) THEN
            WRITE(IINFFIL,*)'SMC_THRESH = ',SMC_THRESH,' > SAT = ',SAT
            CLOSE(IINFFIL)
            STOP
         ENDIF
         IF (SMC_THRESH>DUL.AND.I_E.EQ.1) WRITE(IINFFIL,*)'Init: Note that SMC_THRESH = ',SMC_THRESH,' >DUL = ',DUL
      ENDIF

      !TRAIN is the most recent day of rain, used in I_E=2
      IF(LREPLANT)THEN !Kathryn Nicklin
       !do not want to reset TRAIN
      ELSE
       TRAIN=0.
       WTH: DO I=ISDAY,1,-1
          TRAIN=TRAIN+1.
          IF (RAIN(I)>R_THRESH) EXIT WTH
       ENDDO WTH
      ENDIF

!  And finally...
      IF(LREPLANT)THEN !Kathryn Nicklin
       !do not reset these totals
      ELSE
       VPDTOT=0.
       TBARTOT=0.
       TMINTOT=0.
       TMAXTOT=0.
      ENDIF
      
150   FORMAT(2(A,F7.3))

!C--------------------------------------------

      RETURN
      END SUBROUTINE INIT_VARS

!C--------------------------------------------



      SUBROUTINE ASSIGN_VALS(ASWS,IHDATE,E_DEPTH)
!     ------------------------------------------------------------------
!     Input     : RI, CI II
!     In        : 
!     Out       : 
!     Output    : Full set of model params as determined by II, CI, RI
!     Local     : 
!     Called by : init_glam, sens
!     Calls     : 
!     ------------------------------------------------------------------
!     Cardinal temperatures are hard-wired in in sens_vars module
!     RLL,DUL,SAT and IPDATE are determined by pointers to matrices (DULMAT etc)
      USE GLOBAL_VARS
      USE MULTI_VARS
      USE HIGHT_VARS
      USE WSPRING_VARS
      USE WWINTER_VARS
      USE RICE_VARS
      USE MAIZE_VARS
      IMPLICIT NONE
      REAL :: ASWS,E_DEPTH
      INTEGER :: IHDATE

      IF (RI(0)%VAL<TESTNODATA) THEN
         YGP=YGPMAT(ILAT,ILON)
      ELSE         
         YGP=RI(0)%VAL
      ENDIF
      NSL=II(1)%VAL
      I_R=II(2)%VAL
      I_E=II(3)%VAL
      IF (LREPLANT) THEN !Kathryn Nicklin
       !replanting ISDAY is already saved
      ELSE
       ISDAY=II(KISDAY)%VAL !first run through, ISDAY from file
      ENDIF
      ASWS=RI(1)%VAL
      DLDTMX=RI(2)%VAL
      SHF_CTE=RI(3)%VAL
      EXTC=RI(4)%VAL
      SWF_THRESH=RI(5)%VAL
      IHDATE=II(6)%VAL
      EFV=RI(9)%VAL
      E_DEPTH=RI(10)%VAL
      SMCT_FACT=RI(11)%VAL
      ALBEDO=RI(12)%VAL
      RKCTE=RI(13)%VAL
      D3=RI(14)%VAL
      UPCTE=RI(15)%VAL
      DLDLAI=RI(16)%VAL
      UPDIFC=RI(17)%VAL
      RLVEF=RI(18)%VAL
      EC_CTE=RI(19)%VAL
      R_THRESH=RI(20)%VAL
      CRIT_LAI_T=RI(21)%VAL
      P_TRANS_MAX=RI(22)%VAL
      VPD_CTE=RI(23)%VAL
      VPD_REF=RI(24)%VAL
      TE=RI(25)%VAL
      DHDT=RI(26)%VAL
      TEN_MAX=RI(27)%VAL
      IEMDAY=II(7)%VAL
      IDURMAX=II(8)%VAL
      IBAMAX=II(9)%VAL
      IAAMAX=II(10)%VAL
      GCPLFL=RI(KGC(1))%VAL
      TBFLWR=RI(KTB(1))%VAL
      TOFLWR=RI(KTO(1))%VAL
      TMFLWR=RI(KTM(1))%VAL
      GCFLPF=RI(KGC(2))%VAL
      TBPODF=RI(KTB(2))%VAL
      TOPODF=RI(KTO(2))%VAL
      TMPODF=RI(KTM(2))%VAL
      GCPFLM=RI(KGC(3))%VAL
      TBLMAX=RI(KTB(3))%VAL
      TOLMAX=RI(KTO(3))%VAL
      TMLMAX=RI(KTM(3))%VAL
      GCLMHA=RI(KGC(4))%VAL
      TBHARV=RI(KTB(4))%VAL
      TOHARV=RI(KTO(4))%VAL
      TMHARV=RI(KTM(4))%VAL
      FSWSOW=RI(44)%VAL
      SWFF_THR=RI(45)%VAL
      TENFAC=RI(46)%VAL
      B_TE=RI(47)%VAL
      B_TEN_MAX=RI(48)%VAL
      TCRITMIN=RI(49)%VAL
      PPCRIT=RI(50)%VAL
      TLINT=RI(51)%VAL
      TCSLOPE=RI(52)%VAL
      TLSLOPE=RI(53)%VAL
      FDWIDTH=RI(54)%VAL
      FDOFFSET=RI(55)%VAL
      TLIMMIN=RI(56)%VAL
      TETR1=RI(57)%VAL
      TETR2=RI(58)%VAL
      !Additional wheat (spring and winter)
      GCPFEN=RI(59)%VAL
      GCENHA=RI(60)%VAL
      !Additional winter wheat
      GCPLTS=RI(61)%VAL
      TBPLTS=RI(62)%VAL     
      TOPLTS=RI(63)%VAL      
      TMPLTS=RI(64)%VAL      
      GCTSFL=RI(65)%VAL      
      TBTSFL=RI(66)%VAL      
      TOTSFL=RI(67)%VAL      
      TMTSFL=RI(68)%VAL      
      WWSPA1=RI(69)%VAL      
      RLAIFLT=RI(70)%VAL
      EN=RI(71)%VAL   
      VR=RI(72)%VAL    
      FTDRFAC=RI(73)%VAL
      FTTSFAC=RI(74)%VAL
      VS=RI(75)%VAL  
      PS=RI(76)%VAL   
      WWSPA2=RI(77)%VAL  
      WWSPA3=RI(78)%VAL  
      WWSPA4=RI(79)%VAL  
      WWSPA5=RI(80)%VAL  
      WWSPA6=RI(81)%VAL  
      WWSPA7=RI(82)%VAL  
      WWSPA8=RI(83)%VAL  
      WWSPA9=RI(84)%VAL  
      !MAIZE VARIABLES                  
      RMAIZE1=RI(85)%VAL 
      RMAIZE2=RI(86)%VAL 
      RMAIZE3=RI(87)%VAL 
      RMAIZE4=RI(88)%VAL 
      RMAIZE5=RI(89)%VAL 
      RMAIZE6=RI(90)%VAL 
      RMAIZE7=RI(91)%VAL 
      RMAIZE8=RI(92)%VAL 
      RMAIZE9=RI(93)%VAL 
      IMAIZE1=II(11)%VAL
      IMAIZE2=II(12)%VAL
      IMAIZE3=II(13)%VAL
      IMAIZE4=II(14)%VAL
      IMAIZE5=II(15)%VAL
      IMAIZE6=II(16)%VAL
      IMAIZE7=II(17)%VAL
      IMAIZE8=II(18)%VAL
      IMAIZE9=II(19)%VAL
      !RICE
      RRICE1=RI(94)%VAL 
      RRICE2=RI(95)%VAL 
      RRICE3=RI(96)%VAL 
      RRICE4=RI(97)%VAL 
      RRICE5=RI(98)%VAL
      RRICE6=RI(99)%VAL 
      RRICE7=RI(100)%VAL
      RRICE8=RI(101)%VAL 
      RRICE9=RI(102)%VAL
      IRICE1=II(20)%VAL
      IRICE2=II(21)%VAL
      IRICE3=II(22)%VAL
      IRICE4=II(23)%VAL
      IRICE5=II(24)%VAL
      IRICE6=II(25)%VAL
      IRICE7=II(26)%VAL
      IRICE8=II(27)%VAL
      IRICE9=II(28)%VAL
      !SPARE VARIABLES                                 
      ISPARE1=II(29)%VAL
      ISPARE2=II(30)%VAL
      ISPARE3=II(31)%VAL
      ISPARE4=II(32)%VAL
      ISPARE5=II(33)%VAL
      ISPARE6=II(34)%VAL
      ISPARE7=II(35)%VAL
      ISPARE8=II(36)%VAL
      ISPARE9=II(37)%VAL
      RSPARE1=RI(103)%VAL
      RSPARE2=RI(104)%VAL
      RSPARE3=RI(105)%VAL
      RSPARE4=RI(106)%VAL
      RSPARE5=RI(107)%VAL
      RSPARE6=RI(108)%VAL
      RSPARE7=RI(109)%VAL
      RSPARE8=RI(110)%VAL
      RSPARE9=RI(111)%VAL

      ! This section below (9 lines) added Feb09 for Terminal drought stress (!TDS)
      HIMIN=RSPARE1 
      IF (HIMIN>TESTNODATA) THEN
         DURFLAG=.TRUE.
      ELSE
         DURFLAG=.FALSE. 
      ENDIF
      SWC_FAC=RSPARE2
!     The calculation has been moved from here to INIT_VARS so that SWCRIT varies as DUL varies kathryn nicklin 13 May 2009 
!     SWCRIT=RLL+(DUL-RLL)*SWC_FAC

      !Finally     
      SLA=RNOVAL

      !Taken from v2A.201 and v2B.201. See GLUM51.
      IF (ICO2==0) THEN !current climate run
         IF (TEN_MAX<TESTNODATA) TEN_MAX=TE * B_TEN_MAX/B_TE !ie TE * baselineTEN_MAX/baselineTE
      ELSEIF (ICO2==1) THEN !Elevated CO2 value of TEN_MAX
         IF (TEN_MAX<TESTNODATA) THEN !Value of TEN_MAX is 1*CO2 value
            WRITE(IINFFIL,*)'ASSIGN_VALS: TENMAX=-99 not an option for FC run'
            CLOSE(IINFFIL)
            STOP        
         ENDIF
         IF (TENFAC>TESTNODATA) TEN_MAX=(1.-TENFAC)*B_TEN_MAX + TENFAC*TE*B_TEN_MAX/B_TE
      ENDIF
      IF (TEN_MAX<RI(27)%MIN) THEN
         WRITE(IINFFIL,*)'WARNING: TEN_MAX<MIN, ADJUSTMENT MADE',TEN_MAX
         TEN_MAX=RI(27)%MIN
      ELSEIF (TEN_MAX<RI(27)%MIN.OR.TEN_MAX>RI(27)%MAX) THEN
         WRITE(IINFFIL,*)'WARNING: TEN_MAX>MAX, ADJUSTMENT MADE',TEN_MAX
         TEN_MAX=RI(27)%MAX
      ENDIF

      !Winter wheat senescence option
      IF (WWSPA1<TESTNODATA) WWSPA1=DLDTMX

      RETURN
      END SUBROUTINE ASSIGN_VALS



      SUBROUTINE READ_ERROR(JJ,K,C1,C2,JJ2)
      USE GLOBAL_VARS
      IMPLICIT NONE
      INTEGER :: JJ,K,JJ2
      CHARACTER*11 :: C1,C2

      WRITE(IINFFIL,*)'READ ERROR:'
      IF (K==1) THEN
         WRITE(IINFFIL,*)'Header error on line ',JJ
      ELSE IF (K==2) THEN
         WRITE(IINFFIL,*)'Error reading parameter file, at line ',JJ+5+JJ2*3
      ELSE IF (K==3) THEN
         WRITE(IINFFIL,*)'Error in array INTYPE'
      ELSE IF (K==4) THEN
         WRITE(IINFFIL,*)'Real/Int/Char number mis-match'
      ELSE IF (K==5) THEN
         WRITE(IINFFIL,*)'Varible name mis-match at line',JJ
      ELSE IF (K==6) THEN
      ENDIF
      IF (MOD(K,2)>0) WRITE(IINFFIL,*)'Expected ',C2,' got ',C1

      CLOSE(IINFFIL)
      STOP
      END SUBROUTINE READ_ERROR




      SUBROUTINE INIT_MULTI
!C    Sets up multiple model runs; 
!     HYP=hypercube analsis, SET=set of single runs with daily output
!C
!C    ------------------------------------------------------------------
!C    Input     : 
!C    In        : Crop params and limits
!C    Out       :
!C    Output    : R_INTER, I_INTER
!C    Local     : 
!C    Called by : glam
!C    Calls     : 
!C    ------------------------------------------------------------------
      USE MULTI_VARS
      USE IO_VARS
      IMPLICIT NONE
!      INTEGER :: J,IC,M,K,INO,IY,ICN
!      CHARACTER*40 :: MERROOT,MERFIL
     
      THEBIGIF: IF (MODE.EQ.'SET') THEN
         WRITE(IINFFIL,*)'SET of runs: Years ',ISYR,' to ',IEYR
         NHYP=1
        
      ELSE IF (MODE.EQ.'HYP') THEN
         CALL SENSCALC(NHYP)
         IASCII=MIN(IASCII,1) !No daily output info for HYP run
!         MERROOT=TRIM(OUTDIR)//TRIM(CROP) !AJC-R2 commented out at least next 13 lines
!         IF (NHYP==MAXNVR) THEN
!            MERFIL=MERROOT//RI(KNRUNS)%NAME(1:5)//'.mer'
!         ELSE IF (NHYP==MAXNVI) THEN
!            MERFIL=MERROOT//II(KNRUNS)%NAME(1:5)//'.mer'
!         ELSE IF (NHYP==MAXNVC) THEN
!            MERFIL=MERROOT//CI(KNRUNS)%NAME(1:5)//'.mer'
!         ENDIF
!         ICN=INDEX(MERFIL,'.')
!         DO IC=1,ICN
!            IF (MERFIL(IC:IC)==' ') MERFIL(IC:IC)='x'
!         ENDDO
!         IF (IFLMER.NE.6) OPEN(UNIT=IFLMER,FILE=MERFIL)
!         PRED=FLOAT(NODATA)
!         NSPAT_OPT=N_SPAT_WTH
!         PRED=>STORE(ISYR:IEYR,1:NSPAT_OPT,3)!(3=yield; see SR STORE_VARS)            

         ! Allow Soils and planting to vary from file input baseline if necessary
         IF (NHYP==MAXNVR) THEN 
            IF (RI(KNRUNS)%NAME(1:3)=='RLL') THEN
               RLLMAT(:,:)=RLLMAT(:,:)+RI(KNRUNS)%MIN-R_INTER
            ELSE IF (RI(KNRUNS)%NAME(1:3)=='DUL') THEN
               DULMAT(:,:)=DULMAT(:,:)+RI(KNRUNS)%MIN-R_INTER
            ELSE IF (RI(KNRUNS)%NAME(1:3)=='SAT') THEN
               SATMAT(:,:)=SATMAT(:,:)+RI(KNRUNS)%MIN-R_INTER
            ENDIF
         ELSE IF (NHYP==MAXNVI) THEN
            IF(II(KNRUNS)%NAME(1:5)=='IPDAT') THEN
!               IPDATEMAT(:,:)=IPDATEMAT(:,:)+II(KNRUNS)%MIN-I_INTER
               WRITE(IINFFIL,*)'Cannot perform HYP run on IPDATE. Try ISDAY, FSWSOW or ASWS instead.'
               CLOSE(IINFFIL)
               STOP
            ENDIF
         ENDIF
         WRITE(IINFFIL,*)'This is a hypercube run for the interval ',ISYR,IEYR

      ELSE

         WRITE(IINFFIL,*) 'Incomprehensible run MODE'
         CLOSE(IINFFIL)
         STOP
         
      ENDIF THEBIGIF
      WRITE(IINFFIL,*)

!C--------------------------------------------

         RETURN
       END SUBROUTINE INIT_MULTI

!C--------------------------------------------

       SUBROUTINE SENSCALC(NN)
       USE MULTI_VARS
       USE IO_VARS
       IMPLICIT NONE
       INTEGER :: ICHECK,NFLAG,K,KK(3),NN

       ICHECK=0
       MAXNVI=-999
       MAXNVR=-999
       MAXNVC=-999
       DO K=1,NI
          IF (II(K)%NVAL.NE.NODATA) THEN
             IF (II(K)%NVAL>MAXNVI) THEN 
                MAXNVI=II(K)%NVAL
                KMAXNVI=K
             ENDIF
          ENDIF
       ENDDO
       IF (MAXNVI==1.OR.MAXNVI==-999) ICHECK=ICHECK+1
       DO K=0,NR  !YGP inserted as extra real input param at array subscript 0
          IF (RI(K)%NVAL.NE.NODATA) THEN
             IF (RI(K)%NVAL>MAXNVR) THEN 
                MAXNVR=RI(K)%NVAL
                KMAXNVR=K
             ENDIF
          ENDIF
       ENDDO
       IF (MAXNVR==1.OR.MAXNVR==-999) ICHECK=ICHECK+1
       DO K=1,NC
          IF (CI(K)%METH.EQ.'l'.AND.CI(K)%NVAL.NE.NODATA) THEN
             IF (CI(K)%NVAL>MAXNVC) THEN 
                MAXNVC=CI(K)%NVAL
                KMAXNVC=K
             ENDIF
          ENDIF
       ENDDO
       IF (MAXNVC==1.OR.MAXNVC==-999) ICHECK=ICHECK+1
       IF (ICHECK.NE.2.AND.MODE=='SEN') THEN 
          WRITE(IINFFIL,*)'Wrong number of varying variables for sensitivity analysis: ',MAXNVI,MAXNVC,MAXNVR
          CLOSE(IINFFIL)
          STOP
       ENDIF
       NFLAG=0
       DO K=1,NPHEN
          IF (KMAXNVR==KTB(K).OR.KMAXNVR==KTO(K).OR.KMAXNVR==KTM(K)) THEN
             NFLAG=1
          ELSE IF (KMAXNVR==KGC(K).OR.KMAXNVR==KGC(K).OR.KMAXNVR==KGC(K)) THEN
             NFLAG=2
          ENDIF
       ENDDO
       IF (NFLAG.EQ.1) THEN
          KK(:)=0
          DO K=1,NPHEN
             IF (RI(KTB(K))%NVAL.GT.1) KK(1)=KK(1)+1
             IF (RI(KTO(K))%NVAL.GT.1) KK(2)=KK(2)+1
             IF (RI(KTM(K))%NVAL.GT.1) KK(3)=KK(3)+1
          ENDDO
          IF (SUM(KK).NE.KK(1).AND.SUM(KK).NE.KK(2).AND.SUM(KK).NE.KK(3)) THEN
             WRITE(IINFFIL,*)'Error in no of varying cardinal temperatures: ',KK(1:3)
             CLOSE(IINFFIL)
             STOP
          ENDIF
          IF (KK(1).GT.1) THEN 
             TBFLAG=.TRUE.
          ELSE IF (KK(2).GT.1) THEN 
             TOFLAG=.TRUE.
          ELSE IF (KK(3).GT.1) THEN 
             TMFLAG=.TRUE.
          ENDIF
       ELSE IF (NFLAG.EQ.2) THEN
          KK(:)=0
          DO K=1,NPHEN
             IF (RI(KGC(K))%NVAL.GT.1) KK(1)=KK(1)+1
          ENDDO
          IF (KK(1)==NPHEN) THEN
             DO K=1,NPHEN
                IF (RI(KGC(K))%METH.NE.'a') THEN
                   WRITE(IINFFIL,*)'Only interval METHod a enabled for vaying all GCs together'
                   CLOSE(IINFFIL)
                   STOP
                ENDIF
             ENDDO
             GCFLAG=.TRUE.
          ELSE IF (KK(1).NE.1.AND.KK(1).NE.NPHEN) THEN
             WRITE(IINFFIL,*)'Error in no of varying Genetic Coeffs. Choose 1 or NPHEN.'
             CLOSE(IINFFIL)
             STOP
          ENDIF
       ENDIF
       NN=MAX(MAXNVR,MAXNVI,MAXNVC)
       IF (NN==-999) THEN
          WRITE(IINFFIL,*)'MAXNV\NN error'
          CLOSE(IINFFIL)
          STOP
       ELSE IF (NN==1) THEN
          KNRUNS=1
       ELSE
          IF (NN==MAXNVR) THEN
             KNRUNS=KMAXNVR
             IF (RI(KMAXNVR)%METH=='p') THEN
                IF (RI(KMAXNVR)%VAL>TESTNODATA) THEN
                   R_INTER=(RI(KMAXNVR)%MAX-RI(KMAXNVR)%MIN)*RI(KMAXNVR)%VAL/(100.*(RI(KMAXNVR)%NVAL-1))
                   RI(KMAXNVR)%MIN=RI(KMAXNVR)%MIN/100.*RI(KMAXNVR)%VAL
                ELSE
                   WRITE(IINFFIL,*)'Cannot use method ',RI(KMAXNVR)%METH,' for SEN/HYP with NODATA val.'
                   CLOSE(IINFFIL)
                   STOP
                ENDIF
             ELSE IF (RI(KMAXNVR)%METH=='a') THEN
!                IF (RI(KMAXNVR)%VAL>TESTNODATA) THEN
                   R_INTER=(RI(KMAXNVR)%MAX-RI(KMAXNVR)%MIN)/(RI(KMAXNVR)%NVAL-1)
!                ELSE
!                   WRITE(IINFFIL,*)'Cannot use method ',RI(KMAXNVR)%METH,' for SEN/HYP with NODATA val.'
!                   STOP
!                ENDIF
             ELSE IF (RI(KMAXNVR)%METH=='d') THEN
                R_INTER=(RI(KMAXNVR)%MAX-RI(KMAXNVR)%MIN)/(RI(KMAXNVR)%NVAL-1)
                IF (RI(KMAXNVR)%VAL>TESTNODATA) RI(KMAXNVR)%MIN=RI(KMAXNVR)%VAL+RI(KMAXNVR)%MIN
             ELSE
                WRITE(IINFFIL,*)'Main: real interval method error. Methods p, a and d only are valid options. '
                CLOSE(IINFFIL)
                STOP
             ENDIF
             IF (TBFLAG) THEN
                WRITE(IINFFIL,*)NN,' sens runs per spatio-temporal point, varying all base temperatures'
             ELSE IF (TOFLAG) THEN
                WRITE(IINFFIL,*)NN,'sens runs per spatio-temporal point, varying all optimum temperatures'
             ELSE IF (TMFLAG) THEN
                WRITE(IINFFIL,*)NN,'sens runs per spatio-temporal point, varying all maximum temperatures'
             ELSE IF (GCFLAG) THEN
                WRITE(IINFFIL,*)NN,'sens runs per spatio-temporal point, varying all GenCoeffs'
             ELSE
                WRITE(IINFFIL,*)NN,'sens runs per spatio-temporal point, varying ',RI(KMAXNVR)%NAME, 'only'
             ENDIF
             IF (KMAXNVR==0) THEN
!                YGPFLAG=.TRUE. !IE it's YGP that is being varied in HYP/SEN
!                IF (YGP<TESTNODATA) THEN
!                   WRITE(IINFFIL,*)'Cannot vary YGP from baseline of ',YGP,'!!'
!                   STOP
!                ENDIF
                IF (MODE=='SEN') THEN
                   WRITE(IINFFIL,*)'Cannot do SEN run varying YGP!!'
                   CLOSE(IINFFIL)
                   STOP
                ENDIF
             ENDIF
          ELSE IF (NN==MAXNVI) THEN
             KNRUNS=KMAXNVI
             IF (II(KMAXNVI)%METH=='a') THEN
                IF (II(KMAXNVI)%VAL>TESTNODATA) THEN  
                   I_INTER=(II(KMAXNVI)%MAX-II(KMAXNVI)%MIN)/(II(KMAXNVI)%NVAL-1)
                ELSE
                   WRITE(IINFFIL,*)'Cannot use method ',RI(KMAXNVR)%METH,' for SEN/HYP with NODATA val.'
                   CLOSE(IINFFIL)
                   STOP
                ENDIF
             ELSE IF (II(KMAXNVI)%METH=='d') THEN
                I_INTER=(II(KMAXNVI)%MAX-II(KMAXNVI)%MIN)/(II(KMAXNVI)%NVAL-1)
                IF (II(KMAXNVI)%VAL.NE.NODATA) II(KMAXNVI)%MIN=II(KMAXNVI)%VAL+II(KMAXNVI)%MIN
             ELSE
                WRITE(IINFFIL,*)'Main: integer interval method error. Methods a and d only are valid options.'
                CLOSE(IINFFIL)
                STOP
             ENDIF
             WRITE(IINFFIL,*)NN,'sens runs per spatio-temporal point, varying ',II(KMAXNVI)%NAME, 'only'
          ELSE IF (NN==MAXNVC) THEN
             KNRUNS=KMAXNVC
             WRITE(IINFFIL,*)NN,'sens runs per spatio-temporal point, varying ',CI(KMAXNVC)%NAME, 'only'
          ENDIF
       ENDIF

       RETURN
       END SUBROUTINE SENSCALC

!------------------------------------------------------------------------------------------
! Subroutine added by Kathryn Nicklin December 2009 to read in era-interim data from the netcdf files. 

       SUBROUTINE GETVARA(NCID,VARNAME,OPTION)   
       USE AFRICAMOD
       USE GLOBAL_VARS
       IMPLICIT NONE
       INCLUDE 'netcdf.inc' !need to change to correct location for GLAM
    
       INTEGER :: STATUS, NCID, VARID
       CHARACTER(LEN=20) :: VARNAME
       CHARACTER(LEN=31) :: DUMMY
       INTEGER   :: XTYPE, NDIM, NATTS, I, LEN
       INTEGER :: DIMIDS(10), START(10), COUNT(10)
       CHARACTER(LEN=3) :: OPTION

!       PRINT *,'variable name is: ',VARNAME

       STATUS=NF_INQ_VARID(NCID,TRIM(VARNAME),VARID)
          IF ( STATUS.NE.NF_NOERR ) WRITE(IINFFIL,*) 'error reading era-interim netcdf file'
       STATUS=NF_INQ_VAR(NCID,VARID,DUMMY,XTYPE,NDIM,DIMIDS,NATTS)
          IF ( STATUS.NE.NF_NOERR ) WRITE(IINFFIL,*) 'error reading era-interim netcdf file'
       DO I=1,NDIM
        STATUS=NF_INQ_DIM(NCID,DIMIDS(I),DUMMY,LEN)
          IF ( STATUS.NE.NF_NOERR )WRITE(IINFFIL,*) 'error reading era-interim netcdf file'
        START(I)=1 ; COUNT(I)=LEN
       END DO 

       IF(OPTION.EQ.'wth') STATUS=NF_GET_VARA_REAL(NCID,VARID,START,COUNT,VAR_STORE)
       IF(OPTION.EQ.'lon') STATUS=NF_GET_VARA_REAL(NCID,VARID,START,COUNT,LONGITUDE)
       IF(OPTION.EQ.'lat') STATUS=NF_GET_VARA_REAL(NCID,VARID,START,COUNT,LATITUDE)
      

       RETURN
       END SUBROUTINE GETVARA

!------------------------------------------------------------------------------------------
!Subroutine added by Kathryn Nicklin December 2009 to read in the GPCP 1DD rainfall data

      SUBROUTINE READ_GPCP_1DD
!     For each month file reads data into GPCPDATA(360,180,31)
!     In GPCPDATA the latitude values go from 89.5N to 89.5S and the longitude values go from  0.5E to 0.5W
!     The program then saves only the region lat=0.5 to 29.5 (index 1 to 30) and 
!     lon = -19.5 to 29.5 (index 1 to 50) in GPCPRAIN and moves onto the next month.

      USE MULTI_VARS         
      USE AFRICAMOD

      IMPLICIT NONE

!     variables i need for this subroutine
      CHARACTER(LEN=100) :: FNAME
      CHARACTER(LEN=1440) :: HEADER
      INTEGER :: IRET, j, i, NSKIP, NDAYSMONTH(12), IDAY, IMONTH, NDAYSRAIN
      REAL*4 :: GPCPDATA(360,180,31)
      CHARACTER(LEN=4) :: YEARTEMP
      CHARACTER(LEN=1) :: MONTHTEMP1
      CHARACTER(LEN=2) :: MONTHTEMP2

      PRINT*,'reading in gpcp rainfall data'
      PRINT*,'due to data availability can only run for 1997-2008 or a subset of these years - please check param files'

      GPCPRAIN(:,:,:,:)=FLOAT(NODATA)

      DO IYR=ISYR,IEYR

       NDAYSMONTH(:)=NODATA
       NDAYSRAIN = 0

       YEARTEMP = '0000'
       WRITE(YEARTEMP,'(I4)')IYR
	
       DO IMONTH=1,12
	
	GPCPDATA(:,:,:) = FLOAT(NODATA)
	
	IF(IMONTH.LT.10) THEN
	 WRITE(MONTHTEMP1,'(I1)')IMONTH
         FNAME='/nfs/see-archive-06_a17/eekjn/GPCP_1DD/gpcp_1dd_p1d.'//YEARTEMP//'0'//MONTHTEMP1
!         FNAME='/home/RAID3-D13/tompkins/DATA/gpcp/daily/bin/gpcp_1dd_p1d.'//YEARTEMP//'0'//MONTHTEMP1
	ELSEIF(IMONTH.GE.10) THEN
	 WRITE(MONTHTEMP2,'(I2)')IMONTH
         FNAME='/nfs/see-archive-06_a17/eekjn/GPCP_1DD/gpcp_1dd_p1d.'//YEARTEMP//MONTHTEMP2
!	 FNAME ='/home/RAID3-D13/tompkins/DATA/gpcp/daily/bin/gpcp_1dd_p1d.'//YEARTEMP//MONTHTEMP2
	ENDIF
	
	!     Open the gpcp rainfall data file
!       At the ICTP need to put in record length in bytes. convert='big_endian' might not work with pgf90 compiler??
!	OPEN(UNIT=27,FILE=FNAME,STATUS='OLD',ACCESS='DIRECT',RECL=1440,FORM='UNFORMATTED', IOSTAT=IRET,CONVERT='big_endian')
!       Don't need to in leeds...
        OPEN(UNIT=27,FILE=FNAME,STATUS='OLD',ACCESS='DIRECT',RECL=360,FORM='UNFORMATTED', IOSTAT=IRET,CONVERT='big_endian')
	IF(IRET.NE.0) PRINT*, 'gpcp: problem opening data file'
		
	!     Read in the header
	READ(UNIT=27,REC=1,IOSTAT=IRET) HEADER
	IF(IRET.NE.0) PRINT*, 'gpcp: problem reading in header file'
		
	!     Find the number of days of rainfall data in the current file (should be 28,29,30 or 31)
	i=INDEX(HEADER(1:1440),'days')
	READ(HEADER(i+7:i+8),'(I2)') NDAYSMONTH(IMONTH)
	 	
	NSKIP = 1 !The file header is 1 row long
		
	DO IDAY = 1,NDAYSMONTH(IMONTH)
	 DO j=1,180
	  READ(UNIT=27, REC=((IDAY-1)*180+j+NSKIP), IOSTAT=IRET) (GPCPDATA(i,j,IDAY),i=1,360)
	  IF(IRET.NE.0) PRINT*, 'gpcp: error reading in data'
	 ENDDO
	ENDDO
		
	CLOSE(27)
	
        i=1	
	DO ILON=341,360
         j=1
	 DO ILAT=90,61,-1
	  GPCPRAIN(j,i,IYR-ISYR+1,NDAYSRAIN+1:NDAYSRAIN+NDAYSMONTH(IMONTH)) = GPCPDATA(ILON,ILAT,1:NDAYSMONTH(IMONTH))
          j=j+1
	 ENDDO
         i=i+1
	ENDDO

   	
	DO ILON=1,30
         j=1
	 DO ILAT=90,61,-1
	  GPCPRAIN(j,ILON+20,IYR-ISYR+1,NDAYSRAIN+1:NDAYSRAIN+NDAYSMONTH(IMONTH)) = GPCPDATA(ILON,ILAT,1:NDAYSMONTH(IMONTH))
          j=j+1
	 ENDDO
	ENDDO
	
	NDAYSRAIN = NDAYSRAIN+NDAYSMONTH(IMONTH)
	
       ENDDO !end of month loop
	
!       PRINT *,'ndaysrain is:',NDAYSRAIN
		
!       PRINT *, 'ndaysmonth is:',NDAYSMONTH

      ENDDO !end of year loop

!      PRINT *,'grid cell 1,1 values (lat = 0.5N, lon = -19.5E) for first year are:',GPCPRAIN(1,1,1,:)
!      PRINT *,'grid cell 30,50 values (lat = 29.5N, lon = 29.5E) for first year are:',GPCPRAIN(30,50,1,:)

!       PRINT *,'grid cell 30,50 values (lat = 29.5N, lon = 29.5E) for last year are:',GPCPRAIN(30,50,12,:)
!       PRINT *,'grid cell 30,50 values (lat = 29.5N, lon = 29.5E) for second to last year are:',GPCPRAIN(30,50,11,:)
!       PRINT *,'grid cell 15,21 values (lat = 14.5N, lon = 0.5E) for second to last year are:',GPCPRAIN(15,21,11,:)
!       PRINT *,'grid cell 16,20 values (lat = 15.5N, lon = 0.5W) for third year are:',GPCPRAIN(16,20,3,:)
       RETURN
       END SUBROUTINE READ_GPCP_1DD
