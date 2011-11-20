!     Fortran90 subroutines for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2009.
!     Copyright The University of Reading and the University of Leeds 2009. All rights reserved.

!--------------------------------------------------------------------------------------------
!      OPTIMISATION SRSs: MERIT and MCALC
!--------------------------------------------------------------------------------------------


      SUBROUTINE MERIT
!C    Calculate merit function 
!C
!C     ------------------------------------------------------------------
!C     Input     : 
!C     In        : 
!C     Out       : 
!C     Output    : 
!C     Called by : Main
!C     Calls     : 
!C     ------------------------------------------------------------------
!C

      USE GLOBAL_VARS
      USE MULTI_VARS 
      USE IO_VARS
      IMPLICIT NONE
      INTEGER :: J,IY,ISLAT,IELAT,ISLON,IELON,IOPTH,M
      REAL :: XVAR
      CHARACTER*11 :: CINF
      WRITE(CINF(1:4),'(I4)')ISYR
      CINF(5:5)='-'
      WRITE(CINF(6:9),'(I4)')IEYR
      CINF(10:10)='-'
      WRITE(CINF(11:11),'(I1)')MMNO

      !Initial values
      SD_OBS(:,:)=FLOAT(NODATA)
      MEANX(:,:)=0.0    
      IOPTH=NODATA

      ! Main block
      IF (IMERF==0) THEN !Local optimisation: RMERIT(1:6) 
         IF (NHYP==MAXNVR) THEN
            OPEN(UNIT=IFLMER,FILE=TRIM(OUTDIR)//TRIM(CROP)//'_'//TRIM(RI(KNRUNS)%NAME)//CINF//'_loc.mer')
         ELSE IF (NHYP==MAXNVI) THEN
            OPEN(UNIT=IFLMER,FILE=TRIM(OUTDIR)//TRIM(CROP)//'_'//TRIM(II(KNRUNS)%NAME)//CINF//'_loc.mer')
         ENDIF
         DO ILAT=1,NLAT
            DO ILON=1,NLON
               IF (JSKIP(ILAT,ILON)==0) THEN
                  CALL MCALC(ILAT,ILAT,ILON,ILON,IOPTH,.FALSE.)
                  IF (IOPTH.NE.NODATA) THEN
                     IF (NHYP==MAXNVR) THEN
                        XVAR=RI(KNRUNS)%MIN+FLOAT(IOPTH-1)*R_INTER
                     ELSE IF (NHYP==MAXNVI) THEN
                        XVAR=II(KNRUNS)%MIN+(IOPTH-1)*I_INTER 
                     ENDIF
                     WRITE(IFLMER,MOUTFMT)ILAT,' ',ILON,' ',XVAR,(' ',RMERIT(M),M=1,NMERIT)
                  ELSE
                     WRITE(IINFFIL,*)'No optimal parameter value found.'
                     STOP
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         
      ELSEIF (IMERF==1) THEN !Global optimation: RMERIT(1:3) only
         CALL MCALC(1,NLAT,1,NLON,IOPTH,.TRUE.)
         
      ENDIF
      CLOSE(IFLMER)
      RETURN
    END SUBROUTINE MERIT
    



      SUBROUTINE MCALC(ISLAT,IELAT,ISLON,IELON,IOPTH,LGLOB)
      USE GLOBAL_VARS
      USE MULTI_VARS 
      USE IO_VARS
      IMPLICIT NONE
      REAL :: SUMX,SUMY,SUMX2,SUMY2,SUMXY,SD_PRED,MEANY,IOSTAT,SUMSIMYIELD !variable IOSTAT and SUMSIMYIELD added by KJN
      INTEGER :: ISLAT,IELAT,ISLON,IELON,IOPTH !Latter is index of best output (IHYP)
      INTEGER :: NOBS,IHYP,M,IY,ILA,ILO,IOS,K !variable IOS added by kathryn nicklin 10May2009 to check for opening errors
      LOGICAL :: LGLOB
      REAL :: PRED,OBS
      CHARACTER*9 :: CYRS
      WRITE(CYRS(1:4),'(I4)')ISYR
      CYRS(5:5)='-'
      WRITE(CYRS(6:9),'(I4)')IEYR

      !Initial values
      RMERIT_MIN=1E30  !Worst other-merits is high
      RMERIT_MIN(4)=-1. !worst correlation is zero.Kathryn Nicklin changed this to -1 because this is actually the worst correlation.
      
      IF (LGLOB) THEN
         OPEN(UNIT=IFLMER,FILE=TRIM(OUTDIR)//TRIM(CROP)//'_'//TRIM(RI(KNRUNS)%NAME)//CYRS//'_glo.mer')
      ELSE
         IF (ISLAT.NE.IELAT.AND.ISLON.NE.IELON) THEN
            WRITE(IINFFIL,*)'Local merit error: more than one grid cell'
            STOP
         ENDIF
      ENDIF

      DO IHYP=1,NHYP
         RMERIT(:)=0.
         MEANX(:,:)=0.
         MEANY=0.
         SUMX=0.
         SUMY=0.
         SUMXY=0.
         SUMX2=0.
         SUMY2=0.
         NOBS=0
         DO ILA=ISLAT,IELAT
            DO ILO=ISLON,IELON
               IF (JSKIP(ILA,ILO)==1) THEN
                  IF (.NOT.LGLOB) RETURN
               ELSE
                  DO IY=1,IEYR-ISYR+1
                   IF(OBS_YLD(ILA,ILO,IY).GE.0) THEN !Kathryn Nicklin added to allow missing yield data
                     NOBS=NOBS+1
                     SUMSIMYIELD=0. !KJN added the next few lines to average simulated yields from all planting dates
                     DO K=1,NPLANT
                      SUMSIMYIELD=SUMSIMYIELD+BBYLD(IY,ILA,ILO,IHYP,K)
                     ENDDO
                     PRED=SUMSIMYIELD/FLOAT(NPLANT) !KJN PRED is now average simulated yields
                     OBS=OBS_YLD(ILA,ILO,IY) !Observed yield
                     SUMXY=SUMXY+(OBS*PRED)
                     SUMX=SUMX+OBS
                     SUMY=SUMY+PRED
                     SUMX2=SUMX2+OBS**2
                     SUMY2=SUMY2+PRED**2
                     MEANX(ILA,ILO)=MEANX(ILA,ILO)+OBS
                     MEANY=MEANY+PRED                 
                     RMERIT(1)=RMERIT(1)+(OBS-PRED)**2  !RMSE
                     RMERIT(2)=RMERIT(2)+ABS(OBS-PRED)  !Mean absolute error
                     IF (OBS>0.) RMERIT(3)=RMERIT(3)+ABS(OBS-PRED)/OBS !Mean fractional error
                   ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
         RMERIT(1)=SQRT(RMERIT(1)/FLOAT(NOBS))
         RMERIT(2)=RMERIT(2)/FLOAT(NOBS)
         RMERIT(3)=RMERIT(3)/FLOAT(NOBS)
         SAVEMERIT(2,IHYP) = RMERIT(1)
!         PRINT *, RMERIT(1)

         IF (LGLOB) THEN
            RMERIT(4:6)=FLOAT(NODATA)
            IF (NHYP==MAXNVR) THEN
               WRITE(IFLMER,MOUTFMT)-99,' ',-99,' ',RI(KNRUNS)%MIN+FLOAT(IHYP-1)*R_INTER,(' ',RMERIT(M),M=1,3)        
            ELSE IF (NHYP==MAXNVI) THEN
               WRITE(IFLMER,MOUTFMT)-99,' ',-99,' ',FLOAT(II(KNRUNS)%MIN+(IHYP-1)*I_INTER),(' ',RMERIT(M),M=1,3)     
            ENDIF
         ELSE            
          
            !Calculate local-only merits
            IF (NOBS>0) THEN
               MEANY=MEANY/FLOAT(NOBS)
               MEANX(ISLAT,ISLON)=MEANX(ISLAT,ISLON)/FLOAT(NOBS)
               
               IF (SUMY**2/NOBS==SUMY2.OR.SUMX2==SUMX**2/NOBS) THEN !Correlation
                  RMERIT(4)=FLOAT(NODATA) !Avoid /0.
               ELSE
                  RMERIT(4)=(SUMXY-SUMX*SUMY/FLOAT(NOBS))/SQRT((SUMX2-SUMX**2/NOBS) * (SUMY2-SUMY**2/NOBS))
               ENDIF
                 
                SAVEMERIT(3,IHYP) = RMERIT(4)
!               PRINT *, RMERIT(4)
               SD_OBS(ISLAT,ISLON)= SQRT((ABS(SUMX2-FLOAT(NOBS)*MEANX(ISLAT,ISLON)**2))/FLOAT(NOBS-1)) 
               SD_PRED=SQRT((ABS(SUMY2-FLOAT(NOBS)*MEANY**2))/FLOAT(NOBS-1))                
               RMERIT(5)=SD_OBS(ISLAT,ISLON)-SD_PRED           ! Difference in SD
                SAVEMERIT(4,IHYP) = RMERIT(5)
!               PRINT *, RMERIT(5)
               RMERIT(6)=(SUMX-SUMY)/FLOAT(NOBS)  ! Difference in mean            
                SAVEMERIT(5,IHYP) = RMERIT(6)
!                PRINT *, RMERIT(6) 
           ELSE
               RMERIT(5)=9999.9999 !Rather than FLOAT(NODATA) as this is a feasible value
               RMERIT(6)=9999.9999 !Rather than FLOAT(NODATA) as this is a feasible value
            ENDIF
            
            !Find optimal local merit
            IF (MMNO==4) THEN !High merit is best (correlation)
               IF (RMERIT(MMNO)>RMERIT_MIN(MMNO)) THEN
                  IOPTH=IHYP
                  RMERIT_MIN=RMERIT
               ENDIF
            ELSE !Low absolute merit is best (RMSE, MAE, MFE, Dif-mean and Dif-SD)
               IF (ABS(RMERIT(MMNO))<ABS(RMERIT_MIN(MMNO))) THEN
                  IOPTH=IHYP
                  RMERIT_MIN=RMERIT
               ENDIF
            ENDIF
         ENDIF
      ENDDO

       RMERIT(:)=RMERIT_MIN(:)


       IF(MODE=='HYP')THEN      
        OPEN(UNIT=99,FILE='output/merit.out',STATUS='UNKNOWN',IOSTAT=IOS)
        IF(IOS.NE.0)THEN
         PRINT *,'Error opening merit ouput file'
         STOP
        ENDIF
       
        DO IHYP = 1,NHYP
        WRITE(99,*) SAVEMERIT(1,IHYP), SAVEMERIT(2,IHYP), SAVEMERIT(3,IHYP), SAVEMERIT(4,IHYP), SAVEMERIT(5,IHYP)
        ENDDO
        CLOSE(99)
       ENDIF

      RETURN
    END SUBROUTINE MCALC
