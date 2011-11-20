!     Fortran90 subroutines for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2008.
!     Copyright The University of Reading and the University of Leeds 2008. All rights reserved.

!C-----------------------------------------------------------------------
!C     SRs to calc VPD
!C-----------------------------------------------------------------------

      SUBROUTINE VPDEST(IDAP)
!     Estimate VPD
!C
!C     ------------------------------------------------------------------
!C     Input     :  TMAX, TMIN
!C     In        : 
!C     Out       : 
!C     Output    : 
!C     Local     : 
!C     Called by : 
!C     Calls     : 
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      USE IO_VARS
      IMPLICIT NONE
      REAL :: ESAT_MIN, ESAT_MAX, ESAT, ESAT_DEW, ESAT_TBAR
      INTEGER :: IDAP

      IF (IVMETH==1) THEN
         !Estimate VPD (CMH2;Tanner and Sinclair 1983; see C8)
         !Note that constant is a factor of 10 smaller than in Bolton et al 1980 due to mbar to kPa conversion
         ESAT_MIN=0.61120*EXP((17.67*TMIN(IDAP))/(TMIN(IDAP)+243.5))     
         ESAT_MAX=0.61120*EXP((17.67*TMAX(IDAP))/(TMAX(IDAP)+243.5))     
         VPD=VPD_CTE*(ESAT_MAX-ESAT_MIN) !kPa
!      ELSE
!         WRITE(IINFFIL,*)'IVMETH==2,3 have been disabled'
!         STOP
      ELSE IF (IVMETH==2) THEN
!         !Calculate VPD from vapour pressure and TBAR, as CRU simulations in GLAM2004 paper
         ESAT_TBAR=0.61120*EXP((17.67*TBAR(IDAP))/(TBAR(IDAP)+243.5))
         ESAT_DEW=0.61120*EXP((17.67*(TMIN(IDAP)))/((TMIN(IDAP))+243.5))
!         ESAT_DEW=ESAT_MIN; TDEW=TMIN
         VPD=ESAT_TBAR-ESAT_DEW
!		 VPD=VPD_CTE*ESAT
!         VPD=ESAT-VAP(IDAP)
      ELSE IF (IVMETH==3) THEN
!         !Calculate VPD from Tmax and Tmin as FAO ID paper 56
         ESAT_MIN=0.61120*EXP((17.67*TMIN(IDAP))/(TMIN(IDAP)+243.5))
         ESAT_MAX=0.61120*EXP((17.67*TMAX(IDAP))/(TMAX(IDAP)+243.5))
         ESAT_DEW=0.61120*EXP((17.67*TMIN(IDAP))/(TMIN(IDAP)+243.5))
!         ESAT_DEW=ESAT_MIN; TDEW=TMIN
!         ESAT_DEW=0.61120*EXP((17.67*(TMIN(IDAP)-2.))/((TMIN(IDAP)-2.)+243.5))
         ESAT=0.5*(ESAT_MAX+ESAT_MIN)
         VPD=ESAT-ESAT_DEW
!         VPD=ESAT-VAP(IDAP)
	  ELSE IF (IVMETH==4) THEN
	  	 ESAT_MIN=0.61120*EXP((17.67*TMIN(IDAP))/(TMIN(IDAP)+243.5))
         ESAT_MAX=0.61120*EXP((17.67*TMAX(IDAP))/(TMAX(IDAP)+243.5))
!         ESAT_DEW=0.61120*EXP((17.67*TMIN(IDAP))/(TMIN(IDAP)+243.5))
         ESAT_DEW=0.61120*EXP((17.67*(TMIN(IDAP)-2.))/((TMIN(IDAP)-2.)+243.5))
!         TDEW=TMIN-2
         ESAT=(ESAT_MAX+ESAT_MIN)/2.
         VPD=ESAT-ESAT_DEW
      ELSE IF (IVMETH==5) THEN
         VPD=4. !Season-constant value for VPD which can be varied
      ENDIF

      IF (VPD.LE.0.) THEN
         WRITE(IINFFIL,*)'SR VPDEST: Negative or zero VPD of ',VPD,' set to 0.00001 at ',IDAP
         VPD=0.00001 !Nominally small to avoid division by zero in SR BIOMASS
      ENDIF
      VPDTOT=VPDTOT+VPD
      TBARTOT=TBARTOT+TBAR(IDAP)
      TMINTOT=TMINTOT+TMIN(IDAP)
      TMAXTOT=TMAXTOT+TMAX(IDAP)

      RETURN
      END SUBROUTINE VPDEST
