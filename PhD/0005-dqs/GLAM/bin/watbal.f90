!     Fortran90 subroutines for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2008.
!     Copyright The University of Reading and the University of Leeds 2008. All rights reserved.

!C    Water Balance subroutines
!C----------------------------------------------------------------------------------------------

!C     ------------------------------------------------------------------
!C
!C     ------------------------------------------------------------------
      SUBROUTINE WATBAL(IDAP)
!C    Main Water Balance subroutine
!C    Uses NSL soil layers, NED of which have evaporable water.
!C    Every time SMC_THRESH is reached a new evaporation and uptake cycle begins.
!C    Order of processes: rain;runoff;drainage;evaporation and transpiration.
!C    Subroutines *CALC compute actual values, SR's *EST produce estimates.
!C    Potential trans P_TRANS is LAI limited only, TRANS_E is energy limited,
!     Actual TRANS is uptake, energy LAI ltd. In the evap layer it can be competition ltd.
!C    Potential uptake is soil water [VOLSW(IZ)] limited
!C                 Actual uptake is actually TRANS.
!C    P_EVAP is limited only by soil transport, EVAP_E is energy limited
!C
!C     ------------------------------------------------------------------
!C     Input     : 
!C     In        : 
!C     Out       : 
!C     Output    : SWFAC,VOLSW(NSL)
!C     Local     : 
!C     Called by : HAPPY
!C     Calls     : RCALC,DCALC,PTEST,PEEST,PUPTAKE,TCALC
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      IMPLICIT NONE
      REAL :: RLV(MAXNSL)
      INTEGER :: IZ,IDAP

      T_RAIN=T_RAIN+RAIN(IDAP)

!C    Runoff and drainage.
      CALL RCALC(IDAP)
      IF(RAIN(IDAP)<RUNOFF) THEN 
         WRITE(IINFFIL,*)'More runoff than rain!!!',rain(idap),runoff,idap
         WRITE(IINFFIL,*)'TERMINATION TIME'
         STOP
      ENDIF
      VOLSW(1)=VOLSW(1)+(RAIN(IDAP)-RUNOFF)/DZ
      CALL DCALC()

!C     Estimate, seperately, ET, P_TRANS, P_EVAP
      CALL PETEST(IDAP)  !EVAP_E and TRANS_E, limited by energy only
      CALL PTEST () !P_TRANS, limited only by LAI
      CALL PEEST(IDAP)  !P_EVAP, limited only by soil transport

!C    TRANS and EVAP limited by soil transport and  LAI and energy
      P_TRANS=MIN(P_TRANS,TRANS_E)
      P_EVAP=MIN(P_EVAP,EVAP_E)

!C    Actual TRANS ans EVAP (limited by  soil transport, LAI, energy and SMC)
      CALL PUPTAKE(RLV,IDAP) !Potential uptake
      CALL ETCALC(idap)

!c    Check soil isn't too dry or too wet
      DO IZ=1,NSL
         IF (VOLSW(IZ)+TOLSOL.LT.RLL) WRITE(IINFFIL,*)'SR watbal: soil dry: ',idap,z(iz),volsw(iz),rll
         IF (VOLSW(IZ)-TOLSOL>SAT) WRITE(IINFFIL,*)'SR watbal: soil wet: ',idap,z(iz),volsw(iz),sat  
      ENDDO

!C    Calculate total soil water (SWATER)
      NRD=INT(ZEF(IDAP)/DZ) !Grid point of current rooting depth
      IF (NRD>0) THEN
         RZSW=SUM(VOLSW(1:NRD))/FLOAT(NRD) !Mean volumetric root zone soil moisture
      ELSE
         RZSW=FLOAT(NODATA)
      ENDIF
      SWATER=SUM(VOLSW(1:NSL)*DZ)
      IF (RLAI(2).GT.0.AND.P_TRANS>0.) THEN
         SWFAC=TRANS/P_TRANS        
      ELSE 
         SWFAC=0.1
      ENDIF

!C    Update totals
      T_EVAP=T_EVAP+EVAP
      TP_EVAP=TP_EVAP+P_EVAP
      T_DRAIN=T_DRAIN+DRAIN
      TP_TRANS=TP_TRANS+P_TRANS
      C_EVAP=C_EVAP+EVAP
      T_RUN=T_RUN+RUNOFF
!C
      RETURN
      END SUBROUTINE WATBAL
!C
!C     ------------------------------------------------------------------
!C
!C     ------------------------------------------------------------------
!C


    SUBROUTINE POTWATBAL(IDAP)
!C    Water Balance subroutine for non-water-limited production

!C     ------------------------------------------------------------------
!C     Input     : 
!C     In        : 
!C     Out       : 
!C     Output    : SWFAC,VOLSW(NSL)
!C     Local     : 
!C     Called by : HAPPY
!C     Calls     : 
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      IMPLICIT NONE
      INTEGER IDAP

!  Easy stuff.
      RLA=RNOVAL
      RLV_MEAN=RNOVAL
      RUNOFF=RNOVAL
      DRAIN=RNOVAL
      DTPUPTK=RNOVAL  !Incremental total potential uptake for all soil
      TP_UP=RNOVAL     !Cumulative total potential uptake
      SWATER=RNOVAL
      SWFAC=1.
      
!  Set TRANS and EVAP to energy limited values
      CALL PETEST(IDAP)  !EVAP_E and TRANS_E, limited by energy only
      CALL PTEST !P_TRANS, limited only by LAI
      P_TRANS=MIN(P_TRANS,TRANS_E)
      P_EVAP=EVAP_E
      TRANS=P_TRANS
      EVAP=P_EVAP

!  Totals
      T_EVAP=T_EVAP+EVAP
      TP_EVAP=TP_EVAP+P_EVAP
      TP_TRANS=TP_TRANS+P_TRANS       
      C_EVAP=C_EVAP+EVAP
      T_DRAIN=RNOVAL
      T_RUN=RNOVAL
      T_RAIN=RNOVAL
!C
      RETURN
    END SUBROUTINE POTWATBAL

!C
!C     ------------------------------------------------------------------
!C
!C     ------------------------------------------------------------------
!C

      SUBROUTINE PEEST(IDAP)
!C    Estimates potential soil evaporation, P_EVAP; this is limited by soil transport
!C
!C     ------------------------------------------------------------------
!C     Input     : I_E, rain
!C     In        : 
!C     Out       : 
!C     Output    : P_EVAP
!C     Local     : 
!C     Called by : WATBAL
!C     Calls     : 
!C     ------------------------------------------------------------------

      USE GLOBAL_VARS
      IMPLICIT NONE
      REAL :: SMC
      INTEGER :: IDAP

      IF (I_E==1) THEN
         SMC=SUM(VOLSW(1:NED))/FLOAT(NED)!mean SMC of layers 1:NED
         IF(SMC.GE.SMC_THRESH) THEN
            IESTG=1
            P_EVAP=EVAP_COEF
            E_TIME=1.
         ELSE
            IESTG=2
            P_EVAP=EVAP_COEF*(E_TIME**0.5-(E_TIME-1.)**0.5)
            E_TIME=E_TIME+1.
         ENDIF
      ELSE IF (I_E==2) THEN
         IF (RAIN(IDAP)>R_THRESH) THEN
            TRAIN=1.
         ELSE
            TRAIN=TRAIN+1.
         ENDIF
         P_EVAP=EVAP_E/TRAIN
      ENDIF

      END SUBROUTINE PEEST

!C------------------------------------------------------------------
!C
!C------------------------------------------------------------------

      SUBROUTINE PTEST
!C    Estimates potential transpiration, P_TRANS; this is LAI limited only CMH4
!C    See c26, CM12
!C
!C     ------------------------------------------------------------------
!C     Input     : RLAI, CRIT_LAI_T, P_TRANS_MAX
!C     In        :
!C     Out       : 
!C     Output    : P_TRANS
!C     Local     : 
!C     Called by : WATBAL
!C     Calls     : 
!C     ------------------------------------------------------------------

      USE GLOBAL_VARS
      IMPLICIT NONE

!C  Calculate potential transpiration
      IF (RLAI(2).LT.CRIT_LAI_T) THEN
         P_TRANS=P_TRANS_MAX*(1.-(CRIT_LAI_T-RLAI(2))/CRIT_LAI_T)
      ELSE
         P_TRANS=P_TRANS_MAX
      ENDIF

      END SUBROUTINE PTEST


!C------------------------------------------------------------------
!C
!C------------------------------------------------------------------

      SUBROUTINE DCALC()
!C    Calculates and executes DRAINage; CMH4, c81
!C
!C     ------------------------------------------------------------------
!C     Input     : VOLSW(NSL), D_RATE, KSAT
!C     In        : 
!C     Out       : 
!C     Output    : VOLSW(NSL), DRAIN
!C     Local     : 
!C     Called by : WATBAL
!C     Calls     : 
!C     ------------------------------------------------------------------

      USE GLOBAL_VARS
      IMPLICIT NONE
      REAL :: DSW,INFAC,INFLOW
      INTEGER :: IZ

!C    Water in excess of the drained upper limit, DUL is drained at a rate D_RATE*INFAC
      INFLOW=0.
      DO IZ=1,NSL
         INFAC=1.-LOG(INFLOW*DZ+1.)/LOG(RKSAT+1)
         if (infac<0.) then
            write(iinffil,*)'Watbal: -ve infac!',iz,inflow,dz,rksat
            stop
         endif

         IF (VOLSW(IZ)>SAT) THEN
            INFLOW=INFLOW+VOLSW(IZ)-SAT !For next soil layer in loop
            VOLSW(IZ)=SAT
         ENDIF

         IF (VOLSW(IZ)>DUL) THEN
            DSW=(VOLSW(IZ)-DUL)*D_RATE*INFAC
            INFLOW=INFLOW+DSW 
         ELSE
            DSW=-1.*INFLOW
            INFLOW=0.
         ENDIF
         VOLSW(IZ)=VOLSW(IZ) - DSW 
         IF (VOLSW(IZ)>SAT) THEN
            INFLOW=INFLOW+VOLSW(IZ)-SAT
            VOLSW(IZ)=SAT
         ENDIF
      ENDDO
      DRAIN=INFLOW*DZ

      END SUBROUTINE DCALC

!C------------------------------------------------------------------
!C
!C------------------------------------------------------------------

      SUBROUTINE ETCALC(idap)
!C    Calculates actual transpiration and evaporation and extracts water from soil
!C    TRANS (= actual uptake) = MIN(P_TRANS,DTPUPK)
!C
!C     ------------------------------------------------------------------
!C     Input     : P_TRANS,DTPUPTK,VOLSW(NSL)
!C     In        : 
!C     Out       : 
!C     Output    : TRANS,VOLSW(NSL)
!C     Local     : 
!C     Called by : WATBAL
!C     Calls     : 
!C     ------------------------------------------------------------------

      USE GLOBAL_VARS
      IMPLICIT NONE
      REAL :: FRAC,D_VSW_T(NSL),D_VSW_E(NED),ASW,DENOM,TEMP,DEFICIT,FRAC1,FRAC2,UP2
      REAL :: TRANS1,TRANS2,RATIO
      REAL, PARAMETER :: TOL=0.1
      INTEGER :: IZ,idap

!  Uptake demand at each level is reduced proportionately if Pot.Up > P.Trans
      IF (P_TRANS<DTPUPTK.AND.DTPUPTK>0.) THEN 
         FRAC=P_TRANS/DTPUPTK
      ELSE
         FRAC=1.
      ENDIF

!  Determine if evap and trans are in competition
      DO IZ=1,NSL
         D_VSW_T(IZ)=DPUPTK(IZ)/DZ*FRAC !Change in VOLSW due to trans
      ENDDO
      ASW=0.
      DO IZ=1,NED
         ASW=ASW+(VOLSW(IZ)-D_VSW_T(IZ)-RLL)*DZ !Available soil water for evap
         IF (ASW<-1.*TOL) THEN
            WRITE(IINFFIL,*)'SR ETCALC ASW error **1** : ',IZ,Z(IZ),VOLSW(IZ),D_VSW_T(IZ),RLL,ASW
            WRITE(IINFFIL,*)'Termination time.'
            STOP
         ENDIF
      ENDDO
      IFCOMP: IF (ASW.GE.P_EVAP) THEN !No competition in this case
         EVAP=P_EVAP
         TRANS=MIN(P_TRANS,DTPUPTK)
         DO IZ=1,NSL
            IF (VOLSW(IZ)-D_VSW_T(IZ).GE.RLL) THEN
               VOLSW(IZ)=VOLSW(IZ)-D_VSW_T(IZ) !Trans
            ELSE
               D_VSW_T(IZ)=0. ! Correcting rounding error
            ENDIF
         ENDDO
         TEMP=SUM(VOLSW(1:NED))
         DEFICIT=0.
         DO IZ=1,NED
            FRAC=VOLSW(IZ)/TEMP
            D_VSW_E(IZ)=P_EVAP/DZ*FRAC
            IF (VOLSW(IZ)-D_VSW_E(IZ) < RLL) THEN
               DEFICIT=DEFICIT+D_VSW_E(IZ)+RLL-VOLSW(IZ)
               D_VSW_E(IZ)=VOLSW(IZ)-RLL
            ENDIF
            VOLSW(IZ)=VOLSW(IZ)-D_VSW_E(IZ)
         ENDDO
         IF (DEFICIT>0.) THEN
            DO IZ=1,NED
               D_VSW_E(IZ)=MIN(VOLSW(IZ)-RLL,DEFICIT)
               VOLSW(IZ)=VOLSW(IZ)-D_VSW_E(IZ)
               DEFICIT=DEFICIT-D_VSW_E(IZ)
               if (deficit<0.) WRITE(IINFFIL,*)'ETCALC: -ve deficit'
            ENDDO
         ENDIF
      ELSE !We have competition
         ASW=0.
         DO IZ=1,NED
            ASW=ASW+(VOLSW(IZ)-RLL)*DZ !Available soil water for evap and trans
         ENDDO
         IF (ASW<-1.*TOL) THEN
            WRITE(IINFFIL,*)'SR ETCALC ASW error **2** : ',RLL
            DO IZ=1,NED
               WRITE(IINFFIL,*)IZ,Z(IZ),VOLSW(IZ),ASW
            ENDDO
            WRITE(IINFFIL,*)
            STOP
         ENDIF
         IC: IF (I_C==0) THEN
            FRAC=P_EVAP/(SUM(DPUPTK(1:NED))+P_EVAP)
            EVAP=FRAC*ASW
            TRANS1=ASW-EVAP
            TRANS2=SUM(DPUPTK(NED+1:NSL))
            IF (SUM(DPUPTK(1:NED))>0.) THEN
               FRAC1=TRANS1/SUM(DPUPTK(1:NED))
            ELSE
               FRAC1=0.
            ENDIF
            TRANS2=MIN(SUM(DPUPTK(NED+1:NSL)),P_TRANS-TRANS1)
            IF (SUM(DPUPTK(NED+1:NSL))>0.) THEN
               FRAC2=TRANS2/SUM(DPUPTK(NED+1:NSL))
            ELSE
               FRAC2=0.
            ENDIF
            TRANS=TRANS1+TRANS2
         ELSE 
            IP: IF (I_P==1) THEN
               PART=0.37-0.16*RLAI(2) 
               IF (PART<0.) PAUSE '-VE PART; I_P=1'
            ELSE IF (I_P==2) THEN
               PART=P_EVAP/(P_EVAP+P_TRANS) !observed potential partitioning 
            ELSE IF (I_P==3) THEN
               PART=EVAP_E/(EVAP_E+TRANS_E) !Partitioning as determined by energy
            ENDIF IP
            RATIO=PART/(1.-PART)    
            WET: IF (ASW>0.) THEN               
               IC1: IF (I_C==1) THEN
                  DENOM=1+RATIO-SUM(DPUPTK(NED+1:NSL))/DTPUPTK
                  TRANS=ASW/DENOM
                  FRAC1=TRANS/DTPUPTK
                  FRAC2=FRAC1
               ELSE 
                  IC23: IF (I_C==2) THEN
                     UP2=SUM(D_VSW_T(NED+1:NSL)*DZ)
                     TRANS=(ASW+UP2)/(1.+RATIO)
                     TRANS2=UP2
                     TRANS1=TRANS-TRANS2
                  ELSE IF (I_C==3) THEN
                     UP2=SUM(DPUPTK(NED+1:NSL))
                     TRANS=(ASW+UP2)/(1.+RATIO)
                     IF (TRANS>P_TRANS) THEN
                        TRANS=P_TRANS
                        TRANS1=ASW-TRANS*RATIO
                        TRANS2=TRANS-TRANS1
                     ELSE
                        TRANS2=UP2
                        TRANS1=TRANS-TRANS2
                     ENDIF
                  ENDIF IC23
                  IF (SUM(DPUPTK(1:NED))>0.) THEN
                     FRAC1=TRANS1/SUM(DPUPTK(1:NED))
                  ELSE
                     FRAC1=0.
                  ENDIF
                  IF (SUM(DPUPTK(NED+1:NSL))>0.) THEN
                     FRAC2=TRANS2/SUM(DPUPTK(NED+1:NSL))
                  ELSE
                     FRAC2=0.
                  ENDIF
               ENDIF IC1
               EVAP=TRANS*RATIO
            ELSE
               EVAP=0.
               UP2=SUM(D_VSW_T(NED+1:NSL)*DZ)
               TRANS2=UP2
               TRANS=TRANS2
               TRANS1=0.
               FRAC1=0.
               FRAC2=1.
            ENDIF WET
         ENDIF IC
!        Extract water which has been competed for
         DO IZ=1,NED
            D_VSW_T(IZ)=DPUPTK(IZ)/DZ*FRAC1
            IF (VOLSW(IZ)-D_VSW_T(IZ).GE.RLL) THEN
               VOLSW(IZ)=VOLSW(IZ)-D_VSW_T(IZ)
            ELSE
               D_VSW_T(IZ)=0.
            ENDIF
            D_VSW_E(IZ)=VOLSW(IZ)-RLL
            VOLSW(IZ)=RLL
         ENDDO
         DO IZ=NED+1,NSL
            D_VSW_T(IZ)=DPUPTK(IZ)/DZ*FRAC2 !Trans
            VOLSW(IZ)=VOLSW(IZ)-D_VSW_T(IZ)
         ENDDO
      ENDIF IFCOMP

      !c    Check soil isn't too dry
      do IZ=1,NSL
         if (VOLSW(IZ)-RLL.LT.-1.*TOL/DZ) then
            WRITE(IINFFIL,*)'soil dry in SR ETCALC: ',idap,z(iz),volsw(iz),frac
            stop
         endif
      enddo

      ET=TRANS+EVAP

      END SUBROUTINE ETCALC

!C------------------------------------------------------------------
!C
!C------------------------------------------------------------------

      SUBROUTINE RCALC(IDAP)
!C    Calculates runoff; *10 is for cm-->mm
!C    See CM16
!C
!C     ------------------------------------------------------------------
!C     Input     : RAIN(IDAP),VOLSW
!C     In        : 
!C     Out       : 
!C     Output    : RUNOFF
!C     Local     : 
!C     Called by : WATBAL
!C     Calls     : 
!C     ------------------------------------------------------------------

      USE GLOBAL_VARS
      IMPLICIT NONE
      REAL :: SOAK,PPN
      INTEGER :: IDAP
      PPN=RAIN(IDAP)*10.
!      SOAK=MIN(SUM(SAT-VOLSW(1:NSL))*DZ*10.,PPN) !Water which could eventually soak into soil
      SOAK=RKSAT*10.
      IF (PPN>0) THEN
!         RUNOFF=(MAX(PPN-0.2*SOAK,0.))**2/(PPN+0.8*SOAK) !FAO website
!         RUNOFF=PPN-SOAK !Crude method
         RUNOFF=PPN**2/(PPN+SOAK) !SCS style c73
         RUNOFF=RUNOFF/10.      
      ELSE
         RUNOFF=0.
      ENDIF


      END SUBROUTINE RCALC


      SUBROUTINE PETEST(IDAP)
!C    Calculates energy limited EVAP and TRANS according to Priestly Taylor 
!C          based method. CM14; CMH4
!C     ------------------------------------------------------------------
!C     Input     : SRAD, Albedo variables, VOLSW, ESAT slope params, TBAR
!C     In        : EXTC, RLAI, SHF_CTE
!C     Out       : 
!C     Output    : EVAP_E, TRANS_E, ETMAX
!C     Local     : 
!C     Called by : 
!C     Calls     : 
!C     ------------------------------------------------------------------

      USE GLOBAL_VARS
      IMPLICIT NONE
      REAL :: RN,ESLOPE,PT_FACT,ET_MAX,SOILHF!,SMC
      INTEGER :: IDAP
!c      REAL, PARAMETER :: PT1=10.56

!   Net radiation      
      RN=(1.-ALBEDO)*SRAD(IDAP)

!   Soil heat flux
      ESLOPE=A_ESLOPE*B_ESLOPE*C_ESLOPE/(TBAR(IDAP)+C_ESLOPE)**2 &
              *EXP(B_ESLOPE*TBAR(IDAP)/(TBAR(IDAP)+C_ESLOPE))
!   Priestley-Taylor      
      PT_FACT=1.
      PT_COEF=PT_FACT*PT_CONST
      PT_COEF = 1. + (PT_COEF-1.) * VPD / VPD_REF
      IF (ISHF==0) THEN
         ET_MAX=PT_COEF * RN * ESLOPE/(ESLOPE+PSYCHO) &   
              *10**6      & !To convert fluxes MJ to J
              /RLAT_HT    & !Latent heat flux to water flux
              *100./RHO_W   !Kg/m^2 to cm
      ELSE 
         SOILHF=SHF_CTE*RN*EXP(-EXTC*RLAI(2))
         ET_MAX=PT_COEF * (RN-SOILHF) * ESLOPE/(ESLOPE+PSYCHO) &   !This is in fact ET_POT
              *10**6      & !To convert fluxes MJ to J
              /RLAT_HT    & !Latent heat flux to water flux
              *100./RHO_W   !Kg/m^2 to cm
      ENDIF
!   Energy-only limited evap and trans
      TRANS_E=ET_MAX*(1.-EXP(-EXTC*RLAI(2)))
      IF (ISHF==0) THEN
         EVAP_E=ET_MAX*EXP(-EXTC*RLAI(2))*(1.-SHF_CTE)        
      ELSE 
         EVAP_E=ET_MAX*EXP(-EXTC*RLAI(2))
      ENDIF
      PET=EVAP_E+TRANS_E
!   Total radiations
      TRADABS=TRADABS + RN * (1. - EXP(-EXTC*RLAI(2)))  !MJ/m^2
      TRADNET=TRADNET + RN
      
      END SUBROUTINE PETEST

!C------------------------------------------------------------------
!C
!C------------------------------------------------------------------
!C
!C     PUPTK(z) is the potential uptake as a function of z
!C     MAXNSL is the maximum value of NSL
!C     DTPUPTK is the increment in total potential uptake
!C     P_UP(2) is the total potential uptake at this (2) and the previous (1) time step
            !since the beginning of the last (1-exp) uptake start
!C     TP_UP is the total potential uptake (summed over time)
!C     RLV(NSL) is the root length per unit volume (cm)
!C     RLA is the root length per unit area
