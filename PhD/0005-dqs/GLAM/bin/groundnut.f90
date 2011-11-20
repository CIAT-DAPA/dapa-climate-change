!     Fortran90 subroutines for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2008.
!     Copyright The University  of Reading and the University of Leeds 2008. All rights reserved.

      SUBROUTINE BIOMASS(IDAP,ALAI)
!C
!C     Calculate biomass, and dHI/dt and => yield
!C
!C     ------------------------------------------------------------------
!C     Input     : 
!C     In        : TRANS
!C     Out       : YIELD
!C     Output    : 
!C     Called by : Main
!C     Calls     : 
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      USE HIGHT_VARS
      IMPLICIT NONE
      REAL :: TRANS_KG,ALAI,ATE
      INTEGER :: IDAP

!C    Harvest Index
      IF (ISTG.GE.2) THEN
         IF (LHIGHT) THEN 
            HI=HI+DHDT_HT*FLOAT(IDT)
         ELSE
            HI=HI+DHDT*FLOAT(IDT)
         ENDIF
      ELSE
         HI=0.
      ENDIF

!C    Trans in kg/m^2
      TRANS_KG=TRANS/100.*RHO_W !RHO_W is density of water. TRANS is presumably in cm.

!C    Transpiration efficiency
      IF (TETRS<0.) THEN
         ATE=TE
      ELSE IF (TETRS>0.) THEN !Added FEB07 to reduce TE at high temperature
         IF (TBAR(IDAP)>TETR1.AND.TBAR(IDAP)<TETR2) THEN
            ATE=TE*(1. - (TBAR(IDAP)-TETR1)/(TETR2-TETR1))
         ELSEIF (TBAR(IDAP).GE.TETR2) THEN
            ATE=0.
         ELSE
            ATE=TE

         ENDIF
      ELSE
         WRITE(IINFFIL,*)'SR biomass: ambiguous TETRS'
         STOP
      ENDIF

!C    Biomass and yield
      BMASS=BMASS+MIN(TEN_MAX,ATE/VPD)*TRANS_KG*FLOAT(IDT)*10. !*10 is g/m^2 to kg/hectare
      YIELD=HI*BMASS !in kg/hectare

      !Calculate SLA
      IF (BMASS-YIELD>0.) SLA=10**5*ALAI/(BMASS-YIELD) !10**5 is correct conversion to produce SLA in cm^2/g

      IF (LSLA) THEN
         !For first NDSLA days after IEMDAY, fix SLA to SLA_INI
         IF ((IDAP-ISDAY-IEMDAY+1).LE.NDSLA.AND.SLA>SLA_INI) THEN
            BMASS=10**5*ALAI/SLA_INI
            SLA=SLA_INI
         ENDIF
      ENDIF
          
      RETURN
      END SUBROUTINE BIOMASS







      SUBROUTINE GLAM(POT,ASWS,IHDATE,E_DEPTH,IDAP,IHYP,IPLANT) !AJC-R2
!     Main code block for General Large-Area Model for annual crops
!     Release 1, created from v2A.201 in Oct 2005.
!     Documentation: glam-r1.doc 

      USE GLOBAL_VARS 
      USE IO_VARS
      IMPLICIT NONE
      INTEGER :: IHDATE,IDAP,IHYP,IPLANT
      REAL    :: ASWS,E_DEPTH,ALAI !Latter is actual LAI (not reduced by YGP)
      LOGICAL :: POT

 
!      Pre-start day calculations 
      IF (LINT) THEN !Intelligent planting routine used
         IF (POT) THEN
            ISDAY=1
         ELSE               
            FINDSOW: DO IDAP=1,-1*ISDAY,IDT
               CALL VPDEST(IDAP)
               CALL WATBAL(IDAP)
               IF(ISPARE2.EQ.-1) THEN !Kathryn Nicklin added this IF to allow for different planting routines
                IF ((VOLSW(1)-RLL).GE.FSWSOW*(DUL-RLL)) GOTO 7 !Intelligent planting acheived
               ELSEIF(ISPARE2.EQ.1) THEN
                IF(RAIN(IDAP).GT.1.0) GOTO 7 !Intelligent planting acheived
               ELSE
                PRINT*,'can not identify intelligent planting routine'
               ENDIF
            ENDDO FINDSOW
            WRITE(IINFFIL,*)'(intelligent) emergency planting date on ',ISDAY
            IDAP=IDAP-1 !Because IDAP ends up one-over otherwise
!7           ISDAY=IDAP+1
7           ISDAY=IDAP+IPLANT !KJN changed from the above to allow multiple planting dates
            IF(IPLANT.GT.1) THEN !KJN added so that days of water balance are not missed out
             DO IDAP=ISDAY-IPLANT+1,ISDAY-1 
              CALL VPDEST(IDAP)
              CALL WATBAL(IDAP)
             ENDDO
            ENDIF
            !Reset roots
            ITEXP(:)=NODATA
            ZEF(:)=0.
            TP_EVAP=0.
            T_EVAP=0.
            TP_UP=0.
            IF (IASCII==3) WRITE(IINFFIL,*)'(intelligent) planting date is ',ISDAY,'-1 days after the IPDATE of ',IPDATE
         ENDIF
      ELSE !No intelligent planting
         DO IDAP=1,ISDAY-1,IDT
            CALL VPDEST(IDAP)
            CALL WATBAL(IDAP)
         ENDDO
         !Reset roots
         ITEXP(:)=NODATA
         ZEF(:)=0.
         TP_EVAP=0.
         T_EVAP=0.
         TP_UP=0.
      ENDIF

8     CONTINUE
      !Pre-emergence loop
      DO IDAP=ISDAY,ISDAY+IEMDAY-1,IDT 
         CALL VPDEST(IDAP)
         IF (POT) THEN
            CALL POTWATBAL(IDAP)
         ELSE
            CALL WATBAL(IDAP)
         ENDIF
         CALL TTCALC(IDAP,TTFLWR,TBFLWR,TOFLWR,TMFLWR)
         IF (TTFLWR>GCPLFL) THEN
            WRITE(IINFFIL,*)'Flowering occurs before IEMDAY!'
            STOP
         ENDIF
         SLA=0. 
         IF (MOD(IDAP,IOUT).EQ.0.AND.IASCII.GE.2) CALL OUTPUT(FLOAT(IDAP),IDAP,POT) 
      ENDDO
     
      !Run rest of season
      NDAYSDROUGHT=0 !Kathryn Nicklin
      SEASON: DO IDAP=ISDAY+IEMDAY,NDAYSYR,IDT 
         IF (IDAP.GE.NWTH) THEN 
            WRITE(IINFFIL,*)'Run out of weather data at IDAP=',IDAP,' with IPDATE=',IPDATE !AJC-R2
            ISTG=99 !AJC-R2
            CALL HARVEST(IDAP,POT) !AJC-R2
            RETURN  !AJC-R2
         ENDIF
         CALL VPDEST(IDAP)
         IF (POT) THEN
            CALL POTWATBAL(IDAP)
         ELSE
            CALL WATBAL(IDAP)
         ENDIF
         CALL LAI(IDAP)
         IF (YGP_METH=='ASW') THEN  !YGP acts on TRANS, or final yield, or LAI
            TRANS=TRANS*YGP
         ELSEIF (YGP_METH=='LAI') THEN !This would have to be changed for DT.NE.1
            ALAI=RLAI(2)
            RLAI(1)=RLAI(2)
            ALAI=RLAI(2)
            RLAI(2)=RLAI(2)*YGP
         ENDIF
         T_TRANS=T_TRANS+TRANS
         ET=TRANS+EVAP
         
!c       Kathryn Nicklin replaced this line with following 4 lines  11/2/2009 so that ALAI is updated when YGP_METH.NE.LAI
!c       IF (YGP_METH.NE.'LAI') RLAI(1)=RLAI(2)

         IF (YGP_METH.NE.'LAI') THEN
          RLAI(1)=RLAI(2)
          ALAI=RLAI(2)
         ENDIF


         CALL BIOMASS(IDAP,ALAI)
         IF (YGP_METH=='EOS') THEN !YGP acts on TRANS, or final yield, or LAI
            IF (YGP.LE.1.) THEN
               YIELD=YGP*YIELD !Actual yield is (YGP*predicted) in this case
            ELSE
               YIELD=YIELD-YGP !Actual yield is (predicted - YGP) in this case
            ENDIF
         ENDIF
         CALL HARVCHECK(IDAP,POT)
         IF (MOD(IDAP,IOUT).EQ.0.AND.IASCII.GE.2) CALL OUTPUT(FLOAT(IDAP),IDAP,POT) 
         IF (ISTG==4.OR.ISTG==6.OR.ISTG>80) THEN !AJC-R2, kathryn nicklin added .OR. ISTG == 6
            EXIT SEASON
         ENDIF

         !Kathryn Nicklin added replanting routine.
!         IF((IDAP.LT.102).AND.(IDAP.GE.(ISDAY+IEMDAY+1)).AND.(IDAP.LE.(ISDAY+IEMDAY+20)).AND.(LREPLANT.EQ..FALSE.))THEN
!          IF(SWFAC.LE.0.3) NDAYSDROUGHT=NDAYSDROUGHT+1
!           IF(ISPARE2.EQ.1) THEN
!            IF(((IDAP.LE.(ISDAY+IEMDAY+NDSLA)).AND.(NDAYSDROUGHT.GE.(NDSLA-3))).OR.(NDAYSDROUGHT.GE.10))THEN !crop is failed, trigger resowing
!             ISDAY=IDAP+1 !crop is replanted tomorrow
!             WRITE(IINFFIL,*)'crop failed, triggering automatic resowing'
!             LREPLANT=.TRUE.
!             CALL INIT_RUN(ASWS,IHDATE,E_DEPTH,IHYP)
!             GOTO 8
!            ENDIF
!           ELSE IF(ISPARE2.EQ.-1)THEN
!            IF((IDAP.LE.(ISDAY+IEMDAY+NDSLA)).AND.(NDAYSDROUGHT.GE.(NDSLA-3))) THEN
!             ISDAY=IDAP+1 !crop is replanted tomorrow
!             WRITE(IINFFIL,*)'crop failed, triggering automatic resowing. IDAP and IPLANT =',IDAP, IPLANT
!             LREPLANT=.TRUE.
!             CALL INIT_RUN(ASWS,IHDATE,E_DEPTH,IHYP)
!             GOTO 8
!            ENDIF
!           ELSE
!            PRINT*,'can not identify planting routine'
!           ENDIF
!         ENDIF

      ENDDO SEASON

      IF (IASCII==3) WRITE(IINFFIL,*)'Succesful completion'

!C--------------------------------------------
!C      
      END SUBROUTINE GLAM
!C
!C--------------------------------------------
!C 
!C    IDAP is the no of days after planting
!C    ISTG is the growth stage:
!C         0 Planting
!C         1 First flower
!C         2 Pod filling
!C         3 LAI max


      SUBROUTINE HARVCHECK(IDAP,POT)
!C
!C     Determine whether or not harvest occurs at current time step
!C
!C     ------------------------------------------------------------------
!C     Input     : 
!C     In        : 
!C     Out       : 
!C     Output    : 
!C     Called by : Main
!C     Calls     : HARVEST
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      USE IO_VARS !AJC-R2
      IMPLICIT NONE
      INTEGER IDAP
      LOGICAL :: POT

      IF (IDAP.EQ.IHDAP) THEN
         IF (IASCII==3) WRITE(IINFFIL,*)'Management harvest date reached => Harvest'
         ISTG=88 !AJC-R2
         CALL HARVEST(IDAP,POT)
      ELSE IF (IHDAP.EQ.-99) THEN
         IF (TTHARV.GE.GCLMHA) THEN
            IF (IASCII==3) WRITE(IINFFIL,*)'Automatic harvest (thermal time) at ',IDAP,' DAP'
            ISTG=4
            CALL HARVEST(IDAP,POT)
         ELSE IF (ISTG.GE.2.AND.RZSW.LT.SWCRIT.AND.HI>HIMIN.AND.DURFLAG.AND..NOT.POT) THEN !TDS
            IF (IASCII==3) WRITE(IINFFIL,*) 'IDAP= ',IDAP, & !TDS
                 'Root zone soil water has fallen to below the critical value of ',SWCRIT, ' and HI > ',HIMIN,' => Harvest' !TDS
            ISTG=6 !TDS
            CALL HARVEST(IDAP,POT) !TDS
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE HARVCHECK

!C
!C--------------------------------------------
!C--------------------------------------------


      SUBROUTINE HARVEST(IDAP,POT)
!C     Harvest crop (i.e. end simulation)
!C
!C     ------------------------------------------------------------------
!C     Input     : 
!C     In        : 
!C     Out       : 
!C     Output    : 
!C     Called by : HARVCHECK
!C     Calls     : 
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS  
      USE IO_VARS !AJC-R2
      IMPLICIT NONE
      INTEGER IDAP
      LOGICAL :: POT


      IF (IASCII.GE.2) CALL OUTPUT(FLOAT(IDAP),IDAP,POT)
      CLOSE(IFLDAILY)

      IF (IASCII.LE.2) RETURN !AJC-R2

      WRITE(IINFFIL,*)'Harvest date = ',IPDATE,' + ',IDAP,' -1'
      WRITE(IINFFIL,*)
      WRITE(IINFFIL,*)'*********************************'
      WRITE(IINFFIL,*)'Water balance:'
      WRITE(IINFFIL,*)'*********************************'
      WRITE(IINFFIL,10)'Total transpiration          = ',T_TRANS,' cm'
      WRITE(IINFFIL,10)'Total evaporation            = ',T_EVAP,' cm'
      IF (.NOT.POT) THEN
         WRITE(IINFFIL,10)'Total rainfall               = ',T_RAIN,' cm'
         WRITE(IINFFIL,10)'Total runoff                 = ',T_RUN,' cm'
         WRITE(IINFFIL,10)'Total drainage               = ',T_DRAIN,' cm'
         
         WRITE(IINFFIL,10)'Change in soil water content = ',(SUM(VOLSW(1:NSL))-VOLSWS*NSL)*DZ,' cm'
         WRITE(IINFFIL,10)'Net water input              = ', &
              T_RAIN-T_TRANS-T_EVAP-T_RUN-T_DRAIN, ' cm'
         WRITE(IINFFIL,*)
         WRITE(IINFFIL,10)'Total potential uptake = ',TP_UP,' cm'    
         
         
         WRITE(IINFFIL,10)'Total potential trans  = ',TP_TRANS,' cm'
         WRITE(IINFFIL,10)'Total potential evap   = ',TP_EVAP,' cm'
         
         WRITE(IINFFIL,*)
         WRITE(IINFFIL,*)'*********************************'
         WRITE(IINFFIL,*)'Crop status:'
         WRITE(IINFFIL,*)'*********************************'
         WRITE(IINFFIL,10)'LAI         = ',RLAI(2),''
         WRITE(IINFFIL,20)'Biomass     = ',BMASS,' kg/ha'
         WRITE(IINFFIL,20)'Pod yield   = ',YIELD,' kg/ha'
         IF (BMASS>0.) THEN
            WRITE(IINFFIL,10)'HI          = ',YIELD/BMASS,''
         ELSE
            WRITE(IINFFIL,10)'HI          = -99'
         ENDIF
         
      ENDIF
      
      RETURN

10    FORMAT(A,F7.3,A)
20    FORMAT(A,F6.0,A)

      END SUBROUTINE HARVEST




      SUBROUTINE HIGHT(IDAP)
!
!     High temperature sub-model; IDAP is day one of podfill
!     ------------------------------------------------------------------
!     Input     : 
!     In        : 
!     Out       : 
!     Output    : 
!     Called by : LAI
!     Calls     : FINDCRIT,FLWRDISN
!     ------------------------------------------------------------------
!     
      USE HIGHT_VARS   
      IMPLICIT NONE  
      INTEGER :: IDAP

      CALL FLWRDISN(IDAP)
      CALL FINDCRIT(IDAP)      
      CALL IMPACT_DHDT(IDAP)

      END SUBROUTINE HIGHT



!\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

      SUBROUTINE FINDCRIT(IDAP)
!
!     Find critical temperature and  its impact on pods
!     ------------------------------------------------------------------
!     Input     : 
!     In        : 
!     Out       : 
!     Output    : PERCPOD
!     Called by : HIGHT
!     Calls     : 
!     ------------------------------------------------------------------
!     
      USE HIGHT_VARS    
      USE MULTI_VARS
      IMPLICIT NONE
      REAL :: TCRIT(NDAYSYR,MAXEP),TLIM(NDAYSYR,MAXEP)
      REAL :: PP_B(NDAYSYR),PP_A(NDAYSYR),PODP(NDAYSYR,MAXEP)
      INTEGER :: I,J,IEP,IDRA,IDAP,NEP,IEPLOC_B(NDAYSYR),IEPLOC_A(NDAYSYR)
      LOGICAL :: LEXCEED(1:NDAYSYR) !No crop has a duration >NDAYSYR
      LOGICAL :: LEXONE
      REAL*8, EXTERNAL :: S15ABF


      !Find all high temp days that can affect flowering
      LEXCEED=.FALSE.
      LEXONE=.FALSE.
      DO I=IDAPFLWR-IBAMAX,IDAP+IAAMAX+IDURMAX/2
         IF (TAM(I).GE.TCRITMIN) THEN
            LEXCEED(I)=.TRUE.
            LEXONE=.TRUE.
         ENDIF
      ENDDO

      !Continue only if TCRITMIN has been at all exceeded
      IF (LEXONE) THEN
         CONTINUE
      ELSE
         PERCPOD(:)=1.
         RETURN
      ENDIF

      !Define episodes by timing and duration
      IEP=1
      DO I=IDAPFLWR-IBAMAX,IDAP-1 
         ODD: DO J=0,IDURMAX/2-1
            IF (LEXCEED(I+J).AND.LEXCEED(I-J)) THEN 
               EPISODE(IEP)%ITIME=I !Centred time of episode
               EPISODE(IEP)%IDUR=2*J+1  !Duration of episode 
               IEP=IEP+1
            ELSE
               EXIT ODD
            ENDIF
         ENDDO ODD
         EVEN: DO J=0,IDURMAX-1
            IF (MOD(J,2)==0.AND.LEXCEED(I+J)) THEN 
               CONTINUE
            ELSE IF (LEXCEED(I+J)) THEN 
               EPISODE(IEP)%ITIME=I+(J-1)/2 !Time of episode (smaller-t side of centre)
               EPISODE(IEP)%IDUR=J+1  !Duration of episode 
               IEP=IEP+1
            ELSE
               EXIT EVEN
            ENDIF
         ENDDO EVEN
      ENDDO
      NEP=IEP-1
      IF (NEP>MAXEP) THEN
         WRITE(IINFFIL,*)'Max number of hight episodes exceeded ',MAXEP
         STOP
      ENDIF


      !Calculate pre- and post- anthesis pod percentages for each day of crop flowering
      PODP=1.
      TCRIT(:,:)=FLOAT(NODATA)
      TLIM(:,:)=FLOAT(NODATA)
      DO I=IDAPFLWR-IBAMAX,IDAP-1 !For flowering on day I ...
         DO IEP=1,NEP    !.. examine each episode
            IDRA=EPISODE(IEP)%ITIME-I !Days relative to anthesis; -ve=before
            IF (IDRA.LE.0.AND.ABS(IDRA).LE.IBAMAX) THEN
               TCRIT(I,IEP)= 36.   + TCSLOPE * (ABS(IDRA) - 6.)
               TLIM(I,IEP) = TLINT + TLSLOPE * (ABS(IDRA) - 6.)
               TCRIT(I,IEP)=MAX(TCRIT(I,IEP),TCRITMIN)
            ELSE IF (IDRA.GT.0.AND.ABS(IDRA).LE.IDURMAX) THEN
!               TCRIT(I,IEP)=37.8 + (1.1 *IDRA) - 1.7*EPISODE(IEP)%IDUR !Version 1 of eqn 3
               TCRIT(I,IEP)=37.5 + (1.8 *IDRA) - 3*EPISODE(IEP)%IDUR    !Version 2 of eqn 3
               TLIM(I,IEP)= 48.8 + (0.75*IDRA) - 1.5*EPISODE(IEP)%IDUR                  
               TCRIT(I,IEP)=MAX(TCRIT(I,IEP),TCRITMIN)
            ELSE
               GOTO 10 
            ENDIF
            IF (TAM(I+IDRA)>TCRIT(I,IEP)) THEN
               IF (TLIM(I,IEP).LE.TLIMMIN) THEN
                  TLIM=TLIMMIN
               ENDIF
               IF (TAM(I+IDRA)<TLIM(I,IEP)) THEN
                  PODP(I,IEP)=1.-(TAM(I+IDRA)-TCRIT(I,IEP))/(TLIM(I,IEP)-TCRIT(I,IEP))
               ELSE
                  PODP(I,IEP)=0.
               ENDIF                     
            ENDIF
10          CONTINUE
         ENDDO
      ENDDO

      !Sum the impacts of the biggest (1)pre- and (2)post- anthesis events
      PP_B=1.
      PP_A=1.
      IEPLOC_A(:)=NODATA
      IEPLOC_B(:)=NODATA
      DO I=IDAPFLWR-IBAMAX,IDAP-1 ! Ann inserted -IBAMAX as otherwise the first six events would be omitted
         DO IEP=1,NEP
            IDRA=EPISODE(IEP)%ITIME-I
            IF (IDRA.LE.0) THEN
               IF (PODP(I,IEP)<PP_B(I)) THEN
                  PP_B(I)=PODP(I,IEP)
                  IEPLOC_B(I)=IEP              
               ENDIF
            ELSE
               IF (PODP(I,IEP)<PP_A(I)) THEN
                  PP_A(I)=PODP(I,IEP)   
                  IEPLOC_A(I)=IEP              
               ENDIF
            ENDIF
         ENDDO
         PERCPOD(I)=MIN(PP_B(I),PP_A(I))
      ENDDO




      END SUBROUTINE FINDCRIT

!\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

      SUBROUTINE FLWRDISN(IDAP)
!
!     Calculate flowering distibution
!     ------------------------------------------------------------------
!     Input     : 
!     In        : 
!     Out       : 
!     Output    : FLWRDIS
!     Called by : HIGHT
!     Calls     : 
!     ------------------------------------------------------------------
      USE HIGHT_VARS
      USE IO_VARS
      USE MULTI_VARS
      IMPLICIT NONE
      INTEGER :: IDAP,I,IFAIL,SWITCH,IWRITE,JWRITE,WHICHYR
      REAL :: FLWRDISMATRIX(365.,24.)
      REAL*8 :: X,FLWRDUR
      REAL :: NORMMIN,NORMRANGE,RNORM
      REAL*8, EXTERNAL :: S15ABF
      INTEGER, PARAMETER :: IFLFLWRDIS=140 ! FLWRDIS data file

      IFAIL=0
      FLWRDIS=0.

      SWITCH=2 !The SWITCH function allows running simulations with either the new (correct) representation of cumulative flower distribution ('SWITCH=2') or with the old representation ('SWITCH=1'). Note that whatever value 'SWITCH' is set to in this subroutine, it must be set to in the 'IMPACT_DHDT' subroutine as well to match the accompanying alternatives (Brak, Dec 2010)

      FLWRDUR=FLOAT(IDAP-1-IDAPFLWR)
      DO I=IDAPFLWR,IDAP-1
         X=FDWIDTH * (FLOAT(I-IDAPFLWR)/FLWRDUR-0.5D0+ FDOFFSET)
         FLWRDIS(I)=S15ABF(X,IFAIL)
      ENDDO

      IF (SWITCH.EQ.1) THEN
	     RNORM=SUM(FLWRDIS(IDAPFLWR:IDAP-1))
	     DO I=IDAPFLWR,IDAP-1
	        FLWRDIS(I)=FLWRDIS(I)/RNORM
	     ENDDO
      ELSEIF (SWITCH.EQ.2) THEN
	     NORMRANGE=FLWRDIS(IDAP-1)-FLWRDIS(IDAPFLWR)
	     NORMMIN=FLWRDIS(IDAPFLWR)
	
	  DO I=IDAPFLWR,IDAP-1
	     FLWRDIS(I)=(FLWRDIS(I)-NORMMIN)/NORMRANGE
	  ENDDO
      ENDIF

!     Alternatively: constant flowering distribution (if have no nag routines)
!      DO I=IDAPFLWR,IDAP-1 
!         FLWRDIS(I)=1.
!      ENDDO
!      RNORM=SUM(FLWRDIS(IDAPFLWR:IDAP-1))
!      DO I=IDAPFLWR,IDAP-1 
!         FLWRDIS(I)=FLWRDIS(I)/RNORM
!      ENDDO

      ! to save FLWRDIS
      ! PRINT*,IYR
      WHICHYR=IYR-1965.
      DO IWRITE=1,365.
         FLWRDISMATRIX(IWRITE,WHICHYR)=FLWRDIS(IWRITE)
      ENDDO

      IF (IYR .EQ. 1989.) THEN
         OPEN(UNIT=IFLFLWRDIS,FILE='FLWRDISfile.txt')
         DO JWRITE=1,24.
            DO IWRITE=1,365.
               WRITE(IFLFLWRDIS,*)FLWRDISMATRIX(IWRITE,JWRITE)
            ENDDO
         ENDDO
         CLOSE(IFLFLWRDIS)
      ENDIF

      END SUBROUTINE FLWRDISN

!\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

      SUBROUTINE IMPACT_DHDT(IDAP)
!
!     Calculate impact of pod-percentage on DHDT
!     ------------------------------------------------------------------
!     Input     : PERCPOD
!     In        : 
!     Out       : 
!     Output    : 
!     Called by : HIGHT
!     Calls     : 
!     ------------------------------------------------------------------
      USE HIGHT_VARS
      USE MULTI_VARS
      IMPLICIT NONE
      INTEGER :: IDAP,I,IDAPHARV,SWITCH,IWRITE,JWRITE,WHICHYR
      REAL :: PERCPODMATRIX(365.,24.)
      REAL :: FLWR_WS,RI1,RI2,TNP,TPOD,TPOD1
      INTEGER, PARAMETER :: IFLPERCPOD=150 ! PERCPOD data file
      TOTPP=0.
      TOTPP_WAT=0.
      TOTPP_HIT=0.
      RI1=0.
      RI2=0.

      SWITCH=2 !The SWITCH function allows running simulations with either the new (correct) representation of cumulative flower distribution in FLWRDISN ('SWITCH=2') or with the old representation ('SWITCH=1'). Note that whatever value 'SWITCH' is set to in this subroutine, it must be set to in the 'FLWRDISN' subroutine as well to match the accompanying alternatives (Brak, Dec 2010)


      ! to save PERCPOD
      ! PRINT*,IYR
      WHICHYR=IYR-1965.
      DO IWRITE=1,365.
         PERCPODMATRIX(IWRITE,WHICHYR)=PERCPOD(IWRITE)
      ENDDO

      IF (IYR .EQ. 1989.) THEN
         OPEN(UNIT=IFLPERCPOD,FILE='PERCPODfile.txt')
         DO JWRITE=1,24.
            DO IWRITE=1,365.
               WRITE(IFLPERCPOD,*)PERCPODMATRIX(IWRITE,JWRITE)
            ENDDO
         ENDDO
         CLOSE(IFLPERCPOD)
      ENDIF



      CALL FINDHARV(IDAP,IDAPHARV)
      TPOD1= IDAPHARV-IDAP+1   !Time that first flower's pod has for podfill 
      DO I=IDAPFLWR,IDAP-1
         TPOD= IDAPHARV-IDAP+1-(I-IDAPFLWR) !Time that flower i's pod has for podfill
         TNP=TPOD/TPOD1 !Normalised duration of podfill for i
         IF (SWFF_THR>TESTNODATA) THEN

             IF (SWITCH.EQ.1) THEN
               FLWR_WS=FLWRDIS(I) * MIN(1.,SWFAC/SWFF_THR)
             ELSEIF (SWITCH.EQ.2) THEN
                IF (I.EQ.IDAPFLWR) THEN
                   FLWR_WS=FLWRDIS(I) * MIN(1.,SWFAC/SWFF_THR)
                ELSE
                   FLWR_WS=(FLWRDIS(I)-FLWRDIS(I-1)) * MIN(1.,SWFAC/SWFF_THR)
                ENDIF
             ENDIF
         ELSE
             IF (SWITCH.EQ.1) THEN
                FLWR_WS=FLWRDIS(I)
             ELSEIF (SWITCH.EQ.2) THEN
                IF (I.EQ.IDAPFLWR) THEN
                   FLWR_WS=FLWRDIS(I)
                ELSE
                   FLWR_WS=(FLWRDIS(I)-FLWRDIS(I-1))
                ENDIF
             ENDIF
         ENDIF
         TOTPP_WAT = TOTPP_WAT + FLWR_WS !Water-stress-only time-integrated perc pods
         IF (SWITCH.EQ.1) THEN
            TOTPP_HIT = TOTPP_HIT + PERCPOD(I) * FLWRDIS(I) !High temp only time-integrated perc pods
         ELSEIF (SWITCH.EQ.2) THEN
            IF (I.EQ.IDAPFLWR) THEN
               TOTPP_HIT = TOTPP_HIT + PERCPOD(I) * FLWRDIS(I)
            ELSE
               TOTPP_HIT = TOTPP_HIT + PERCPOD(I) * (FLWRDIS(I)-FLWRDIS(I-1))
            ENDIF
         ENDIF
         TOTPP = TOTPP + PERCPOD(I) * FLWR_WS !Total-time integrated perc pods
         RI1=RI1+FLWR_WS*PERCPOD(I)*TNP
         RI2=RI2+FLWR_WS*TNP
      ENDDO

      IF (TOTPP<PPCRIT) THEN
         DHDT_HT = DHDT * (1.-(PPCRIT-TOTPP)/PPCRIT) 
      ELSE
         DHDT_HT = DHDT
      ENDIF

      RETURN
      END SUBROUTINE IMPACT_DHDT



      SUBROUTINE FINDHARV(IDAP,IDAPHARV)
      !Calculate harvest date, as needed for SR IMPACT_DHDT
      USE HIGHT_VARS
      USE IO_VARS !AJC-R2
      IMPLICIT NONE
      INTEGER :: IDAP,IDAPDUM,ISTGDUM,IDAPHARV
      REAL :: TTDUM

      IDAPHARV=NWTH !AJC-R2
      TTDUM=TTLMAX
      ISTGDUM=2
      TTDUM=0.
      CML: DO IDAPDUM=IDAP,NDAYSYR,IDT 
         IF (ISTGDUM==2) THEN
            CALL TTCALC(IDAPDUM,TTDUM,TBLMAX,TOLMAX,TMLMAX) 
            IF (TTDUM.GE.GCPFLM) THEN
               ISTGDUM=3
               TTDUM=0.
            ENDIF
         ELSEIF (ISTGDUM==3) THEN        
            CALL TTCALC(IDAPDUM,TTDUM,TBHARV,TOHARV,TMHARV) 
            IF  (TTDUM.GE.GCLMHA) THEN
               IDAPHARV=IDAPDUM
               EXIT CML
            ENDIF
         ENDIF
      ENDDO CML

      END SUBROUTINE FINDHARV












      SUBROUTINE LAI(IDAP)
!C     Contains equasion for time evolution of LAI
!C     Also updates growth stage variable, ISTG
!C     For explanation of variable names, see bottom of file
!C
!C     ------------------------------------------------------------------
!C     Input     : SWFA!C,DLDTMX,weather,cardinal temperatures
!C     In        : 
!C     Out       : 
!C     Output    : TT,RLAI,ISTG
!C     Called by : Main
!C     Calls     : TTCALC
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      USE HIGHT_VARS
      USE IO_VARS !AJC-R2
      IMPLICIT NONE
      REAL :: DLDT,RLAIFAC,TEMPSLA
      INTEGER :: IDAP
!C

      RLAIFAC=MIN(1.,SWFAC/SWF_THRESH)

      IF (ISTG.EQ.0) THEN
         IF(TTFLWR.LT.GCPLFL) THEN
            CALL TTCALC(IDAP,TTFLWR,TBFLWR,TOFLWR,TMFLWR) 
         ELSE
            ISTG=1
            CALL TTCALC(IDAP,TTPODF,TBPODF,TOPODF,TMPODF) 
            IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' Flowering'         
            IF (LHIGHT) IDAPFLWR=IDAP
         ENDIF
         DLDT=DLDTMX*RLAIFAC
      ELSE IF (ISTG.EQ.1) THEN
         IF (TTPODF.LT.GCFLPF) THEN
            CALL TTCALC(IDAP,TTPODF,TBPODF,TOPODF,TMPODF) 
            DLDT=DLDTMX*RLAIFAC
         ELSE 
            ISTG=2
            CALL TTCALC(IDAP,TTLMAX,TBLMAX,TOLMAX,TMLMAX) 
            IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' Pod filling starts'
            IF (LHIGHT) CALL HIGHT(IDAP)
         ENDIF
         DLDT=DLDTMX*RLAIFAC
      ELSE IF (ISTG==2) THEN
         IF (TTLMAX.LT.GCPFLM) THEN
            CALL TTCALC(IDAP,TTLMAX,TBLMAX,TOLMAX,TMLMAX) 
            DLDT=DLDTMX*RLAIFAC
         ELSE 
            ISTG=3
            CALL TTCALC(IDAP,TTHARV,TBHARV,TOHARV,TMHARV) 
            IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' Max value of LAI reached'
            DLDT=0.
            IDCNT=IDAP
         ENDIF
      ELSE IF (ISTG.EQ.3) THEN
         CALL TTCALC(IDAP,TTHARV,TBHARV,TOHARV,TMHARV) 
         DLDT=0.
      ENDIF

      RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)

      IF (LSLA) THEN
         !For days NDSLA+1 to end of LAI growth period, limit LAI growth using SLA_INI
         IF ((IDAP-ISDAY-IEMDAY+1)>NDSLA.AND.ISTG.LE.2) THEN
            IF ((BMASS-YIELD)>0.) THEN
               TEMPSLA=10**5*RLAI(2)/(BMASS-YIELD) !SLA associated with RLAI(2)
            ELSE
               TEMPSLA=9999. !This will get decreased below
            ENDIF
            IF (TEMPSLA>SLA_INI) RLAI(2)=MAX(RLAI(1),SLA_INI*(BMASS-YIELD)/10**5) !LAI cte, SLA up
            !IF (TEMPSLA>SLA_INI) RLAI(2)=SLA_INI*(BMASS-YIELD)/10**5 !LAI can fall, SLA cte
         ENDIF
         IF ((RLAI(2)-RLAI(1)<-1.*TOLLAI)) WRITE(IINFFIL,*)'SR LAI: LAI decrease>TOLLAI=',TOLLAI, & 
              ' found at ',ISTG,IDAP,BMASS,YIELD,RLAI(1),RLAI(2),RLAI(2)-RLAI(1)
      ENDIF

      RETURN
      END SUBROUTINE LAI

!C     SWFAC is the water stress index (pot. uptake/pot. trans.)
!C     DLDT is d(LAI)/dt
!C     RLAI(2) is the mewly calculated LAI


!C     ------------------------------------------------------------------
!C     The single subroutine below is taken from the original watbal.f90
!C      because it modifies the root profile, which is crop-dependant.
!C     ------------------------------------------------------------------
!C
      SUBROUTINE PUPTAKE(RLV,IDAP)
!C    Potential uptake. Corrected for GLAM-R2
!C
!C     ------------------------------------------------------------------
!C     Input     : VOLSW(NSL)
!C     In        : 
!C     Out       : 
!C     Output    : DTPUPTK, TP_UP, DPUPTK(NSL), RLV(NSL)
!C     Local     : IZ,Z,EXPNT
!C     Called by : WATBAL
!C     Calls     : 
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      IMPLICIT NONE
      INTEGER :: IZ
      REAL :: EXPNT,RLVTOP,DLV_DZ,DLV,RLV(MAXNSL)
      INTEGER :: IDAP


!C   Calcute depth of Exraction Front
      IF (ISTG==3.AND.I_R==1) THEN 
         ZEF(IDAP)=ZEF(IDAP-1)
      ELSE
         ZEF(IDAP)=ZEF(IDAP-1)+EFV*FLOAT(IDT)
      ENDIF
      IF (ZEF(IDAP).GT.ZSMAX) THEN
         WRITE(IINFFIL,*)'Watbal: Extraction front = ',ZEF(IDAP), ' > ZSMAX =',ZSMAX,' => have set ZEF(IDAP)=ZSMAX'
         ZEF(IDAP)=ZSMAX
      ENDIF
 
      IF (IUPT==0) THEN
         !C   Set ITEXP (time from start of exp. SW decrease).
         DO IZ=1,NSL
            ROOTZONE: IF (Z(IZ).LE.ZEF(IDAP)) THEN
               NEWLAYER: IF (ITEXP(IZ).EQ.NODATA) THEN 
                  ITEXP(IZ)=1
               ELSE 
                  IF(VOLSW(IZ)<UPTOL) THEN
                     ITEXP(IZ)=ITEXP(IZ)+1
                  ELSE
                     ITEXP(IZ)=1
                     PUPTK(IZ,1)=0. !So that DPUPTK comes out right
                  ENDIF
               ENDIF NEWLAYER
            ENDIF ROOTZONE
         ENDDO
         !C   Calculate potential uptake profile
         RLVTOP=DLDLAI*RLAI(2)+RLVEF !Lv at Z=ZSMIN
         IF (ZEF(IDAP).GT.ZSMIN) THEN
            DLV_DZ=(RLVEF-RLVTOP)/(ZEF(IDAP)-ZSMIN)
         ELSE
            DLV_DZ=0.
         ENDIF
         RLA=0.
         RLV_MEAN=0.
         DO IZ=1,NSL
            IF (ITEXP(IZ).GE.1) THEN   
               DLV=DLV_DZ*(Z(IZ)-ZSMIN)
               RLV(IZ)=RLVTOP+DLV !l_v as a function of z
               EXPNT=RLV(IZ)*FLOAT(ITEXP(IZ))*(-1.)*UPDIFC
               if (Z(IZ).GT.ZEF(IDAP)) then
                  WRITE(IINFFIL,*)'Watbal: z(iz).gt.zef',iz,z(iz),zef(IDAP),itexp(iz),expnt
                  stop
               endif
               if (expnt.gt.0) then
                  WRITE(IINFFIL,*)'Watbal: uptake exponent > 0'
                  WRITE(IINFFIL,*)expnt,rlvtop,dlv_dz,dlv,rlai(2)
                  stop
               endif
               PUPTK(IZ,2)=UPTOL*(1.-EXP(EXPNT))*DZ
               if ((PUPTK(IZ,2)+TOLSOL)<PUPTK(IZ,1))WRITE(IINFFIL,*) & 
                    idap,iz,'WATBAL: Potential uptake decrease',&
                    PUPTK(IZ,2),PUPTK(IZ,1)
               DPUPTK(IZ)=MIN(PUPTK(IZ,2)-PUPTK(IZ,1),(VOLSW(IZ)-RLL)*DZ)
               RLA=RLA+RLV(IZ)*DZ
               RLV_MEAN=RLV_MEAN+RLV(IZ)
            ELSE
               PUPTK(IZ,2)=0.
               RLV(IZ)=0.
            ENDIF
         ENDDO
         RLV_MEAN=RLV_MEAN/FLOAT(NSL)
         
      ELSE
         RLVTOP=DLDLAI*RLAI(2)+RLVEF !Lv at Z=ZSMIN
         IF (ZEF(IDAP).GT.ZSMIN) THEN
            DLV_DZ=(RLVEF-RLVTOP)/(ZEF(IDAP)-ZSMIN)
         ELSE
            DLV_DZ=0.
         ENDIF
         RLA=0.
         RLV_MEAN=0.
         DO IZ=1,NSL
            IF (Z(IZ).LE.ZEF(IDAP)) THEN
               DLV=DLV_DZ*(Z(IZ)-ZSMIN)
               RLV(IZ)=RLVTOP+DLV !l_v as a function of z
               RLA=RLA+RLV(IZ)*DZ
               RLV_MEAN=RLV_MEAN+RLV(IZ)            
               EXPNT=RLV(IZ)*(-1.)*UPDIFC
               DPUPTK(IZ)=(VOLSW(IZ)-RLL)*(1.-EXP(EXPNT))*DZ
            ELSE
               RLV(IZ)=0.
               DPUPTK(IZ)=0.
            ENDIF
         ENDDO         
      ENDIF
      
!C    Sum uptake over all levels and time-steps
      DTPUPTK=SUM(DPUPTK(1:NSL))!Incremental total potential uptake for all soil
      TP_UP=TP_UP+DTPUPTK       !Cumulative total potential uptake
      PUPTK(1:NSL,1)=PUPTK(1:NSL,2)

      RETURN
      END SUBROUTINE PUPTAKE



!C-----------------------------------------------------------------------
!C     SRs to calc thermal time and VPD
!C-----------------------------------------------------------------------


      SUBROUTINE TTCALC(I,TT,TB,TO,TM) 
!C     Calculate thermal time
!C     Inputs 2-5 are dummy variables
!C
!C     ------------------------------------------------------------------
!C     Input     : IDPA,weather,cardinal temperatures
!C     In        : 
!C     Out       : 
!C     Output    : TT
!C     Local     : TB,TO,TM,TT
!C     Called by : LAI,INIT
!C     Calls     : 
!C     ------------------------------------------------------------------
!C
      USE GLOBAL_VARS
      IMPLICIT NONE
      
      REAL :: TO,TB,TM,TEFF,TT
      INTEGER :: I
!C
      IF (TBAR(I).LT.TO.AND.TBAR(I).GT.TB) THEN
        TEFF=TBAR(I)-TB
      ELSE IF(TBAR(I).GE.TO.AND.TBAR(I).LT.TM) THEN
        TEFF=TO-TB-((TBAR(I)-TO)/(TM-TO))*(TO-TB)
      ELSE IF (TBAR(I).LT.TB.OR.TBAR(I).GT.TM) THEN
        TEFF=0.
      ENDIF
      TT=TT+TEFF*FLOAT(IDT)
!C
      RETURN
      END SUBROUTINE TTCALC
!C
!C     TB, TO and TM are base, optimum and maximum cardinal temperatures




