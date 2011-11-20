!     Fortran90 subroutines for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2009.
!     Copyright The University of Reading and the University of Leeds 2009. All rights reserved.

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
      REAL :: TRANS_KG,ALAI
      INTEGER :: IDAP

!C    Harvest Index
      IF (ISTG.GE.3) THEN
         IF (ISPARE1==1.AND.RLAI(2)<RLAITEST) THEN
            CONTINUE
         ELSE
            IF (LHIGHT) THEN 
               HI=HI+DHDT_HT*FLOAT(IDT)
            ELSE
               HI=HI+DHDT*FLOAT(IDT)
            ENDIF
         ENDIF
      ELSE
         HI=0.
      ENDIF
      TRANS_KG=TRANS/100.*RHO_W !Trans in kg/m^2
      BMASS=BMASS+MIN(TEN_MAX,TE/VPD)*TRANS_KG*FLOAT(IDT)*10.
      YIELD=HI*BMASS

      !Calculate SLA
      IF (BMASS-YIELD>0.) SLA=10**5*ALAI/(BMASS-YIELD)

      IF (LSLA) THEN
         !For first NDSLA days after IEMDAY, fix SLA to SLA_INI
         IF ((IDAP-ISDAY-IEMDAY+1).LE.NDSLA.AND.SLA>SLA_INI) THEN
            BMASS=10**5*ALAI/SLA_INI
            SLA=SLA_INI
         ENDIF
      ENDIF
          
      RETURN
      END SUBROUTINE BIOMASS







      SUBROUTINE GLAM(POT,ASWS,IHDATE,E_DEPTH,IDAP) !AJC-R2
!     Main code block for General Large-Area Model for annual crops
!     Release 1, created from v2A.201 in Oct 2005.
!     Documentation: glam-r1.doc 

      USE GLOBAL_VARS 
      USE IO_VARS
      USE WWINTER_VARS
      IMPLICIT NONE
      INTEGER :: IHDATE,IDAP
      REAL    :: ASWS,E_DEPTH,ALAI !Latter is actual LAI (not reduced by YGP)
      LOGICAL :: POT

      IF (LHIGHT) THEN 
         WRITE(IINFFIL,*)'HTS switched off - winter wheat version not developed yet.'
         LHIGHT=.FALSE.
      ENDIF

!      Pre-start day calculations 
      IF (LINT) THEN !Intelligent planting routine used
         IF (POT) THEN
            ISDAY=1
         ELSE               
            FINDSOW: DO IDAP=1,-1*ISDAY,IDT
               CALL VPDEST(IDAP)
               CALL WATBAL(IDAP)
               IF ((VOLSW(1)-RLL).GE.FSWSOW*(DUL-RLL)) GOTO 7 !Intelligent planting acheived
            ENDDO FINDSOW
            WRITE(IINFFIL,*)'(intelligent) emergency planting date on ',ISDAY
            IDAP=IDAP-1 !Because IDAP ends up one-over otherwise
7           ISDAY=IDAP+1
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

      !Pre-emergence loop
      DO IDAP=ISDAY,ISDAY+IEMDAY-1,IDT 
         CALL VPDEST(IDAP)
         IF (POT) THEN
            CALL POTWATBAL(IDAP)
         ELSE
            CALL WATBAL(IDAP)
         ENDIF
         CALL TTCALC(IDAP,TTTSFL,TBTSFL,TOTSFL,TMTSFL)
         IF (TTTSFL>GCTSFL) THEN
            WRITE(IINFFIL,*)'Flowering occurs before IEMDAY!'
            STOP
         ENDIF
         SLA=0. 
         IF (MOD(IDAP,IOUT).EQ.0.AND.IASCII.GE.2) CALL OUTPUT(FLOAT(IDAP),IDAP,POT) 
      ENDDO
      
      !Run rest of season
      SEASON: DO IDAP=ISDAY+IEMDAY,NDAYSYR,IDT 
         IF (IDAP.GE.NWTH) THEN
            WRITE(IINFFIL,*)'Run out of weather data.',IDAP,NWTH !AJC-R2
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
         ELSE IF (YGP_METH=='LAI') THEN !This would have to be changed for DT.NE.1
            ALAI=RLAI(2)
            RLAI(1)=RLAI(2)
            RLAI(2)=RLAI(2)*YGP
         ENDIF
         T_TRANS=T_TRANS+TRANS
         ET=TRANS+EVAP
         IF (YGP_METH.NE.'LAI') RLAI(1)=RLAI(2)
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
         IF (ISTG==5.OR.ISTG>80) THEN !AJC-R2
            EXIT SEASON
         ENDIF
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
      USE WSPRING_VARS
      USE WWINTER_VARS
      USE IO_VARS !AJC-R2
      IMPLICIT NONE
      INTEGER IDAP
      LOGICAL :: POT

      IF (IDAP.EQ.IHDAP) THEN
         IF (IASCII==3) WRITE(IINFFIL,*)'Management harvest date reached => Harvest'
         ISTG=88 !AJC-R2
         CALL HARVEST(IDAP,POT)
      ELSE IF (IHDAP.EQ.-99) THEN
         IF (TTHARV.GE.GCENHA) THEN
            IF (IASCII==3) WRITE(IINFFIL,*)'Automatic harvest (thermal time) at ',IDAP,' DAP'
            ISTG=5
            CALL HARVEST(IDAP,POT)
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
      USE IO_VARS
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
         WRITE(IINFFIL,20)'Grain yield   = ',YIELD,' kg/ha'
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
      IMPLICIT NONE
      REAL :: TCRIT(NDAYSYR,MAXEP),TLIM(NDAYSYR,MAXEP)
      REAL :: PP_B(NDAYSYR),PP_A(NDAYSYR),PODP(NDAYSYR,MAXEP)
      INTEGER :: I,J,IEP,IDRA,IDAP,NEP,IEPLOC_B(NDAYSYR),IEPLOC_A(NDAYSYR)
      LOGICAL :: LEXCEED(1:NDAYSYR) !No crop has a duration >NDAYSYR
      LOGICAL :: LEXONE

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
      DO I=IDAPFLWR,IDAP-1
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
      IMPLICIT NONE
      INTEGER :: IDAP,I
      REAL :: RNORM
      FLWRDIS=0.

      DO I=IDAPFLWR,IDAP-1 
         FLWRDIS(I)=1.
      ENDDO

      RNORM=SUM(FLWRDIS(IDAPFLWR:IDAP-1))
      DO I=IDAPFLWR,IDAP-1 
         FLWRDIS(I)=FLWRDIS(I)/RNORM
      ENDDO

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
      IMPLICIT NONE
      INTEGER :: IDAP,I,IDAPHARV
      REAL :: FLWR_WS,RI1,RI2,TNP,TPOD,TPOD1
      TOTPP=0.
      TOTPP_WAT=0.
      TOTPP_HIT=0.
      RI1=0.
      RI2=0.

      CALL FINDHARV(IDAP,IDAPHARV)
      TPOD1= IDAPHARV-IDAP+1   !Time that first flower's pod has for podfill 
      DO I=IDAPFLWR,IDAP-1
         TPOD= IDAPHARV-IDAP+1-(I-IDAPFLWR) !Time that flower i's pod has for podfill
         TNP=TPOD/TPOD1 !Normalised duration of podfill for i
         IF (SWFF_THR>TESTNODATA) THEN
            FLWR_WS=FLWRDIS(I) * MIN(1.,SWFAC/SWFF_THR)
         ELSE
            FLWR_WS=FLWRDIS(I)
         ENDIF
         TOTPP_WAT = TOTPP_WAT + FLWR_WS !Water-stress-only time-integrated perc pods
         TOTPP_HIT = TOTPP_HIT + PERCPOD(I) * FLWRDIS(I) !High temp only time-integrated perc pods
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
      USE WSPRING_VARS
      USE WWINTER_VARS
      USE IO_VARS !AJC-R2
      IMPLICIT NONE
      INTEGER :: IDAP,IDAPDUM,ISTGDUM,IDAPHARV
      REAL :: TTDUM

      IDAPHARV=NWTH !AJC-R2
      TTDUM=TTLMAX
      ISTGDUM=3
      TTDUM=0.
      CML: DO IDAPDUM=IDAP,NDAYSYR,IDT 
         IF (ISTGDUM==3) THEN
            CALL TTCALC(IDAPDUM,TTDUM,TBLMAX,TOLMAX,TMLMAX) 
            IF (TTDUM.GE.GCPFEN) THEN
               ISTGDUM=4
               TTDUM=0.
            ENDIF
         ELSEIF (ISTGDUM==4) THEN        
            CALL TTCALC(IDAPDUM,TTDUM,TBHARV,TOHARV,TMHARV) 
            IF  (TTDUM.GE.GCENHA) THEN
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
     USE WWINTER_VARS
     USE WSPRING_VARS
     IMPLICIT NONE
     REAL :: DLDT,RLAIFAC,TEMPSLA!,PESW,LAIM,RGR,CM,RGRM,TMEAN,LA
     INTEGER ::IDAP

     RLAIFAC=MIN(1.,SWFAC/SWF_THRESH)
      
     IF (ISTG.EQ.0) THEN
        CALL TTCALC(IDAP,TTPLTS,TBPLTS,TOPLTS,TMPLTS) 
        IF(TTPLTS.LT.GCPLTS) THEN
           CALL TTCALC(IDAP,TTPLTS,TBPLTS,TOPLTS,TMPLTS)     
        ELSE
           ISTG=1
           CALL TTCALC(IDAP,TTTSFL,TBTSFL,TOTSFL,TMTSFL) 
           IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' Terminal spikelet' 
           VN=IDAP
!           IF (LHIGHT) IDAPFLWR=IDAP !AJC
        ENDIF
     
        IF (TBAR(IDAP).LT.0)THEN
           DLDT=0
        ELSE
           DLDT=RLAIFAC*DLDTMX/RLAIFLT*TBAR(IDAP) !AJC change for save8/. Has associate change in parameter file - RLAIFLT is now 26 for the control run, and not 0.00385
        ENDIF
        RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)  
        
     ELSE IF (ISTG.EQ.1) THEN
        IF (TTTSFL.LT.GCTSFL) THEN
           CALL TTCALC(IDAP,TTTSFL,TBTSFL,TOTSFL,TMTSFL) 
        ELSE 
           ISTG=2
           CALL TTCALC(IDAP,TTPODF,TBPODF,TOPODF,TMPODF) 
           IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' Flowering starts'
!           IF (LHIGHT) CALL HIGHT(IDAP) !AJC
        ENDIF
        DLDT=DLDTMX* RLAIFAC
        RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)	
		   				
     ELSE IF (ISTG.EQ.2) THEN
        IF (TTPODF.LT.GCFLPF) THEN
           CALL TTCALC(IDAP,TTPODF,TBPODF,TOPODF,TMPODF) 
        ELSE 
           ISTG=3
           CALL TTCALC(IDAP,TTLMAX,TBLMAX,TOLMAX,TMLMAX) 
           IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' Grain filling starts'
!           IF (LHIGHT) CALL HIGHT(IDAP) !AJC
        ENDIF
        DLDT=-WWSPA1
        RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)
        
     ELSE IF (ISTG==3) THEN
        IF (TTLMAX.LT.GCPFEN) THEN
           CALL TTCALC(IDAP,TTLMAX,TBLMAX,TOLMAX,TMLMAX) 
           DLDT=-WWSPA1
           RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)
        ELSE 
           ISTG=4
           CALL TTCALC(IDAP,TTHARV,TBHARV,TOHARV,TMHARV) 
           IF (IASCII==3) WRITE(IINFFIL,*)'IDAP = ',IDAP,' End of grain filling'  
           DLDT=-WWSPA1
           RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)
        ENDIF
        
     ELSE IF (ISTG.EQ.4) THEN
        CALL TTCALC(IDAP,TTHARV,TBHARV,TOHARV,TMHARV) 
        DLDT=-WWSPA1
        RLAI(2)=RLAI(1)+DLDT*FLOAT(IDT)
     ENDIF

     IF (RLAI(2)<RLAITEST) THEN 
        IF (ISPARE1==-1) THEN 
           WRITE(IINFFIL,*)'SR LAI: IDAP= ',IDAP,' LAI=0 and HI is continuing to increase (set ISPARE1=+1 to change this)'
        ELSE IF (ISPARE1==+1) THEN
           WRITE(IINFFIL,*)'SR LAI: IDAP= ',IDAP,' LAI=0 and so DHDT set to zero.'
           WRITE(IINFFIL,*)'If this period of zero LAI is too long, then consider a slower senescence.'
        ENDIF
     ENDIF

     IF (RLAI(2).lt.0) RLAI(2)=0 

     IF (LSLA) THEN
        !For days NDSLA+1 to end of LAI growth period, limit LAI growth using SLA_INI
        IF ((IDAP-ISDAY-IEMDAY+1)>NDSLA.AND.ISTG.LE.3) THEN
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
      REAL, SAVE :: RLVTOPM !AJC-Leeds


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
         SELECT CASE(ISTG)
         CASE(0,1)
            RLVTOP=DLDLAI*RLAI(2)+RLVEF			
            RLVTOPM=RLVTOP
         CASE(2,3,4,5)
            RLVTOP=RLVTOPM
         END SELECT
         ! RLVTOP=DLDLAI*RLAI(2)+RLVEF !Lv at Z=ZSMIN
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
               if ((PUPTK(IZ,2)+TOLSOL)<PUPTK(IZ,1)) & 
                    WRITE(IINFFIL,*)idap,iz,'WATBAL: Potential uptake decrease',PUPTK(IZ,2),PUPTK(IZ,1)
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
         SELECT CASE(ISTG)
         CASE(0,1)
            RLVTOP=DLDLAI*RLAI(2)+RLVEF			
            RLVTOPM=RLVTOP
         CASE(2,3,4,5)
            RLVTOP=RLVTOPM
         END SELECT
         !RLVTOP=DLDLAI*RLAI(2)+RLVEF !Lv at Z=ZSMIN
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
      USE WWINTER_VARS
      IMPLICIT NONE
        
      REAL:: TO,TB,TM,TEFF,TT,DEC,DLV,PHOT,RPE
      INTEGER :: I,DAY,N
      !INTEGER, PARAMETER ::N=8
      REAL::TFAC,TEMP,RTE,FTTS,DFT,FTDR!,PS,VS,FTHD
      FTDR=FTDRFAC/EN  !thermal time requirement for double ridge
      FTTS=FTTSFAC/EN  ! thermal time requirement for terminal spikelet. 

      DO N=1,8 
         TFAC=0.931+0.114*N-0.0703*N**2.+0.0053*N**3
         TEMP=TMIN(I)+TFAC*(TMAX(I)-TMIN(I))  ! TEMP eight 3-h temperature 
         IF (TEMP.LT.TO.AND.TEMP.GT.TB) THEN
            RTE=(TEMP-TB)/(TO-TB) !  RTE Relative thermal effectiveness
         ELSE IF(TEMP.GE.TO.AND.TEMP.LT.TM) THEN
            RTE=(TM-TEMP)/(TM-TO)
         ELSE IF (TEMP.LT.TB.OR.TEMP.GT.TM) THEN
            RTE=0.
         ENDIF
         DTE=DTE+RTE*FLOAT(IDT)    ! DTE Daily thermal effectiveness 

      ENDDO
     
      IF (TBAR(I).LT.TO.AND.TBAR(I).GT.TB) THEN
         TEFF=TBAR(I)-TB
      ELSE IF(TBAR(I).GE.TO.AND.TBAR(I).LT.TM) THEN
         TEFF=TO-TB-((TBAR(I)-TO)/(TM-TO))*(TO-TB)
      ELSE IF (TBAR(I).LT.TB.OR.TBAR(I).GT.TM) THEN
         TEFF=0.
      ENDIF
      
      
      SELECT CASE(ISTG)
      CASE(0)
         IF (TBAR(I).GT.(0.).and.(TBAR(I).LT.15.))THEN
            RVE=((15.-TBAR(I))/(15.-7.))*(TBAR(I)/7.)**((15.-7.)/15.)
         ELSE IF((TBAR(I).LE.(0.)).or.(TBAR(I).GE.15.)) THEN
		        RVE=0.
         ENDIF
         VD=VD+RVE*FLOAT(IDT) ! VERNALIZATION DAYS
			   IF(VD.LT.VR)THEN
			      VF=1-VS*(VR-VD) !Relative vernalization development rate
			   ELSE
            VF=1
         ENDIF
         DAY=I+IPDATE  ! DAY OF YEAR
         IF (DAY.GT.365)THEN
            DAY=I+IPDATE-365 ! DAY OF YEAR
         ENDIF
         DEC=0.4093*SIN(0.0172*(DAY-82.2))
         DLV=(-SIN(0.01745*RLAT)*SIN(DEC)-0.1047)/(COS(0.01745*RLAT)*COS(DEC))
         PHOT=7.639*ACOS(DLV) ! PHOTOPERIOD HOURS
         RPE=1-PS*(20-PHOT)**2 !  PHOTOPERIOD FACTORS
         IF (VD.LT.VR) DTS=RPE*VF !AJC added Feb2007
         DFT=RTE*DTS  !daily flowering time
         AFT=AFT+DFT*FLOAT(IDT) ! accumulative thermal time
         IF(VD.LT.VR)THEN
            DTS=RPE*VF ! daily thermal sensitivity
            TT=TT+DTS*TEFF*FLOAT(IDT)
         ELSE IF(VD.GT.VR.AND.AFT.LE.FTTS)THEN
            DTS=RPE
            TT=TT+DTS*TEFF*FLOAT(IDT)
		     ELSE
            TT=TT+TEFF*FLOAT(IDT)
		     ENDIF

      CASE(1,2,3,4,5)
         TT=TT+TEFF*FLOAT(IDT)
      END SELECT
      RETURN
      END SUBROUTINE TTCALC
!C
!C     TB, TO and TM are base, optimum and maximum cardinal temperatures



