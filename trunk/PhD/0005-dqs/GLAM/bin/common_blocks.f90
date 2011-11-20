!     Fortran90 common blocks for General Large-Area Model for annual crops (GLAM)
!     Release 2, created from Release 1 during 2007-2008.
!     Copyright The University  of Reading and the University of Leeds 2008. All rights reserved.

!     This module was introduced by Kathryn Nicklin December 2009 for the reading in of netcdf files containing
!     era-interim weather data for West Africa. 
      MODULE AFRICAMOD
       IMPLICIT NONE
       INTEGER, PARAMETER :: NLON_AFRICA = 50, NLAT_AFRICA = 30 !these need to be the correct values for the whole netcdf file
       REAL :: LONGITUDE(NLON_AFRICA), LATITUDE(NLAT_AFRICA)       
       REAL :: VAR_STORE(NLON_AFRICA,NLAT_AFRICA,366)
       REAL*4 :: GPCPRAIN(NLAT_AFRICA,NLON_AFRICA,12,366)!the max number of years of gpcp data is 12. the max number of days in a year is 366.
      END MODULE AFRICAMOD

   MODULE COMMON
     IMPLICIT NONE
     REAL :: YGP !Yield gap parameter, read in at top line of param.txt. See SR OPT
     CHARACTER*3 :: YGP_METH !Yield gap determination method
     INTEGER, PARAMETER :: MAXLAT=21,MAXLON=42,MAXPLANTS=30 !KJN added MAXPLANTS !Inputs. For values > 10: %limit stacksize unlimited
     INTEGER, PARAMETER :: MAXYRS=24,NWTHVAR=5,NMOD=7,MAXTIM=370,MAXENS=10 !Inputs. For netcdf input, all MAX values in this line and above should be one greater than the actual size
     INTEGER :: IINFFIL !Information output file number
     INTEGER, PARAMETER :: NDAYSYR=365 !Maximum length of a single cropping season
     INTEGER, PARAMETER :: NODATA=-99 !must correspond with input NODATA (SR read_spat)
     REAL, PARAMETER :: TESTNODATA=-90.! Used to test for real NODATA vals.
     CHARACTER*1 :: RUNSCALE,OUTSCALE !Model input (b,(d),s), Model output (=opt scale) (b,y)
     REAL, PARAMETER :: RNOVAL=-99. !No value value
     INTEGER :: IVMETH
     REAL :: SLA,SLA_INI
     INTEGER :: NDSLA,ISHF,IUPT
     INTEGER, DIMENSION(MAXLAT,MAXLON,MAXYRS,MAXPLANTS) :: IPSAVE,ISTGSAVE !Saved PDATE and ISTG
     REAL, PARAMETER :: RNCNODATA=2.0E20 !Nodata value for netcdf file
     REAL, PARAMETER :: TESTRNCNODATA=1.0E20 !Used to test for RNCNODATA
   END MODULE COMMON

   MODULE GLOBAL_VARS
!C Global common block for core crop model
   USE COMMON
   IMPLICIT NONE
   REAL, PARAMETER :: PI=3.1415927
   REAL, PARAMETER :: TOLLAI=0.001!Tolerance for flagging decreases in LAI
   REAL, PARAMETER :: TOLSOL=0.001!Tolerance for flagging soil moisture errors
   REAL, PARAMETER :: RLAITEST=0.0001 !Test for LAI==0
   INTEGER, PARAMETER ::MAXNSL=100
   INTEGER :: ISDAY,IEMDAY,ISTG,IDCNT !The latter is the duration count, see SR LAI
   INTEGER :: NSL,IHDAP,I_E,I_C,I_R,I_P
   INTEGER, PARAMETER :: IDT=1,IOUT=1
   REAL, PARAMETER :: RHO_W=997., PSYCHO=62. !Density of water, Psychometric const (SI)
   REAL, PARAMETER   :: RLAT_HT=2.26E6,PT_CONST=1.26 !Lat heat of vap of water, Priestly-Taylor constant
   REAL, PARAMETER ::  A_ESLOPE=611.2,B_ESLOPE=17.67,C_ESLOPE=243.5 ! Ctes for d(e_sat)/dT
   REAL, PARAMETER :: A_SOILHF=0.266, B_SOILHF=0.197, C_SOILHF=6.39 ! Ctes for G calc
   REAL :: GCFLPF,DLDTMX,SWFAC,TTPODF,TTLMAX,S_THICK,DURFAC,DURCTE
   REAL :: GCPLFL,GCLMHA,SWATER,RZSW,TOPODF,TMPODF,TBPODF,TTFLWR,TTHARV,GCPFLM,TOLMAX,TMLMAX,TBLMAX
   REAL :: TOHARV,TBHARV,TMHARV,TBFLWR,TOFLWR,TMFLWR,SWCRIT,RLA,RLV_MEAN
   REAL :: ZSMIN,ZSMAX,DLDLAI,UPDIFC,RLVEF,VOLSW(MAXNSL),VOLSWS,DZ
   REAL :: UPTOL,DTPUPTK,SMC_THRESH,SMCT_FACT,VPD_CTE,EC_CTE,VPDTOT,TBARTOT,TMINTOT,TMAXTOT
   REAL :: TRANS,T_TRANS,P_TRANS,TP_TRANS,CRIT_LAI_T,P_TRANS_MAX,EVAP_COEF
   REAL :: T_RAIN,EFV,TEN_MAX,VPD_REF,VPD,TRADABS,TRADNET
   REAL :: SMLON,TIME_DIFF,RLAT,RLON
   REAL :: T_EVAP,E_TIME,P_EVAP,EVAP,TP_EVAP,C_EVAP
   REAL :: P_UP(2),TP_UP,D1,D2,D3,UPCTE,RKCTE
   REAL :: T_DRAIN,DRAIN,D_RATE,T_RUN,RUNOFF,RKSAT,ALBEDO,SWF_THRESH
   REAL :: RLAI(2),PUPTK(MAXNSL,2),DPUPTK(MAXNSL)
   REAL :: TE,DHDT,BMASS,YIELD,HI,EVAP_E,TRANS_E,EXTC,SHF_CTE,SWFF_THR
   REAL :: TETRS,TENFAC,TETR1,TETR2
   REAL :: PET,ET,PT_COEF,R_THRESH,TRAIN,PART
   REAL :: B_TEN_MAX,B_TE
   INTEGER :: ITEXP(MAXNSL),NED,NRD,IESTG
   REAL :: ZEF(0:NDAYSYR), Z(MAXNSL)
   REAL :: RAIN(NDAYSYR), SRAD(NDAYSYR),TMAX(NDAYSYR),TMIN(NDAYSYR),TBAR(NDAYSYR),TAM(NDAYSYR)
   REAL :: RLL,DUL,SAT, FSWSOW
   INTEGER :: IPDATE,ICO2
   LOGICAL :: LINT, LHIGHT,LSLA !Intelligent planting, high temp stress module and SLA-control (on) flags
   LOGICAL :: LREPLANT !Kathryn Nicklin. TRUE if crop is being replanted and run needs to be reset apart from soil moisture.
   INTEGER :: NDAYSDROUGHT !Kathryn Nicklin. Used in planting routine of Sultan et al 2005. 
   REAL :: RSPARE1,RSPARE2, RSPARE3, RSPARE4, RSPARE5, RSPARE6, RSPARE7, RSPARE8, RSPARE9
   INTEGER :: ISPARE1,ISPARE2, ISPARE3, ISPARE4, ISPARE5, ISPARE6, ISPARE7, ISPARE8, ISPARE9
   INTEGER :: IVALUE
   REAL :: VALUE
   LOGICAL :: DURFLAG !TDS (terminal drought stress)
   REAL :: SWC_FAC,HIMIN !TDS
   !TDS: Note that variable SWCRIT, used for TDS, is declared above
   REAL, ALLOCATABLE :: SAVEMERIT(:,:) !introduced by kathryn nicklin 10May2009 to output the merits of each run
   END MODULE GLOBAL_VARS

   MODULE IO_VARS
   USE COMMON
   IMPLICIT NONE
! The Filenames File
   CHARACTER :: FNAMESFILE*60
!C Global common block for GLAM model: Input and output variables
   REAL, DIMENSION(MAXLAT,MAXLON,MAXYRS,MAXPLANTS) :: RIPSAVE
   INTEGER :: NWTH !Number of days in weather file
   INTEGER, PARAMETER :: IFLWTH=10   !Weather file number
   INTEGER, PARAMETER :: IFLASC=20   !ASCII spatial output file number
   INTEGER, PARAMETER :: IFLSTYP=30  !Soil types file no
   INTEGER, PARAMETER :: IFLGRID=40  !input grid file number
!   INTEGER, PARAMETER :: IFLNCF=50   !NETCDF spatial output file number
   INTEGER, PARAMETER :: IFLPAR=60   !Parameter file number
   INTEGER, PARAMETER :: IFLSOW=70   !Sowing dates file number
   INTEGER, PARAMETER :: IFLDAILY=80 !Daily output file number
   INTEGER, PARAMETER :: IFLMER=90  !Merit file
   INTEGER, PARAMETER :: IFLYLD=120  !Yield data file  
   INTEGER, PARAMETER :: IFLYGP=130   !YGP file
   INTEGER :: INETCDF,IASCII
   CHARACTER :: FLDAILY*20,OUTFIL*20
   CHARACTER :: OUT_HEADER*99
   CHARACTER*100 :: FLWTH(MAXLAT,MAXLON,MAXYRS) !Kathryn Nicklin changed from 50 to 100
   CHARACTER*50 :: WTHROOT, FLSTYP,FLSOL,FLSOW, FLYLD,FLYGP !Kathryn Nicklin changed from length 40 to length 50
   CHARACTER*40, PARAMETER :: OUTDIR='output/'
   CHARACTER :: CROP*14
   CHARACTER*139,PARAMETER :: &
        OUTFMT='(F12.5,A,I3,A,2(F6.2,A),2(F6.0,A),2(F6.2,A),2(F5.1,A), &
        &4(F6.2,A),16(F7.2,A),F6.2,A,F10.5,A,F5.1,A,F10.5,A,F6.1,A,F3.0,&
        &A,F3.0,A,F3.0)'!A,F3.0)'
   CHARACTER*160,PARAMETER :: &
        OUTFMT2='(I4,A,F8.2,A,F8.2,A,I3,A,I3,A,2(F6.2,A),2(F6.0,A),2(F6.2,A),2(F5.1,A), &
        &4(F6.2,A),16(F7.2,A),F6.2,A,F10.5,A,F5.1,A,F10.5,A,F6.1,A,F5.2,&
        &A,F5.2,A,F5.2,A,F5.2)'
!  Extra output format added by Kathryn Nicklin
   CHARACTER*167,PARAMETER :: & 
        OUTFMT3='(I4,A,F8.2,A,F8.2,A,I3,A,I3,A,2(F6.2,A),2(F6.0,A),2(F6.2,A),2(F5.1,A), &
        &4(F6.2,A),16(F7.2,A),F6.2,A,F10.5,A,F5.1,A,F10.5,A,F6.1,A,F5.2,&
        &A,F5.2,A,F5.2,A,F5.2,A,F6.0)'
   CHARACTER*160,PARAMETER :: &
        OUTFMT4='(F12.5,A,I3,A,2(F6.2,A),2(F6.0,A),2(F6.2,A),2(F5.1,A), &
        &4(F6.2,A),16(F7.2,A),F6.2,A,F10.5,A,F5.1,A,F10.5,A,F10.5,A,F10.5,&
        &A,F10.5,A,F10.5,A,F10.5,A,F10.5,A,F10.5)'!A,F3.0)'
   INTEGER :: KMAXNVI,KMAXNVC,KMAXNVR
   CHARACTER*29, PARAMETER :: MOUTFMT='(I3,A,I3,A,F12.5,6(A1,F10.4))' 
   REAL, DIMENSION (MAXLAT,MAXLON) :: SD_OBS,MEANX
   END MODULE IO_VARS

   MODULE MULTI_VARS
!C Global common block for parameter file inputs and model driver variables
   USE COMMON
   IMPLICIT NONE
   INTEGER :: IMP
   INTEGER, PARAMETER ::NR=111, NI=37, NC=2 !Number of real, integer and character inputs in param file
   INTEGER, PARAMETER :: NVARS=37 ! Number of output variables
   INTEGER, PARAMETER :: NPHEN=4 !Number of phenological stages for groundnut
   INTEGER, PARAMETER :: MAXNSOILS=882 !Maximum number of soils
   INTEGER, PARAMETER :: MAXNHYP=50 !Max number of hyp runs
   INTEGER, PARAMETER :: KRLL=6,KDUL=7,KSAT=8,KIPDATE=5
   INTEGER, PARAMETER :: MAXLATLONIN=882
   INTEGER :: MMNO,IMERF !Merit no used to choose optimum params; local (0) or global (1) optimisation
   INTEGER :: KTB(NPHEN),KTO(NPHEN),KTM(NPHEN),KGC(NPHEN) 
   INTEGER :: KISDAY !Address of ISDAY
   CHARACTER :: MODE*3
   LOGICAL :: LPLANT 
   REAL :: RLONMAT(MAXLATLONIN),RLATMAT(MAXLATLONIN),RTIMMAT(MAXTIM),RSURMAT(MAXENS)
   INTEGER :: NRVARS,NIVARS,ISYR,IEYR,NLAT,NLON,ILAT,ILON,IYR,IPLANT !KJN added IPLANT
   INTEGER :: NPLANT !KJN added. The number of plantings each year
   REAL :: SSR_MIN,SSR_MAX
!   REAL, ALLOCATABLE :: WTH(:,:,:,:,:) 
   REAL :: WTH(MAXLAT,MAXLON,MAXYRS,NDAYSYR,NWTHVAR)
   REAL, DIMENSION (MAXLAT,MAXLON) :: YGPMAT, DULMAT, SATMAT, RLLMAT,RLSMASK
   INTEGER, DIMENSION (MAXLAT,MAXLON) :: IPDATEMAT
   DATA KTB(1:NPHEN)/29,33,37,41/
   DATA KTO(1:NPHEN)/30,34,38,42/
   DATA KTM(1:NPHEN)/31,35,39,43/
   DATA KGC(1:NPHEN)/28,32,36,40/
   DATA KISDAY/4/
   INTEGER :: MAXNVI,MAXNVR,MAXNVC,KNRUNS
   INTEGER :: I_INTER,NHYP
   TYPE I_INP
      INTEGER :: VAL,MIN,MAX,NVAL
      CHARACTER :: NAME*11,METH*1
   END TYPE I_INP
   TYPE R_INP
      REAL :: VAL,MIN,MAX
      INTEGER :: NVAL
      CHARACTER :: NAME*11,METH*1
   END TYPE R_INP
   TYPE C_INP
      CHARACTER :: NAME*11,VAL*8,METH*1
      INTEGER :: NVAL,MIN,MAX
   END TYPE C_INP
   TYPE(I_INP) :: II(NI)
   TYPE(R_INP) :: RI(0:NR) !YGP inserted as extra real input param at array subscript 0
   TYPE(C_INP) :: CI(NC)

   TYPE SOIL_CLASSIFICATION
      REAL :: RLL,DUL,SAT,RLL_L,RLL_U,DUL_L,DUL_U,SAT_L,SAT_U,ASW_L,ASW_U
      CHARACTER :: NAME*11,METH*1
   END TYPE SOIL_CLASSIFICATION
   TYPE(SOIL_CLASSIFICATION) :: SOIL(MAXNSOILS)

   REAL :: STORE(MAXYRS,MAXLAT,MAXLON,MAXPLANTS,NVARS) !Stored block output. KJN added MAXPLANTS dimension
   REAL :: BBYLD(MAXYRS,MAXLAT,MAXLON,MAXNHYP,MAXPLANTS) !KJN added MAXPLANTS dimension
   INTEGER :: JSKIP(MAXLAT,MAXLON) 
   INTEGER :: NWTHMAT(MAXLAT,MAXLON,MAXYRS)
   INTEGER, PARAMETER :: NMERIT=6 !This value is also hardwired into moutfmt
   REAL    :: R_INTER,RMERIT(NMERIT),SAVED_RMERIT(NMERIT),RMERIT_MIN(NMERIT)
   LOGICAL :: TBFLAG,TMFLAG,TOFLAG,GCFLAG !Flags true if each cardinal T/all GC's is/are to be varied for *all* phen stages
   LOGICAL :: YGPFLAG !True if YGPBLK is to vary across sets
   LOGICAL :: LNEWSET !True when HYP mode is ready to run the next set of runs
   REAL, DIMENSION (MAXLAT,MAXLON,MAXYRS) :: OBS_YLD
   INTEGER, DIMENSION (MAXLAT,MAXLON,MAXYRS,6) :: YLD_FLAG !Kathryn Nicklin
   INTEGER, DIMENSION (MAXLAT,MAXLON,MAXYRS) :: AREAEQPROD_FLAG, TEMP_ARRAY !Kathryn Nicklin
   INTEGER, DIMENSION (MAXLAT,MAXLON) :: LOW_AVG_AREA !Kathryn Nicklin
   INTEGER, DIMENSION (MAXLAT,MAXLON,MAXYRS,3) :: LOW_AREA !Kathryn Nicklin
   REAL, DIMENSION(4) :: RCOIN !(Real) Coordinates INput to model, on command line
   END MODULE MULTI_VARS


   MODULE HIGHT_VARS
     !High temperature stress parameters for groundnut
     USE GLOBAL_VARS
     INTEGER, PARAMETER :: MAXEP=500 !Max number of high temp episodes
     INTEGER :: IDURMAX !Max duration for after-anthesis effect; must be even
     INTEGER :: IBAMAX  !Max no of days before anthesis for High T effect
     INTEGER :: IAAMAX  !Max no of days after anthesis for High T effect
     REAL :: TCRITMIN,PPCRIT,TLINT,TCSLOPE,TLSLOPE,TLIMMIN,FDWIDTH,FDOFFSET
     INTEGER :: IDAPFLWR
     REAL :: PERCPOD(NDAYSYR),FLWRDIS(NDAYSYR) !No crop takes longer to mature than one year.
     REAL :: DHDT_HT,TOTPP,TOTPP_HIT,TOTPP_WAT
     TYPE HIGHT1
        INTEGER :: IDUR,ITIME
     END TYPE HIGHT1
     TYPE(HIGHT1) :: EPISODE(MAXEP),workep(maxep)
   END MODULE HIGHT_VARS

   MODULE WSPRING_VARS
     !Additional spring wheat variables, also used in winter wheat
     REAL :: GCPFEN, GCENHA
   END MODULE WSPRING_VARS

   MODULE WWINTER_VARS
     !Additional winter wheat variables
     REAL :: GCTSFL,TTTSFL,TTPLTS,GCPLTS,WWSPA1,VN
     REAL :: TBTSFL,TOTSFL,TMTSFL,TBPLTS,TOPLTS,TMPLTS,VS,PS
     REAL :: VD,RVE,VR,VF,EN,DTS,AFT,DTE,RLAIFLT,FTDRFAC,FTTSFAC
     REAL :: WWSPA2, WWSPA3, WWSPA4, WWSPA5, WWSPA6, WWSPA7, WWSPA8, WWSPA9
   END MODULE WWINTER_VARS

   MODULE MAIZE_VARS
     !Additional maize variables
     REAL :: RMAIZE1,RMAIZE2, RMAIZE3, RMAIZE4, RMAIZE5, RMAIZE6, RMAIZE7, RMAIZE8, RMAIZE9
     INTEGER :: IMAIZE1,IMAIZE2, IMAIZE3, IMAIZE4, IMAIZE5, IMAIZE6, IMAIZE7, IMAIZE8, IMAIZE9
   END MODULE MAIZE_VARS

   MODULE RICE_VARS
     !Additional maize variables
     REAL :: RRICE1,RRICE2, RRICE3, RRICE4, RRICE5, RRICE6, RRICE7, RRICE8, RRICE9
     INTEGER :: IRICE1,IRICE2, IRICE3, IRICE4, IRICE5, IRICE6, IRICE7, IRICE8, IRICE9
   END MODULE RICE_VARS
