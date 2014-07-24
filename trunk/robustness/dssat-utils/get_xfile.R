#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#grab xfile details
get_xfile <- function(run_data) {
  basename <- run_data$BASENAME
  years <- run_data$ISYR:run_data$IEYR
  
  #prepare object
  xfil_data <- list()
  xfil_data$general <- list(PEOPLE="generic",ADDRESS="generic",SITE="generic",
                            PLOT_DETAILS=data.frame(PAREA=-99,PRNO=-99,PLEN=-99,PLDR=-99,PLSP=-99,
                                                    PLAY=-99,HAREA=-99,HRNO=-99,HLEN=-99,HARM=-99))
  xfil_data$treatments <- data.frame(N=1,R=1,O=1,C=0,TNAME="generic",CU=1,FL=1,SA=0,IC=1,MP=1,
                                     MI=0,MF=0,MR=0,MC=0,MT=0,ME=0,MH=0,SM=1)
  xfil_data$cultivars <- data.frame(C=1,CR="MZ",INGENO="GE0001",CNAME="generic")
  xfil_data$fields <- data.frame(L=1,
                                 ID_FIELD=paste(basename,"0001",sep=""),
                                 WSTA=paste(basename,substr(as.character(run_data$ISYR),3,4),length(years),sep=""),
                                 FLSA=-99,FLOB=-99,FLDT="DR003",
                                 FLDD=-99,FLDS=-99,FLST=-99,SLTX=-99,SLDP=-99,ID_SOIL="IB00000001",
                                 FLNAME="field1",
                                 XCRD=run_data$LON,
                                 YCRD=run_data$LAT,
                                 ELEV=run_data$ELEV,
                                 AREA=-99,SLEN=-99,
                                 FLWR=-99,SLAS=-99,FLHST=-99,FHDUR=-99)
  xfil_data$ini_cond_properties <- data.frame(C=1,PCR="MZ",
                                              ICDAT=paste(substr(as.character(run_data$ISYR),3,4),run_data$SOW_DATE,sep=""),
                                              ICRT=-99,ICND=-99,ICRN=-99,ICRE=-99,
                                              ICWD=-99,ICRES=-99,ICREN=-99,ICREP=-99,ICRIP=-99,ICRID=-99,
                                              ICNAME="inicond1")
  xfil_data$ini_cond_profile <- data.frame(C=rep(1,5),ICBL=rep(-99,5),SH2O=rep(-99,5),SNH4=rep(-99,5),
                                           SNO3=rep(-99,5))
  xfil_data$planting <- data.frame(P=1,
                                   PDATE=paste(substr(as.character(run_data$ISYR),3,4),run_data$SOW_DATE,sep=""),
                                   EDATE=paste(substr(as.character(run_data$ISYR),3,4),run_data$SOW_DATE+8,sep=""),
                                   PPOP=7,PPOE=7,PLME="S",PLDS="R",PLRS=80,PLRD=-99,PLDP=5,
                                   PLWT=-99,PAGE=-99,PENV=-99,PLPH=-99,SPRL=-99,PLNAME="plant1")
  xfil_data$sim_ctrl <- data.frame(N=1,GENERAL="GE",
                                   NYERS=length(years)-1, #run 1 year less in case of running short of weather years
                                   NREPS=1,START="S",
                                   SDATE=paste(substr(as.character(run_data$ISYR),3,4),run_data$SOW_DATE,sep=""),
                                   RSEED=2150,
                                   SNAME="simctr1",SMODEL=-99,OPTIONS="OP",WATER="Y",NITRO="N",SYMBI="N",
                                   PHOSP="N",POTAS="N",DISES="N",CHEM="N",TILL="N",CO2="D",METHODS="ME",
                                   WTHER="M",INCON="M",LIGHT="E",EVAPO="R",INFIL="S",PHOTO="R",HYDRO="R",
                                   NSWIT=1,MESOM="G",MESEV="S",MESOL=2,MANAGEMENT="MA",PLANT="A",IRRIG="N",
                                   FERTI="N",RESID="N",HARVS="M",OUTPUTS="OU",FNAME="N",OVVEW="Y",
                                   SUMRY="Y",FROPT=1,GROUT="Y",CAOUT="Y",WAOUT="Y",NIOUT="N",MIOUT="Y",
                                   DIOUT="N",VBOSE="0",CHOUT="N",OPOUT="Y")
  xfil_data$auto_mgmt <- data.frame(N=1,PLANTING="PL",
                                    PFRST=paste(substr(as.character(run_data$ISYR),3,4),run_data$SOW_DATE,sep=""),
                                    PLAST=paste(substr(as.character(run_data$ISYR),3,4),run_data$SOW_DATE+run_data$SOW_WINDOW,sep=""),
                                    PH2OL=25,PH2OU=100,PH2OD=20,
                                    PSTMX=40,PSTMN=10,IRRIGATION="IR",IMDEP=30,ITHRL=50,ITHRU=100,
                                    IROFF="GS000",IMETH="IR001",IRAMT=10,IREFF=1,NITROGEN="NI",NMDEP=30,
                                    NMTHR=50,NAMNT=25,NCODE="FE001",NAOFF="GS000",RESIDUES="RE",RIPCN=100,
                                    RTIME=1,RIDEP=20,HARVEST="HA",HFRST=0,HLAST=95001,HPCNP=100,HPCNR=0)
  
  #return object
  return(xfil_data)
}
