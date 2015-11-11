#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#grab xfile details
get_xfile <- function(years, basename="P4NE", xy_loc) {
  #xy_loc <- out_seas[1,]
  #years <- 2011
  #basename <- "P4NE"
  
  #prepare object
  xfil_data <- list()
  xfil_data$general <- list(PEOPLE="-99",ADDRESS="-99",SITE="LSMS sites, Niger",
                            PLOT_DETAILS=data.frame(PAREA=-99,PRNO=-99,PLEN=-99,PLDR=-99,PLSP=-99,
                                                    PLAY=-99,HAREA=-99,HRNO=-99,HLEN=-99,HARM=-99))
  xfil_data$treatments <- data.frame(N=c(1:10),R=rep(1,10),O=rep(1,10),C=rep(0,10),
                                     TNAME=c("short, residue", "short, manure", "mid1, residue",
                                             "mid1, manure", "mid2, residue", "mid2, manure",
                                             "mid3, residue", "mid3, manure", "long, residue",
                                             "long, manure"),
                                     CU=c(1,1,2,2,3,3,4,4,5,5),FL=rep(1,10),SA=rep(0,10),
                                     IC=rep(1,10),MP=rep(1,10),MI=rep(0,10),MF=rep(0,10),
                                     MR=rep(1:2,5),MC=rep(0,10),MT=rep(0,10),ME=rep(0,10),
                                     MH=rep(0,10),SM=rep(1,10))
  xfil_data$cultivars <- data.frame(C=c(1:5),CR=rep("ML",5),INGENO=paste("P4000",1:5,sep=""),
                                    CNAME=c("Sadore-short","Sadore-mid1","Sadore-mid2",
                                            "Sadore-mid3","Sadore-long"))
  xfil_data$fields <- data.frame(L=1,
                                 ID_FIELD=paste(basename,"0001",sep=""),
                                 WSTA=paste(basename,substr(as.character(min(years)),3,4),sprintf("%02d",length(years)),sep=""),
                                 FLSA=-99,FLOB=-99,FLDT="DR000",
                                 FLDD=-99,FLDS=-99,FLST=100,SLTX=-99,SLDP=-99,
                                 ID_SOIL=paste(basename,"000001",sep=""),
                                 FLNAME="Niger",
                                 XCRD=xy_loc$lon[1],
                                 YCRD=xy_loc$lat[1],
                                 ELEV=-99,
                                 AREA=-99,SLEN=-99,
                                 FLWR=-99,SLAS=-99,FLHST=-99,FHDUR=-99)
  xfil_data$ini_cond_properties <- data.frame(C=1,PCR="ML",
                                              ICDAT=paste(substr(as.character(min(years)),3,4),sprintf("%03d",(xy_loc$SDATE-1)),sep=""),
                                              ICRT=100,ICND=0,ICRN=1,ICRE=1,
                                              ICWD=-99,ICRES=100,ICREN=1.18,ICREP=0,ICRIP=100,ICRID=5,
                                              ICNAME="IC, Niger")
  xfil_data$ini_cond_profile <- data.frame(C=rep(1,6),ICBL=c(5,15,30,60,100,200),
                                           SH2O=runif(6,0.02,0.03),
                                           SNH4=rep(0,6),
                                           SNO3=runif(6,0.002,0.01))
  xfil_data$planting <- data.frame(P=1,
                                   PDATE=paste(substr(as.character(min(years)),3,4),sprintf("%03d",xy_loc$SDATE),sep=""),
                                   EDATE=-99,
                                   PPOP=15,PPOE=15,PLME="S",PLDS="H",PLRS=80,PLRD=0,PLDP=5,
                                   PLWT=-99,PAGE=-99,PENV=-99,PLPH=-99,SPRL=-99,PLNAME="Planting, Niger")
  xfil_data$residues <- data.frame(R=c(1,2),RDATE=rep(paste(substr(as.character(min(years)),3,4),sprintf("%03d",(xy_loc$SDATE-1)),sep=""),2),
                                   RCOD=c("RE202","RE002"),RAMT=c(1500,1500),RESN=c(1.20,2.50),
                                   RESP=rep(0,2),RESK=rep(0,2),RINP=rep(100,2),RDEP=rep(15,2),
                                   RMET=rep("AP002",2),RENAME=c("Residue, Niger","Manure, Niger"))
  
  
  xfil_data$sim_ctrl <- data.frame(N=1,GENERAL="GE",
                                   NYERS=length(years),
                                   NREPS=1,START="S",
                                   SDATE=paste(substr(as.character(min(years)),3,4),sprintf("%03d",(xy_loc$SDATE-1)),sep=""),
                                   RSEED=2150,
                                   SNAME="Sim. Contr., Niger",
                                   SMODEL=-99,OPTIONS="OP",WATER="Y",NITRO="Y",SYMBI="Y",
                                   PHOSP="N",POTAS="N",DISES="N",CHEM="N",TILL="N",CO2="D",METHODS="ME",
                                   WTHER="M",INCON="M",LIGHT="E",EVAPO="R",INFIL="S",PHOTO="R",HYDRO="R",
                                   NSWIT=1,MESOM="G",MESEV="S",MESOL=2,MANAGEMENT="MA",PLANT="R",IRRIG="N",
                                   FERTI="R",RESID="R",HARVS="M",OUTPUTS="OU",FNAME="N",OVVEW="Y",
                                   SUMRY="Y",FROPT=1,GROUT="Y",CAOUT="Y",WAOUT="Y",NIOUT="Y",MIOUT="Y",
                                   DIOUT="N",VBOSE="Y",CHOUT="N",OPOUT="Y")
  xfil_data$auto_mgmt <- data.frame(N=1,PLANTING="PL",
                                    PFRST=paste(substr(as.character(min(years)),3,4),sprintf("%03d",(xy_loc$SDATE-10)),sep=""),
                                    PLAST=paste(substr(as.character(min(years)),3,4),sprintf("%03d",(xy_loc$SDATE+10)),sep=""),
                                    PH2OL=40,PH2OU=100,PH2OD=30,
                                    PSTMX=40,PSTMN=10,IRRIGATION="IR",IMDEP=30,ITHRL=50,ITHRU=100,
                                    IROFF="GS000",IMETH="IR001",IRAMT=10,IREFF=1,NITROGEN="NI",NMDEP=30,
                                    NMTHR=50,NAMNT=25,NCODE="FE001",NAOFF="GS000",RESIDUES="RE",RIPCN=100,
                                    RTIME=1,RIDEP=20,HARVEST="HA",HFRST=-99,HLAST=-99,HPCNP=-99,HPCNR=-99)

  #return object
  return(xfil_data)
}

