#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014

##########################################################################################
########### write the x-file (AFRB8001.MZX), with a generic experiment name and details
########### to be used for a given location. 
##########################################################################################

#example data
#in_data <- list()
#in_data$general <- list(PEOPLE="generic",ADDRESS="generic",SITE="generic",
#                        PLOT_DETAILS=data.frame(PAREA=-99,PRNO=-99,PLEN=-99,PLDR=-99,PLSP=-99,
#                                         PLAY=-99,HAREA=-99,HRNO=-99,HLEN=-99,HARM=-99))
#in_data$treatments <- data.frame(N=1,R=1,O=1,C=0,TNAME="generic",CU=1,FL=1,SA=0,IC=1,MP=1,
#                                 MI=0,MF=0,MR=0,MC=0,MT=0,ME=0,MH=0,SM=1)
#in_data$cultivars <- data.frame(C=1,CR="MZ",INGENO="GE0001",CNAME="generic")
#in_data$fields <- data.frame(L=1,ID_FIELD="AFRB0001",WSTA="AFRB5001",FLSA=-99,FLOB=-99,FLDT="DR003",
#                             FLDD=-99,FLDS=-99,FLST=-99,SLTX=-99,SLDP=-99,ID_SOIL="IB00000001",
#                             FLNAME="field1",XCRD=35.438,YCRD=-9.562,ELEV=733,AREA=-99,SLEN=-99,
#                             FLWR=-99,SLAS=-99,FLHST=-99,FHDUR=-99)
#in_data$ini_cond_properties <- data.frame(C=1,PCR="MZ",ICDAT="50001",ICRT=-99,ICND=-99,ICRN=-99,ICRE=-99,
#                                          ICWD=-99,ICRES=-99,ICREN=-99,ICREP=-99,ICRIP=-99,ICRID=-99,
#                                          ICNAME="inicond1")
#in_data$ini_cond_profile <- data.frame(C=rep(1,5),ICBL=rep(-99,5),SH2O=rep(-99,5),SNH4=rep(-99,5),
#                                       SNO3=rep(-99,5))
#in_data$planting <- data.frame(P=1,PDATE="50005",EDATE="50013",PPOP=7,PPOE=7,PLME="S",PLDS="R",PLRS=80,PLRD=-99,PLDP=5,
#                               PLWT=-99,PAGE=-99,PENV=-99,PLPH=-99,SPRL=-99,PLNAME="plant1")
#in_data$sim_ctrl <- data.frame(N=1,GENERAL="GE",NYERS=3,NREPS=1,START="S",SDATE="50005",RSEED=2150,
#                               SNAME="simctr1",SMODEL=-99,OPTIONS="OP",WATER="Y",NITRO="N",SYMBI="N",
#                               PHOSP="N",POTAS="N",DISES="N",CHEM="N",TILL="N",CO2="D",METHODS="ME",
#                               WTHER="M",INCON="M",LIGHT="E",EVAPO="R",INFIL="S",PHOTO="R",HYDRO="R",
#                               NSWIT=1,MESOM="G",MESEV="S",MESOL=2,MANAGEMENT="MA",PLANT="A",IRRIG="N",
#                               FERTI="N",RESID="N",HARVS="M",OUTPUTS="OU",FNAME="N",OVVEW="Y",
#                               SUMRY="Y",FROPT=1,GROUT="Y",CAOUT="Y",WAOUT="Y",NIOUT="N",MIOUT="Y",
#                               DIOUT="N",VBOSE="Y",CHOUT="N",OPOUT="Y")
#in_data$auto_mgmt <- data.frame(N=1,PLANTING="PL",PFRST="50005",PLAST="50040",PH2OL=25,PH2OU=100,PH2OD=20,
#                                PSTMX=40,PSTMN=10,IRRIGATION="IR",IMDEP=30,ITHRL=50,ITHRU=100,
#                                IROFF="GS000",IMETH="IR001",IRAMT=10,IREFF=1,NITROGEN="NI",NMDEP=30,
#                                NMTHR=50,NAMNT=25,NCODE="FE001",NAOFF="GS000",RESIDUES="RE",RIPCN=100,
#                                RTIME=1,RIDEP=20,HARVEST="HA",HFRST=0,HLAST=95001,HPCNP=100,HPCNR=0)

#setwd("~/Leeds-work/quest-for-robustness/bin/dssat/csm45_1_23_bin_gfort")
#xfil <- make_xfile(in_data, out_file="./AFRB8001.MZX",overwrite=T)


#main function
make_xfile <- function(in_data, out_file, overwrite=F) {
  #open file in write mode
  if (file.exists(out_file)) {
    if (overwrite) {
      pf <- file(out_file,open="w")
    } else {
      rnum <- round(runif(1,10000,20000),0)
      tmpvar <- unlist(strsplit(out_file,"/",fixed=T))
      pth_ref <- paste(tmpvar[1:(length(tmpvar)-1)],collapse="/")
      out_file <- paste(pth_ref,"/copy-",rnum,"_",tmpvar[length(tmpvar)],sep="")
      pf <- file(out_file,open="w")
    }
  } else {
    pf <- file(out_file,open="w")
  }
  
  #write header and stuff
  #pf <- file(out_file,open="w")
  cat("*EXP.DETAILS: AFRB5001MZ AFRICAN MAIZE RUNS\n",file=pf)
  cat("\n",file=pf)
  
  #general stuff
  cat("*GENERAL\n@PEOPLE\n",file=pf)
  cat(paste(sprintf("%-12s",as.character(in_data$general$PEOPLE)),"\n",sep=""),file=pf)
  cat("@ADDRESS\n",file=pf)
  cat(paste(sprintf("%-12s",as.character(in_data$general$ADDRESS)),"\n",sep=""),file=pf)
  cat("@SITE\n",file=pf)
  cat(paste(sprintf("%-12s",as.character(in_data$general$SITE)),"\n",sep=""),file=pf)
  cat("@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........\n",file=pf)
  cat(paste(" ",sprintf("%1$6d%2$6d%3$6d%4$6d%5$6d%6$6d%7$6d%8$6d%9$6d%10$6d",
                        as.integer(in_data$general$PLOT_DETAILS$PAREA),as.integer(in_data$general$PLOT_DETAILS$PRNO),
                        as.integer(in_data$general$PLOT_DETAILS$PLEN),as.integer(in_data$general$PLOT_DETAILS$PLDR),
                        as.integer(in_data$general$PLOT_DETAILS$PLSP),as.integer(in_data$general$PLOT_DETAILS$PLAY),
                        as.integer(in_data$general$PLOT_DETAILS$HAREA),as.integer(in_data$general$PLOT_DETAILS$HRNO),
                        as.integer(in_data$general$PLOT_DETAILS$HLEN),as.integer(in_data$general$PLOT_DETAILS$HARM)),
            "\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #treatments
  cat("*TREATMENTS                        -------------FACTOR LEVELS------------\n",file=pf)
  cat("@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM\n",file=pf)
  cat(paste(sprintf("%1$2d%2$2d%3$2d%4$2d",as.integer(in_data$treatments$N),as.integer(in_data$treatments$R),
                        as.integer(in_data$treatments$O),as.integer(in_data$treatments$C)),
            " ",sprintf("%1$-25s%2$3d%3$3d%4$3d%5$3d%6$3d%7$3d%8$3d%9$3d%10$3d%11$3d%12$3d%13$3d%14$3d",in_data$treatments$TNAME,
                        as.integer(in_data$treatments$CU),as.integer(in_data$treatments$FL),as.integer(in_data$treatments$SA),
                        as.integer(in_data$treatments$IC),as.integer(in_data$treatments$MP),as.integer(in_data$treatments$MI),
                        as.integer(in_data$treatments$MF),as.integer(in_data$treatments$MR),as.integer(in_data$treatments$MC),
                        as.integer(in_data$treatments$MT),as.integer(in_data$treatments$ME),as.integer(in_data$treatments$MH),
                        as.integer(in_data$treatments$SM)),
            "\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #cultivars
  cat("*CULTIVARS\n",file=pf)
  cat("@C CR INGENO CNAME\n",file=pf)
  for (i in 1:nrow(in_data$cultivars)) {
    cat(paste(sprintf("%2d",as.integer(in_data$cultivars$C[i]))," ",sprintf("%2s", in_data$cultivars$CR[i]),
              " ", sprintf("%6s",in_data$cultivars$INGENO[i])," ",sprintf("%-12s",in_data$cultivars$CNAME[i]),
              "\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #fields
  cat("*FIELDS\n",file=pf)
  cat("@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$fields$L))," ",sprintf("%8s",in_data$fields$ID_FIELD),
            " ",sprintf("%8s",in_data$fields$WSTA),sprintf("%6d",as.integer(in_data$fields$FLSA)),
            sprintf("%6d",as.integer(in_data$fields$FLOB)),sprintf("%6s",in_data$fields$FLDT),
            sprintf("%6d",as.integer(in_data$fields$FLDD)),sprintf("%6s",as.integer(in_data$fields$FLDS)),
            sprintf("%6d",as.integer(in_data$fields$FLST))," ",sprintf("%-4d",as.integer(in_data$fields$SLTX)),
            sprintf("%6d",as.integer(in_data$fields$SLDP)),"  ",sprintf("%-10s",in_data$fields$ID_SOIL)," ",
            sprintf("%-12s",in_data$fields$FLNAME),"\n",sep=""),file=pf)
  cat("@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$fields$L))," ",sprintf("%15.3f",in_data$fields$XCRD)," ",
            sprintf("%15.3f",in_data$fields$YCRD)," ",sprintf("%9d",as.integer(in_data$fields$ELEV))," ",
            sprintf("%17d",as.integer(in_data$fields$AREA))," ",sprintf("%5d",as.integer(in_data$fields$SLEN))," ",
            sprintf("%5d",as.integer(in_data$fields$FLWR))," ",sprintf("%5d",as.integer(in_data$fields$SLAS))," ",
            sprintf("%5d",as.integer(in_data$fields$FLHST))," ",sprintf("%5d",as.integer(in_data$fields$FHDUR)),
            "\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #initial conditions
  cat("*INITIAL CONDITIONS\n",file=pf)
  cat("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5s",in_data$ini_cond_properties$PCR),
            " ",sprintf("%5s",in_data$ini_cond_properties$ICDAT)," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRT)),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICND))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRN)),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRE))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICWD)),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRES))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREN)),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREP))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRIP)),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRID))," ",sprintf("%-12s",in_data$ini_cond_properties$ICNAME),
            "\n",sep=""),file=pf)
  cat("@C  ICBL  SH2O  SNH4  SNO3\n",file=pf)
  for (i in 1:nrow(in_data$ini_cond_profile)) {
    cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$ICBL[i])),
              " ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SH2O[i]))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SNH4[i])),
              " ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SNO3[i])),"\n",sep=""),file=pf)
  }
  cat("\n",file=pf)
  
  #planting details
  cat("*PLANTING DETAILS\n",file=pf)
  cat("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$planting$P))," ",sprintf("%5s",in_data$planting$PDATE),
            " ",sprintf("%5s",in_data$planting$EDATE)," ",sprintf("%5d",as.integer(in_data$planting$PPOP)),
            " ",sprintf("%5d",as.integer(in_data$planting$PPOE))," ",sprintf("%5s",in_data$planting$PLME),
            " ",sprintf("%5s",in_data$planting$PLDS)," ",sprintf("%5d",as.integer(in_data$planting$PLRS)),
            " ",sprintf("%5d",as.integer(in_data$planting$PLRD))," ",sprintf("%5d",as.integer(in_data$planting$PLDP)),
            " ",sprintf("%5d",as.integer(in_data$planting$PLWT))," ",sprintf("%5d",as.integer(in_data$planting$PAGE)),
            " ",sprintf("%5d",as.integer(in_data$planting$PENV))," ",sprintf("%5d",as.integer(in_data$planting$PLPH)),
            " ",sprintf("%5d",as.integer(in_data$planting$SPRL))," ",sprintf("%29s",in_data$planting$PLNAME),
            "\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #simulation controls
  cat("*SIMULATION CONTROLS\n",file=pf)
  cat("@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$GENERAL),
            " ",sprintf("%5d",as.integer(in_data$sim_ctrl$NYERS))," ",sprintf("%5d",as.integer(in_data$sim_ctrl$NREPS)),
            " ",sprintf("%5s",in_data$sim_ctrl$START)," ",sprintf("%5s",in_data$sim_ctrl$SDATE),
            " ",sprintf("%5d",as.integer(in_data$sim_ctrl$RSEED))," ",sprintf("%-25s",in_data$sim_ctrl$SNAME),
            " ",sprintf("%-6s",in_data$sim_ctrl$SMODEL),"\n",sep=""),file=pf)
  cat("@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$OPTIONS),
            " ",sprintf("%5s",in_data$sim_ctrl$WATER)," ",sprintf("%5s",in_data$sim_ctrl$NITRO),
            " ",sprintf("%5s",in_data$sim_ctrl$SYMBI)," ",sprintf("%5s",in_data$sim_ctrl$PHOSP),
            " ",sprintf("%5s",in_data$sim_ctrl$POTAS)," ",sprintf("%5s",in_data$sim_ctrl$DISES),
            " ",sprintf("%5s",in_data$sim_ctrl$CHEM)," ",sprintf("%5s",in_data$sim_ctrl$TILL),
            " ",sprintf("%5s",in_data$sim_ctrl$CO2),"\n",sep=""),file=pf)
  cat("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$METHODS),
            " ",sprintf("%5s",in_data$sim_ctrl$WTHER)," ",sprintf("%5s",in_data$sim_ctrl$INCON),
            " ",sprintf("%5s",in_data$sim_ctrl$LIGHT)," ",sprintf("%5s",in_data$sim_ctrl$EVAPO),
            " ",sprintf("%5s",in_data$sim_ctrl$INFIL)," ",sprintf("%5s",in_data$sim_ctrl$PHOTO),
            " ",sprintf("%5s",in_data$sim_ctrl$HYDRO)," ",sprintf("%5d",as.integer(in_data$sim_ctrl$NSWIT)),
            " ",sprintf("%5s",in_data$sim_ctrl$MESOM)," ",sprintf("%5s",in_data$sim_ctrl$MESEV),
            " ",sprintf("%5d",as.integer(in_data$sim_ctrl$MESOL)),"\n",sep=""),file=pf)
  cat("@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$MANAGEMENT),
            " ",sprintf("%5s",in_data$sim_ctrl$PLANT)," ",sprintf("%5s",in_data$sim_ctrl$IRRIG),
            " ",sprintf("%5s",in_data$sim_ctrl$FERTI)," ",sprintf("%5s",in_data$sim_ctrl$RESID),
            " ",sprintf("%5s",in_data$sim_ctrl$HARVS),"\n",sep=""),file=pf)
  cat("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$OUTPUTS),
            " ",sprintf("%5s",in_data$sim_ctrl$FNAME)," ",sprintf("%5s",in_data$sim_ctrl$OVVEW),
            " ",sprintf("%5s",in_data$sim_ctrl$SUMRY)," ",sprintf("%5s",in_data$sim_ctrl$FROPT),
            " ",sprintf("%5s",in_data$sim_ctrl$GROUT)," ",sprintf("%5s",in_data$sim_ctrl$CAOUT),
            " ",sprintf("%5s",in_data$sim_ctrl$WAOUT)," ",sprintf("%5s",in_data$sim_ctrl$NIOUT),
            " ",sprintf("%5s",in_data$sim_ctrl$MIOUT)," ",sprintf("%5s",in_data$sim_ctrl$DIOUT),
            " ",sprintf("%5s",in_data$sim_ctrl$VBOSE)," ",sprintf("%5s",in_data$sim_ctrl$CHOUT),
            " ",sprintf("%5s",in_data$sim_ctrl$OPOUT),"\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #automatic management
  cat("@  AUTOMATIC MANAGEMENT\n",file=pf)
  cat("@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$PLANTING),
            " ",sprintf("%5s",in_data$auto_mgmt$PFRST)," ",sprintf("%5s",in_data$auto_mgmt$PLAST),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OL))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OU)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OD))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$PSTMX)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PSTMN)),"\n",sep=""),file=pf)
  cat("@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$IRRIGATION),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$IMDEP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$ITHRL)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$ITHRU))," ",sprintf("%5s",in_data$auto_mgmt$IROFF),
            " ",sprintf("%5s",in_data$auto_mgmt$IMETH)," ",sprintf("%5d",as.integer(in_data$auto_mgmt$IRAMT)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$IREFF)),"\n",sep=""),file=pf)
  cat("@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$NITROGEN),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$NMDEP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$NMTHR)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$NAMNT))," ",sprintf("%5s",in_data$auto_mgmt$NCODE),
            " ",sprintf("%5s",in_data$auto_mgmt$NAOFF),"\n",sep=""),file=pf)
  cat("@N RESIDUES    RIPCN RTIME RIDEP\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$RESIDUES),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$RIPCN))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$RTIME)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$RIDEP)),"\n",sep=""),file=pf)
  cat("@N HARVEST     HFRST HLAST HPCNP HPCNR\n",file=pf)
  cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$HARVEST),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$HFRST))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$HLAST)),
            " ",sprintf("%5d",as.integer(in_data$auto_mgmt$HPCNP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$HPCNR)),
            "\n",sep=""),file=pf)
  
  #close file
  close(pf)
  
  #output
  return(out_file)
}





