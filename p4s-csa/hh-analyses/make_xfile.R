#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014, modified for millet in Niger Oct 2015

##########################################################################################
########### write the x-file (P4NE1101.MLX), with a generic experiment name and details
########### to be used for a given location. 
##########################################################################################

#main function
make_xfile <- function(in_data, out_file, overwrite=F) {
  #in_data <- loc_xfil
  #out_file <- paste(mdata_outdir,"/PANE1101.MLX",sep="")
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
  cat("*EXP.DETAILS: P4NE1101ML MILLET, NIGER LSMS SITES (P4S-CSA)\n",file=pf)
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
  for (i in 1:nrow(in_data$treatments)) {
    cat(paste(sprintf("%1$2d%2$2d%3$2d%4$2d",as.integer(in_data$treatments$N[i]),as.integer(in_data$treatments$R[i]),
                          as.integer(in_data$treatments$O[i]),as.integer(in_data$treatments$C[i])),
              " ",sprintf("%1$-25s%2$3d%3$3d%4$3d%5$3d%6$3d%7$3d%8$3d%9$3d%10$3d%11$3d%12$3d%13$3d%14$3d",in_data$treatments$TNAME[i],
                          as.integer(in_data$treatments$CU[i]),as.integer(in_data$treatments$FL[i]),as.integer(in_data$treatments$SA[i]),
                          as.integer(in_data$treatments$IC[i]),as.integer(in_data$treatments$MP[i]),as.integer(in_data$treatments$MI[i]),
                          as.integer(in_data$treatments$MF[i]),as.integer(in_data$treatments$MR[i]),as.integer(in_data$treatments$MC[i]),
                          as.integer(in_data$treatments$MT[i]),as.integer(in_data$treatments$ME[i]),as.integer(in_data$treatments$MH[i]),
                          as.integer(in_data$treatments$SM[i])),
              "\n",sep=""),file=pf)
  }
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
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRES))," ",sprintf("%5.2f",in_data$ini_cond_properties$ICREN),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREP))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRIP)),
            " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRID))," ",sprintf("%-12s",in_data$ini_cond_properties$ICNAME),
            "\n",sep=""),file=pf)
  cat("@C  ICBL  SH2O  SNH4  SNO3\n",file=pf)
  for (i in 1:nrow(in_data$ini_cond_profile)) {
    cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$ICBL[i])),
              " ",sprintf("%5.3f",in_data$ini_cond_profile$SH2O[i])," ",sprintf("%5.3f",in_data$ini_cond_profile$SNH4[i]),
              " ",sprintf("%5.3f",in_data$ini_cond_profile$SNO3[i]),"\n",sep=""),file=pf)
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
            " ",sprintf("%5d",as.integer(in_data$planting$SPRL)),paste(rep(" ",24),collapse=""),sprintf("%-10s",in_data$planting$PLNAME),
            "\n",sep=""),file=pf)
  cat("\n",file=pf)
  
  #residues and organic fertilizer
  cat("*RESIDUES AND ORGANIC FERTILIZER\n",file=pf)
  cat("@R RDATE  RCOD  RAMT  RESN  RESP  RESK  RINP  RDEP  RMET RENAME\n",file=pf)
  for (i in 1:nrow(in_data$residues)) {
    cat(paste(sprintf("%2d",as.integer(in_data$residues$R[i]))," ",sprintf("%5s",in_data$residues$RDATE[i]),
              " ",sprintf("%5s",in_data$residues$RCOD[i])," ",sprintf("%5d",as.integer(in_data$residues$RAMT[i])),
              " ",sprintf("%5.2f",in_data$residues$RESN[i])," ",sprintf("%5.0f",in_data$residues$RESP[i]),
              " ",sprintf("%5.0f",in_data$residues$RESK[i])," ",sprintf("%5d",as.integer(in_data$residues$RINP[i])),
              " ",sprintf("%5d",as.integer(in_data$residues$RDEP[i]))," ",sprintf("%5s",in_data$residues$RMET[i]),
              " ",sprintf("%-20s",in_data$residues$RENAME[i]),
              "\n",sep=""),file=pf)
  }
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





