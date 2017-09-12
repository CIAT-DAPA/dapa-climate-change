## Lecturas de los dias (bandas)
 
 lecturabandas=function(data){
  data=paste(data)
  lectura=raster(paste(data),band=T)
  dias=sapply(1:lectura@ file@nbands, function(i) raster(paste(data),band=i) )
  return(stack(dias))
  
}

## para extraer las fechas de las grillas se construye la siguiente funcion (extrae las fechas en dias julianos)

extraerfechas=function(data,año){
  tamaño=sapply(1:12,function(i)dim(data[[año]][[i]])[3])
  fecha=unlist(lapply(1:length(tamaño),function(j) sapply(1:tamaño[j],function(i) paste(data[[año]][[j]][[i]]@z))))
  origen<-paste(fecha[1])
  tmp <- as.POSIXlt(paste(fecha), format = "%Y%m%d")
  dias<-julian(as.Date(tmp),origin = as.Date(origen,"%Y%m%d"))+1
  tenvalues<-which(dias<10)
  pos_1=which(dias<10)
  c0=str_sub(fecha,3,4)
  c0=c0[1]
  c1=sapply(1:length(pos_1),function(i) paste(c0,"00",sep="",pos_1[i]))
  pos_2=which(dias>=10 & dias<=100)
  c2=sapply(1:length(pos_2),function(i) paste(c0,"0",sep="",pos_2[i]))
  pos_3=which(dias>100)
  c3=sapply(1:length(pos_3),function(i) paste(c0,"",sep="",pos_3[i]))
  return(c(c1,c2,c3))
  
}

## Extraer Valores de un Pixel por Año

Extraervalores_grilla=function(data,año,grilla){
  tamdias=sapply(1:12,function(i) dim(data[[año]][[i]])[3])
  return(unlist(sapply(1:length(tamdias),function(j) sapply(1:tamdias[j],function(i) as.vector(rasterToPoints(data[[año]][[j]][[i]])[grilla,3])) )))
  
}

## Crea Directorios dada una ruta

Create_dir<-function(path){
  
  dir.create(path,showWarnings=F,recursive=T)
  
  
}

WriteWTH<-function(año,Srad,Tmax,Tmin,Prec,lat,long,wfd){

 yrs<-año
  yrs2<-(yrs*100):((yrs*(100))+365)
  yrs<-(yrs*1000):((yrs*(1000))+365)
  
  
  yrs<-yrs[-1]
  yrs2<-yrs2[-1]

if(wfd=="wfd"){

Prec <- na.omit(as.vector(as.matrix(Prec)))
Prec <- Prec*86400

Srad <- na.omit(as.vector(as.matrix(Srad)))
Srad = Srad/11.5740741 

Tmax <- as.vector(as.matrix(Tmax))
Tmax <- Tmax - 273.15

Tmin <- as.vector(as.matrix(Tmin))
Tmin <- Tmin - 273.15

if(length(Srad) == 366){
yrs[366] <- yrs[365] + 1

}


}

if(wfd=="modelo"){

Prec <- as.vector(as.matrix(Prec))
Srad <- as.vector(as.matrix(Srad))
Tmax <- as.vector(as.matrix(Tmax))
Tmin <- as.vector(as.matrix(Tmin))
if(length(Srad) == 366){
yrs[366] <- yrs[365] + 1

}

}
## pf <- file(paste("JBID",yrs2[1],".WTH",sep=""),open="a",encoding="latin1")
sink(paste("JBID",yrs2[1],".WTH",sep=""), append = T)
##cat(paste("*WEATHER DATA :"),paste(coordenadas[1,1]),paste(coordenadas[1,2]))
cat(paste("*WEATHER DATA :"),paste("BID"))
cat("\n")
cat("\n")
cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
cat("\n")
cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.2f %5.2f %5.2f","JBID", lat, long, -99,-99, -99, 0, 0))
cat("\n")
cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
cat("\n")
cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f",yrs,Srad,Tmax,Tmin,Prec)),sep="\n")
sink()
}


Xfile <- function(inicial,final,nombre,sistema,año,Nit1,Nit2,Nit3,DiasNit){

aplicacion1 <- Nit1
aplicacion2 <- Nit2
aplicacion3 <- Nit3
aplicacion <- c(aplicacion1,aplicacion2,aplicacion3)
DiasNitrogeno <-DiasNit
cat(inicial,"\n")
cat(final,"\n")
if (inicial > final) {  #if start day > end day, set end day to next year
final = final + 365
}
fecha <- round(mean(c(inicial,final)))  #average start & end days
if (fecha > 365) {  #if average in next year, reset to previous year
fecha = fecha - 365
}
cat("Corrio","\n")
if(sistema=="Riego"){
## Riego	Tambien habia error con la fecha de inicio de simulacion
		
		sdate <- pmax((fecha-15),1)
		if(fecha<100){
		
			if(fecha<10){
			
			planting <- as.numeric(paste(año,"00",fecha,sep=""))
			
			}
			else{
			planting <- as.numeric(paste(año,0,fecha,sep=""))
			}
			
			
		
						if(sdate<100){
						
								if(sdate<10){
							sdate1 <- pmax(as.numeric(paste(año,"00",sdate,sep="")),0)
										}
										else{
										sdate1 <- pmax(as.numeric(paste(año,0,sdate,sep="")),0)
										}
									}
		if(sdate>=100){
		
		sdate1 <- pmax(as.numeric(paste(año,sdate,sep="")),0)
		
		}
		
		}
		
		if(fecha>=100){
		planting <- pmax(as.numeric(paste(año,fecha,sep="")),0)
		if(sdate<100){
		
		sdate1 <- pmax(as.numeric(paste(año,0,sdate,sep="")),0)
		
		}
		if(sdate>=100){
		
		sdate1 <- pmax(as.numeric(paste(año,sdate,sep="")),0)
		
		}
		}
		
		A <- "PL"	
		inicial <- -99
		final <- -99
		#A <- "A"
	   IRR <- "A"
	   plant<-"R"

}
	   

if(sistema=="Secano"){

    #inicial <-inicial
	#final <- final
	# inicial = pmax((fecha - 7),0)  #2-week sowing window in the middle of MIRCA window)
	# final = pmax((fecha + 7),0)
	inicial = fecha - 21  #calcular ventana de siembra
	final = fecha + 21
	
	if (fecha>=0 & fecha <=36)  #principio del año
	{
		inicial = 15  #se necesitan 15 días al principio para iniciar la simulación
		final = 57
	}

	if (fecha>344 & fecha <=365)  #final del año
	{
		inicial = 323
		final = 365
	}

sdate<-pmax((inicial-15),0)
	
	if(sdate<10){
	sdate1 <- pmax(as.numeric(paste(año,"00",sdate,sep=""),0))
	
	}
	if(sdate>=10 & sdate<100){
	sdate1 <- pmax(as.numeric(paste(año,"0",sdate,sep=""),0))
	
	}
	
	if(sdate>=100){
	sdate1 <- pmax(as.numeric(paste(año,sdate,sep=""),0)) ## Este era el error
	}


	# if (final > 365) {  #if at end of year, hard-code to last 2 weeks to avoid problems in DSSAT (but shouldn't find since use mid-month from MIRCA)
		# inicial = 351
		# final = 365
	# }
	planting <- -99
	A <- "A"
	IRR <- "N"
   plant<-"A"
}


in_data <- list()
## Definicion del Experimento

## Datos Generales del Experimento
in_data$general <- list(PEOPLE="Diego Obando Jeison Mesa Patricia Moreno",ADDRESS="CIAT",SITE="CALI")
                        
in_data$treatments <- data.frame(N=1,R=1,O=1,C=0,TNAME="BID001",CU=1,FL=1,SA=0,IC=0,MP=1,
                                 MI=0,MF=1,MR=0,MC=0,MT=0,ME=0,MH=0,SM=1)

in_data$cultivars <- data.frame(C=1,CR="MZ",INGENO="IB0006",CNAME="IRNA")

in_data$fields <- data.frame(L=1,ID_FIELD="BID1",WSTA="JBID",FLSA=-99,FLOB=-99,FLDT="DR000",
                             FLDD=-99,FLDS=-99,FLST=-99,SLTX=-99,SLDP=-99,ID_SOIL="BID0000001",
                             FLNAME="FIELD01",XCRD=-99,YCRD=-99,ELEV=-99,AREA=-99,SLEN=-99,
                             FLWR=-99,SLAS=-99,FLHST=-99,FHDUR=-99)

in_data$ini_cond_properties <- data.frame(C=1,PCR="MZ",ICDAT="50001",ICRT=-99,ICND=-99,ICRN=-99,ICRE=-99,
                                          ICWD=-99,ICRES=-99,ICREN=-99,ICREP=-99,ICRIP=-99,ICRID=-99,
                                          ICNAME="inicond1")

#in_data$ini_cond_profile <- data.frame(C=rep(1,5),ICBL=rep(-99,5),SH2O=rep(-99,5),SNH4=rep(-99,5),
#                                       SNO3=rep(-99,5))


in_data$planting <- data.frame(P=1,PDATE=planting,EDATE=-99,PPOP=5,PPOE=5,PLME="S",PLDS="R",PLRS=80,PLRD=0,PLDP=5,
                               PLWT=-99,PAGE=-99,PENV=-99,PLPH=-99,SPRL=-99)


in_data$sim_ctrl <- data.frame(N=1,GENERAL="GE",NYERS=27,NREPS=1,START="S",SDATE=sdate1,RSEED=2150,
                               SNAME="simctr1",SMODEL="MZIXM",OPTIONS="OP",WATER="Y",NITRO="Y",SYMBI="N",
                               PHOSP="N",POTAS="N",DISES="N",CHEM="N",TILL="N",CO2="D",METHODS="ME",
                               WTHER="M",INCON="M",LIGHT="E",EVAPO="R",INFIL="S",PHOTO="C",HYDRO="R",
                               NSWIT=1,MESOM="G",MESEV="S",MESOL=2,MANAGEMENT="MA",PLANT=plant,IRRIG=IRR,
                               FERTI="D",RESID="R",HARVS="M",OUTPUTS="OU",FNAME="N",OVVEW="Y",
                               SUMRY="Y",FROPT=1,GROUT="Y",CAOUT="Y",WAOUT="N",NIOUT="N",MIOUT="N",
                               DIOUT="N",VBOSE="N",CHOUT="N",OPOUT="N")

in_data$auto_mgmt <- data.frame(N=1,PLANTING=A,PFRST=inicial,PLAST=final,PH2OL=50,PH2OU=100,PH2OD=30,
                                PSTMX=40,PSTMN=10,IRRIGATION="IR",IMDEP=30,ITHRL=50,ITHRU=100,
                                IROFF="GS000",IMETH="IR001",IRAMT=10,IREFF=1,NITROGEN="NI",NMDEP=30,
                                NMTHR=50,NAMNT=25,NCODE="FE001",NAOFF="GS000",RESIDUES="RE",RIPCN=100,
                                RTIME=1,RIDEP=20,HARVEST="HA",HFRST=0,HLAST=00001,HPCNP=100,HPCNR=0)




#Funcion para escribir archivos experimentales
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
  cat("*EXP.DETAILS: BID17101MZ MAIZE LAC\n",file=pf)
  cat("\n",file=pf)
  
  #general stuff
  cat("*GENERAL\n@PEOPLE\n",file=pf)
  cat(paste(sprintf("%-12s",as.character(in_data$general$PEOPLE)),"\n",sep=""),file=pf)
  cat("@ADDRESS\n",file=pf)
  cat(paste(sprintf("%-12s",as.character(in_data$general$ADDRESS)),"\n",sep=""),file=pf)
  cat("@SITE\n",file=pf)
  cat(paste(sprintf("%-12s",as.character(in_data$general$SITE)),"\n",sep=""),file=pf)

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
  cat(paste(sprintf("%2d",as.integer(in_data$fields$L))," ",sprintf("%-8s",in_data$fields$ID_FIELD),
            " ",sprintf("%-8s",in_data$fields$WSTA),sprintf("%6d",as.integer(in_data$fields$FLSA)),
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
  #cat("*INITIAL CONDITIONS\n",file=pf)
  #cat("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n",file=pf)
  #cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5s",in_data$ini_cond_properties$PCR),
  #          " ",sprintf("%5s",in_data$ini_cond_properties$ICDAT)," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRT)),
  #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICND))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRN)),
  #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRE))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICWD)),
  #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRES))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREN)),
  #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREP))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRIP)),
  #          " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRID))," ",sprintf("%-12s",in_data$ini_cond_properties$ICNAME),
  #          "\n",sep=""),file=pf)
  #cat("@C  ICBL  SH2O  SNH4  SNO3\n",file=pf)
  #for (i in 1:nrow(in_data$ini_cond_profile)) {
  #  cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$ICBL[i])),
  #            " ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SH2O[i]))," ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SNH4[i])),
  #            " ",sprintf("%5.0f",as.integer(in_data$ini_cond_profile$SNO3[i])),"\n",sep=""),file=pf)
  #}
  #cat("\n",file=pf)
  
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

  ## Detalles de Fertilizacion
  cat("*FERTILIZERS (INORGANIC)\n",file=pf)
  cat("@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME                       \n",file=pf )
 for(i in 1:3){
  if(!is.na(aplicacion)[i]){
  cat(sprintf("%2s %5s %4s %5s %5i %5.1f %5i %5i %5i %5i %5i %1i",1,1,"FE005","AP002",DiasNitrogeno[i],aplicacion[i],0,-99,-99,-99,-99,-99),"\n",file=pf)
 }
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
make_xfile(in_data, out_file=nombre,overwrite=T)
}

read.summary <- function(...) {

pat <- "SDAT|PDAT|ADAT|MDAT|HWAH|HIAM|EPCM|PRCP|ETCP"   

imp.head <- scan("Summary.OUT", what = "character", skip = 3, nlines = 1, quiet = T)
  headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
  
  # Read in main table in fixed width format
  seps <- c(-93, 7, 8, -8, 8, 8, -33, 5, -36, 6, -31, 5, -316, 6, -1, 6)
  imp.dat <- read.fwf("Summary.OUT", width = seps, skip = 4, header = F, sep = "")
  colnames(imp.dat) <- headers
  return(imp.dat)

}

Crop_Run<-function(año,pixel,clima,crop,pixeldataframe,sistema,latitude,longitude,nit1,nit2,nit3,diasFertilizacion,wfd){
gc()
outList <- list.files(pattern=".OUT")
wthList<- list.files(pattern=".WTH")
soilList<- list.files(pattern=".SOIL")
file.remove(outList)
file.remove(wthList)
file.remove(soilList)

Xfile(crop[pixeldataframe,8], crop[pixeldataframe,9],"./JBID.MZX",sistema,año[1],
nit1,nit2,nit3,diasFertilizacion)
#crop[pixeldataframe,"N.app.0d"],crop[pixeldataframe,"N.app.30d"],crop[pixeldataframe,"N.app.40d"],c(0,30,40))

##Xfile(Maize.loc.cal[ind_clim1[3],"mirca.irr.start"],Maize.loc.cal[ind_clim1[3],"mirca.irr.end"],"./JBID.MZX")
##Maize.loc.cal[ind_clim1[3],"mirca.irr.start"]
##Maize.loc.cal[ind_clim1[3],"mirca.irr.end"]

if(wfd=="wfd"){
sapply(1:length(año),function(i) WriteWTH(año[i],clima$Radiacion[[i]][pixel,],clima$Tmax[[i]][pixel,],clima$Tmin[[i]][pixel,],clima$Prec[[i]][pixel,],latitude,longitude,"wfd"))
gc() 
}

if(wfd=="modelo"){

sapply(1:length(año),function(i) WriteWTH(año[i],clima$Srad[[pixel]][[i]],clima$Tmax[[pixel]][[i]],clima$Tmin[[pixel]][[i]],clima$Prec[[pixel]][[i]],latitude,longitude,"modelo"))
gc()

}
Extraer.SoilDSSAT(valores[pixeldataframe],getwd())

##sfSapply(1:length(año),function(i) WriteWTH(año[1],clima$Radiacion[[1]][[1]],clima$Tmax[[1]][1,],clima$Tmin[[1]][1,],clima$Prec[[1]][[1]],Maize.loc.cal[1,"Longitude"],Maize.loc.cal[1,"Latitude"]))
cat(getwd(),"\n")
cat(pixeldataframe,"\n")

system("./DSCSM045.EXE MZCER045 B DSSBatch.v45",ignore.stdout=T)
cat("Corrio","\n")
 pat <- "SDAT|PDAT|ADAT|MDAT|IRCM|HWAH|HIAM|EPCM|NICM|NDCH|PRCP|ETCP|CWAM"
imp.head <- scan("Summary.OUT", what = "character", skip = 3, nlines = 1, quiet = T)
headers <- imp.head[grep(pat, imp.head, perl = TRUE)]


seps <- c(-92, 8, 8, -8, 8, 8, -14, 8, -8, 8, -36, 6, -12, 6, -12, 6, -30, 6, -242, 6, -31, 7, 7)

imp.dat <- read.fwf("Summary.OUT", width = seps, skip = 4, header = F, sep = "")
colnames(imp.dat) <- headers
return(imp.dat) 
  gc()
}


