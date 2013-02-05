#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"


src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a102/eejarv/CMIP5"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#data directories
c3Dir <- paste(mdDir,"/assessment/output-data-cmip3/_summary",sep="") #CMIP3 dir
c5Dir <- paste(mdDir,"/assessment/output-data/_summary_revised2",sep="") #CMIP5 dir
figDir <- paste(c5Dir,"/taylor_diag",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}


#load CMIP3
load(file=paste(c3Dir,"/cl-summary_final.RData",sep=""))
c3_mets <- all_mets
rm(all_mets); g=gc(); rm(g)
c3_mets$PRMSE1 <- abs(c3_mets$PRMSE1);
c3_mets$PRMSE2 <- abs(c3_mets$PRMSE2)

#load CMIP5
load(file=paste(c5Dir,"/cl-summary_final.RData",sep=""))
c5_mets <- all_mets
rm(all_mets); g=gc(); rm(g)
c5_mets$PRMSE1 <- abs(c5_mets$PRMSE1)
c5_mets$PRMSE2 <- abs(c5_mets$PRMSE2)

#select ISO, variable and dataset of interest
diag <- c3_mets[which(c3_mets$OBS == "cl-CRU"),]
diag <- diag[which(diag$ISO == "IND"),]
diag <- diag[which(diag$SEASON != "ANN"),]

diag2 <- c5_mets[which(c5_mets$OBS == "cl_rev2-CRU"),]
diag2 <- diag2[which(diag2$ISO == "IND"),]
diag2 <- diag2[which(diag2$SEASON != "ANN"),]

for (vn in c("pr","tas","dtr","rd")) {
  this_diag <- diag
  this_diag2 <- diag2
  
  if (vn == "pr") this_diag <- this_diag[which(this_diag$VAR == "prec"),]
  if (vn == "tas") this_diag <- this_diag[which(this_diag$VAR == "tmean"),]
  if (vn == "dtr" | vn == "rd") this_diag <- this_diag[which(this_diag$VAR == vn),]
  this_diag2 <- this_diag2[which(this_diag2$VAR == vn),]
  
  if (vn == "pr") this_maxsd <- 6
  if (vn == "tas") this_maxsd <- 2
  if (vn == "dtr") this_maxsd <- 4
  if (vn == "rd") this_maxsd <- 8
  
  ##### construct Taylor diagram
  
  tiff(paste(figDir,"/",vn,"_IND_cl-CRU.tif",sep=""),res=300,compression="lzw",
       pointsize=12,height=2048,width=2048)
  #### CMIP3
  if (nrow(this_diag) > 0) {
    for (i in 1:nrow(this_diag)) {
      R <- this_diag$CCOEF2[i]
      sd.r <- this_diag$std_OBS[i]
      sd.f <- this_diag$std_GCM[i]
      seas <- paste(this_diag$SEASON[i])
      gcm <- paste(this_diag$GCM[i])
      if (seas == "DJF") this_col <- "black"
      if (seas == "MAM") this_col <- "blue"
      if (seas == "JJA") this_col <- "red"
      if (seas == "SON") this_col <- "green"
      if (gcm == "multi_model_mean") {this_pch <- 20} else {this_pch <- 21}
      if (gcm == "multi_model_mean") {this_pcex <- 1.25} else {this_pcex <- 0.75}
      
      if (i == 1) {
        taylorD(R, sd.r, sd.f, add = F, col = this_col, pch = this_pch, pos.cor = T, main = NA,
                ref.sd = T, grad.corr.lines = c(seq(0,1,by=0.1),0.95,0.99), pcex = this_pcex, 
                cex.axis = 1, normalize = T, mar = c(5, 4, 5, 6),maxsd=this_maxsd,
                ylab="Normalised standard deviation")
      } else {
        taylorD(R, sd.r, sd.f, add = T, col = this_col, pch = this_pch, pos.cor = T, 
                main = NA, ref.sd = T, pcex = this_pcex, cex.axis = 1, normalize = T, 
                mar = c(5, 4, 6, 6))
      }
    }
  }
  
  #### CMIP5
  for (i in 1:nrow(this_diag2)) {
    R <- this_diag2$CCOEF2[i]
    sd.r <- this_diag2$std_OBS[i]
    sd.f <- this_diag2$std_GCM[i]
    seas <- paste(this_diag2$SEASON[i])
    gcm <- paste(this_diag2$GCM[i])
    if (seas == "DJF") this_col <- "black"
    if (seas == "MAM") this_col <- "blue"
    if (seas == "JJA") this_col <- "red"
    if (seas == "SON") this_col <- "green"
    if (gcm == "multi_model_mean_ENS_r1i1p1") {this_pch <- 17} else {this_pch <- 2}
    if (gcm == "multi_model_mean_ENS_r1i1p1") {this_pcex <- 1.25} else {this_pcex <- 0.75}
    
    if (nrow(this_diag) > 0) {
      taylorD(R, sd.r, sd.f, add = T, col = this_col, pch = this_pch, pos.cor = T, 
              main = NA, ref.sd = T, pcex = this_pcex, cex.axis = 1, normalize = T, 
              mar = c(5, 4, 6, 6))
    } else {
      if (i == 1) {
        taylorD(R, sd.r, sd.f, add = F, col = this_col, pch = this_pch, pos.cor = T, 
                main = NA, ref.sd = T, pcex = this_pcex, 
                grad.corr.lines = c(seq(0,1,by=0.2),0.9, 0.95, 0.99),
                cex.axis = 1, normalize = T, mar = c(5, 4, 5, 6),maxsd=this_maxsd,
                ylab="Normalised standard deviation")
      } else {
        taylorD(R, sd.r, sd.f, add = T, col = this_col, pch = this_pch, pos.cor = T, 
                main = NA, ref.sd = T, pcex = this_pcex, cex.axis = 1, normalize = T, 
                mar = c(5, 4, 6, 6))
      }
    }
  }
  dev.off()
}



